#!/usr/bin/env python3
"""Generate and run fixed-load tenant-tree sweeps using the multi-edit compiler."""
from __future__ import annotations

import argparse
import copy
import csv
import hashlib
import json
import shutil
import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path

from pifo_figures.evaluation import commit_rows
from pifo_multiedit_compiler import compile_request
from pifo_traffic_program import generate_traffic, load_traffic_program
from pifo_transaction_program import write_transaction_program
from request_trace import write_trace

ROOT = Path(__file__).resolve().parents[2]
RESOURCES = ROOT / "experiments" / "scalability"
RESULTS = ROOT / "experiment-results" / "scalability"


def write_json(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2) + "\n")


def read_csv(path):
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def write_csv(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as destination:
        writer = csv.DictWriter(destination, fieldnames=list(rows[0]) if rows else ["run"],
                                lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def tenant(index, policy):
    flows = [{"id": 2 * index - 1, "name": f"Tenant_{index:02}_A"},
             {"id": 2 * index, "name": f"Tenant_{index:02}_B"}]
    if policy == "WFQ":
        flows[0]["weight"], flows[1]["weight"] = 1, 2
    return {"name": f"tenant_{index:02}", "slot": index, "weight": 1, "policy": policy,
            "flows": flows}


def prepare(cfg, request, m):
    before = [tenant(i, ("SP", "RR", "WFQ")[(i - 1) % 3]) for i in range(1, m + 1)]
    after = copy.deepcopy(before)
    if request == "add":
        # The identical new SP subtree is added at the first free slot.
        after.append(tenant(m + 1, "SP"))
    else:
        after[0]["weight"] = 2
    reset = json.loads((ROOT / "experiments/multi-edit/request.json").read_text())["reset_cost"]
    spec = {"schema": "pifo-multi-edit-v1", "cycle": cfg["t1"], "hardware": cfg["hardware"],
            "before": before, "after": after, "reset_cost": reset}
    folder = RESOURCES / request / f"m-{m}"
    write_json(folder / "request.json", spec)
    patterns = []
    packet_interval = cfg["packet_size_bytes"] / cfg["link_bytes_per_cycle"] / cfg["offered_load"]
    for phase, begin, end, tree in (("before", 0, cfg["t1"], before),
                                   ("after", cfg["t1"], cfg["end_cycle"], after)):
        flows = [f for t in tree for f in t["flows"]]
        rate = 1 / (packet_interval * len(flows))
        for index, flow in enumerate(flows):
            start = begin + round(index * packet_interval)
            elapsed, count = 0.0, 0
            while start + round(elapsed) < end:
                count += 1
                elapsed += 1 / rate
            patterns.append({"name": f"{flow['name']}-{phase}", "flows": [flow["id"]],
                             "packets_per_flow": count, "start_cycle": start,
                             "packet_rate": {"distribution": "constant",
                                             "unit": "packets_per_cycle_per_flow", "value": rate},
                             "packet_size_bytes": {"distribution": "constant", "value": cfg["packet_size_bytes"]}})
    write_json(folder / "traffic.json", {"schema": "pifo-traffic-v1", "seed": cfg["seed"], "patterns": patterns})
    flows = [f for t in after for f in t["flows"]]
    write_csv(folder / "flows.csv", [{"flow": f["id"], "flow_name": f["name"],
                                     "untouched_witness": f["id"] == cfg["untouched_flow_id"]} for f in flows])
    traffic = generate_traffic(load_traffic_program(folder / "traffic.json"))
    assert all(p.size_bytes == cfg["packet_size_bytes"] and p.cycle < cfg["end_cycle"] for p in traffic)
    counts = Counter(p.global_flow_id for p in traffic if p.cycle < cfg["t1"])
    assert max(counts.values()) - min(counts.values()) <= 1
    for run in cfg["runs"]:
        path = RESULTS / request / f"m-{m}" / run
        program, plan = compile_request(spec, run)
        retained_peaks = {}
        for transaction in (program.initial, *program.transactions):
            retained = peak = 0
            for command in transaction.commands:
                if command.command == "CommitMapper":
                    break
                if retained or command.command in {"UpdateMapperPre", "UpdateMapperPost"}:
                    retained += 1
                    peak = max(peak, retained)
            assert peak < cfg["control_queue_depth"], (request, m, run, transaction.name, peak)
            retained_peaks[transaction.name] = peak
        plan["retained_epoch_commands"] = retained_peaks
        write_transaction_program(path / "transactions.txt", program)
        write_json(path / "transactions.plan.json", plan)
        shutil.copyfile(folder / "flows.csv", path / "flows.csv")
    return folder


def measure(cfg, request, m, run):
    point = RESULTS / request / f"m-{m}"
    path = point / run
    packets = read_csv(path / "packet-outcomes.csv")
    source = {int(p["request_id"]): p for p in read_csv(path / "requests.csv")}
    completed = {int(p["request_id"]): p for p in read_csv(path / "request-results.csv")}
    assert len(packets) == len(source) == len(completed), (path, "missing packets")
    assert len({p["request_id"] for p in packets}) == len(source), (path, "duplicate packets")
    flows = defaultdict(list)
    for p in packets:
        original = source[int(p["request_id"])]
        assert (p["flow"], p["push_cycle"], p["size_bytes"]) == (
            original["global_flow_id"], original["cycle"], original["size_bytes"])
        assert p["dropped"] == "false" and p["pop_cycle"], (path, p)
        completion = completed[int(p["request_id"])]
        assert completion["completed_cycle"] == p["pop_cycle"]
        assert int(p["push_cycle"]) <= int(completion["admitted_cycle"]) <= int(p["pop_cycle"])
        flows[p["flow"]].append(p)
    for data in flows.values():
        ordered = sorted(data, key=lambda p: (int(p["push_cycle"]), int(p["request_id"])))
        pops = [int(p["pop_cycle"]) for p in ordered]
        assert pops == sorted(pops), (path, "per-flow reordering")
    plan = json.loads((path / "transactions.plan.json").read_text())
    events = read_csv(path / "reconfiguration-events.csv") if run != "control-p2" else []
    observed = read_csv(path / "controller-instructions.csv")
    maintenance = read_csv(path / "maintenance-events.csv")
    accepted = [int(r["cycle"]) for r in observed if r["phase"] == "accepted"]
    assert len(accepted) == sum(plan["instruction_counts"].values())
    assert len(accepted) == len(set(accepted))
    commits = commit_rows({run: path})
    start = int(events[0]["start_cycle"]) if events else cfg["t1"]
    ready = max((int(e["install_finish_cycle"]) for e in events), default=cfg["t1"])
    main = events[0] if events else {}
    stop = int(main["resume_cycle"]) - start if run in {"prefill", "reset"} else 0
    hardware_stops = [int(r["cycle"]) for r in maintenance if r["event"] == "hardware_stop"]
    hardware_resumes = [int(r["cycle"]) for r in maintenance if r["event"] == "hardware_resume"]
    assert len(hardware_stops) == len(hardware_resumes)
    if run in {"rio", "control-p2"}:
        assert stop == 0 and not any(r["command"] == "StopWorld" for r in observed)
        assert not hardware_stops
    t1 = cfg["t1"]
    backlog = [p for p in packets if int(p["push_cycle"]) < t1 <= int(p["pop_cycle"])]
    witness = cfg["untouched_flow_id"]
    labels = {int(r["flow"]): r["flow_name"] for r in read_csv(RESOURCES / request / f"m-{m}" / "flows.csv")}
    result = {"request": request, "tenants": m, "flows": 2 * m, "run": run,
              "main_install_instructions": plan["eligible_at_t1_instructions"],
              "guarded_instructions": plan["guarded_instructions"],
              "cycles_to_publication": int(main["commit_applied_cycle"]) - start if main else 0,
              "cycles_to_main_ready": int(main["install_finish_cycle"]) - start if main else 0,
              "cycles_to_completion": ready - t1,
              "request_start_cycle": start, "configuration_finish_cycle": ready,
              "main_bank_replay_cycles": int(main["bank_cleanup_cycles"]) if main else 0,
              "total_bank_replay_cycles": sum(int(e["bank_cleanup_cycles"]) for e in events),
              "main_mapper_write_instructions": sum(r["phase"] == "accepted"
                  and r["command"] in {"UpdateMapperPre", "UpdateMapperPost"}
                  and int(r["cycle"]) < int(main["commit_cycle"]) for r in observed) if main else 0,
              "guard_drain_instructions": sum(r["phase"] == "accepted" and r["command"] == "GuardDrain" for r in observed),
              "global_stop_cycles": stop,
              "hardware_stop_cycles": sum(b - a for a, b in zip(hardware_stops, hardware_resumes)),
              "peak_stop_buffer_packets": int(main.get("peak_buffer_occupancy_packets") or 0),
              "pre_t1_backlog_packets": len(backlog),
              "pre_t1_waiting_at_input": sum(int(completed[int(p["request_id"])]["admitted_cycle"]) >= t1 for p in backlog),
              "pre_t1_utilization": sum(int(p["size_bytes"]) for p in packets if int(p["pop_cycle"]) < t1) / (t1 * cfg["link_bytes_per_cycle"]),
              "measured_pre_t1_offered_load": sum(int(p["size_bytes"]) for p in packets if int(p["push_cycle"]) < t1) / (t1 * cfg["link_bytes_per_cycle"]),
              "measured_post_t1_offered_load": sum(int(p["size_bytes"]) for p in packets if int(p["push_cycle"]) >= t1) / ((cfg["end_cycle"] - t1) * cfg["link_bytes_per_cycle"]),
              "untouched_flow": labels[witness], "peak_added_delay_cycles": None,
              "completed_packets": len(packets), "drops": 0, "reorderings": 0,
              "trace_sha256": hashlib.sha256((path / "requests.csv").read_bytes()).hexdigest()}
    if stop:
        changes = Counter()
        for p in packets:
            changes[int(p["push_cycle"])] += 1
            changes[int(p["pop_cycle"])] -= 1
        count = peak = 0
        for cycle, delta in sorted(changes.items()):
            count += delta
            if start <= cycle <= int(main["resume_cycle"]):
                peak = max(peak, count)
        result["peak_stop_buffer_packets_from_csv"] = peak
    else:
        result["peak_stop_buffer_packets_from_csv"] = 0
    assert result["peak_stop_buffer_packets_from_csv"] == result["peak_stop_buffer_packets"], (path, "stop-buffer mismatch")
    reference_path = point / "control-p2" / "packet-outcomes.csv"
    pairs = []
    if reference_path.exists():
        assert (path / "requests.csv").read_bytes() == (point / "control-p2" / "requests.csv").read_bytes()
        reference = {r["request_id"]: r for r in read_csv(reference_path)}
        for p in flows[str(witness)]:
            ref = reference[p["request_id"]]
            if int(p["pop_cycle"]) >= t1 or int(ref["pop_cycle"]) >= t1:
                pairs.append({"request_id": p["request_id"], "flow": witness, "flow_name": labels[witness],
                              "push_cycle": p["push_cycle"], "pop_cycle": p["pop_cycle"],
                              "control_pop_cycle": ref["pop_cycle"],
                              "added_delay_cycles": int(p["pop_cycle"]) - int(ref["pop_cycle"])})
        result["peak_added_delay_cycles"] = max(int(p["added_delay_cycles"]) for p in pairs)
    write_csv(path / "untouched-delay.csv", pairs)
    write_csv(path / "commits.csv", commits)
    write_json(path / "measurements.json", result)
    return result


def summarize(cfg):
    rows, commits = [], []
    for request in cfg["requests"]:
        for m in cfg["tenants"]:
            for run in cfg["runs"]:
                path = RESULTS / request / f"m-{m}" / run
                if not (path / "packet-outcomes.csv").exists():
                    continue
                row = measure(cfg, request, m, run)
                rows.append(row)
                commits += [{"request": request, "tenants": m, "flows": 2 * m, **c}
                            for c in read_csv(path / "commits.csv")]
                print(json.dumps(row), flush=True)
    write_csv(RESULTS / "measurements.csv", rows)
    write_csv(RESULTS / "commits.csv", commits)
    expected = len(cfg["requests"]) * len(cfg["tenants"]) * len(cfg["runs"])
    lines = ["# Tree-size scalability results", "",
             f"{len(rows)} of {expected} hardware runs have measured packet outcomes. Compiled plans alone are not counted as runs.", "",
             "See [experiment specification](../../experiments/scalability/README.md) for fixed hardware, traffic, baseline budgets and metric definitions, "
             "and [run provenance](run-notes.md) for this batch's source and validation record.", ""]
    if len(rows) == expected:
        lines += ["## Comparison with baselines", "",
                  "Endpoints below are m=2 → m=32 (4 → 64 existing flows). Every flow has a distinct hardware FIFO leaf. "
                  "Instruction counts exclude initial-policy setup; completion includes post-install cleanup. "
                  "Added delay is the maximum paired difference for Tenant_02_A against the same point's steady-p2 control.", "",
                  "| Request | Run | Main instructions | Guarded/cleanup instructions | Completion cycles | Main replay cycles | Peak added delay cycles |",
                  "| --- | --- | ---: | ---: | ---: | ---: | ---: |"]
        for request in cfg["requests"]:
            for run in cfg["runs"]:
                endpoints = [next(r for r in rows if r["request"] == request and r["run"] == run and r["tenants"] == m)
                             for m in (min(cfg["tenants"]), max(cfg["tenants"]))]
                values = [" → ".join(str(r[field]) for r in endpoints) for field in (
                    "main_install_instructions", "guarded_instructions", "cycles_to_completion",
                    "main_bank_replay_cycles", "peak_added_delay_cycles")]
                lines.append("| " + " | ".join([request, run, *values]) + " |")
        lines += ["", "Read the full five-point data below: endpoint arrows do not imply monotonic intermediate values. "
                  "In particular, reset retains the original 512+513-cycle model budget. Its stop can remain near that "
                  "floor until actual rebuilding dominates; a linear instruction curve does not imply a linear stop curve. "
                  "Paired delay also includes scheduling-state differences from the already-p2 control and need not increase monotonically.", "",
                  "[R-add figure](figures/add/figure.svg), [R-reweight figure](figures/reweight/figure.svg), "
                  "[separate bank-replay figure](figures/bank-replay/figure.svg). Each figure folder includes standalone plot code and CSVs.", "",
                  f"All {sum(r['completed_packets'] for r in rows):,} generated packets completed, with zero drops and zero per-flow reorderings.", "",
                  "## All measured points", ""]
    lines += [
             "| Request | m | Run | Main / cleanup instructions | To publication | Main bank replay | To completion | Global stop | Peak stop buffer | Peak added witness delay |",
             "| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for r in rows:
        cells = [r["request"], r["tenants"], r["run"],
                 f"{r['main_install_instructions']} / {r['guarded_instructions']}"]
        cells += [r[k] for k in ("cycles_to_publication", "main_bank_replay_cycles", "cycles_to_completion",
                                 "global_stop_cycles", "peak_stop_buffer_packets", "peak_added_delay_cycles")]
        lines.append("| " + " | ".join("unavailable" if c is None else str(c) for c in cells) + " |")
    lines += ["", "All listed runs pass complete packet coverage, no-drop and per-flow FIFO checks. "
              "Timing values are simulated cycles, not wall-clock build times. The untouched witness is Tenant_02_A; "
              "added delay is reported only when the same point's target-policy control is available. "
              "Negative peaks mean every compared packet completed earlier than in the control.", "",
              "[Measurements CSV](measurements.csv) includes measured offered load, utilization, backlog, source-side waiting, "
              "mapper-write counts and trace hashes. [Commit CSV](commits.csv) separates every publication and bank replay. "
              "Each request/m-N/run directory retains raw packet, instruction and event CSVs, plus complete named-flow delay pairs.", ""]
    pilots = [r for r in rows if r["request"] == "reweight" and r["run"] == "rio" and r["tenants"] in {2, 16}]
    if len(pilots) == 2:
        fields = ("main_install_instructions", "guarded_instructions", "cycles_to_publication",
                  "main_bank_replay_cycles", "cycles_to_completion")
        identical = len({tuple(r[f] for f in fields) for r in pilots}) == 1
        lines += ["## Requested R-reweight pilot checkpoint", "",
                  "The m=2 and m=16 instruction/timing rows are " + ("identical." if identical else "different; inspect the recorded rows."),
                  "ChangeMeta issues no mapper writes: the recorded bank-ready interval is fixed readiness bookkeeping, "
                  "not a scan of the flow table. This pilot does not measure R-add's mapper-write replay or either baseline.", ""]
    if len(rows) != expected:
        lines += ["The full matrix and final figures are not complete. No results from older experiment families are reused.", ""]
    (RESULTS / "README.md").write_text("\n".join(lines))
    for request in cfg["requests"]:
        for m in cfg["tenants"]:
            paired = [r for r in rows if r["request"] == request and r["tenants"] == m and r["run"] != "control-p2"]
            assert len({r["trace_sha256"] for r in paired}) <= 1
            assert len({r["pre_t1_backlog_packets"] for r in paired}) <= 1, "transition runs start from different congestion"
    if len(rows) == expected:
        for name in (*cfg["requests"], "bank-replay"):
            directory = RESULTS / "figures" / name
            write_csv(directory / "data.csv", [{"figure": name, **r} for r in rows
                                               if name == "bank-replay" or r["request"] == name])
            write_csv(directory / "commits.csv", [r for r in commits
                                                  if name == "bank-replay" or r["request"] == name])
            shutil.copyfile(RESOURCES / "plot.py", directory / "plot.py")
            subprocess.run([sys.executable, "-I", str(directory / "plot.py")], check=True)
    return rows


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("prepare", "run", "batch", "summarize"))
    parser.add_argument("--skip-completed", action="store_true")
    parser.add_argument("--requests", nargs="+", choices=("add", "reweight"))
    parser.add_argument("--tenants", nargs="+", type=int, choices=(2, 4, 8, 16, 32))
    parser.add_argument("--runs", nargs="+", choices=("rio", "prefill", "reset", "control-p2"))
    args = parser.parse_args()
    cfg = json.loads((RESOURCES / "settings.json").read_text())
    if args.action == "summarize":
        summarize(cfg)
        return
    jobs = []
    for request in args.requests or cfg["requests"]:
        for m in args.tenants or cfg["tenants"]:
            source = prepare(cfg, request, m)
            if args.action == "prepare":
                continue
            for run in args.runs or cfg["runs"]:
                path = RESULTS / request / f"m-{m}" / run
                if args.skip_completed and (path / "packet-outcomes.csv").exists():
                    measure(cfg, request, m, run)
                    continue
                if args.action == "batch":
                    traffic = generate_traffic(load_traffic_program(source / "traffic.json"))
                    with (path / "requests.csv").open("w", newline="") as destination:
                        write_trace(traffic, destination)
                    relative = path.relative_to(ROOT)
                    arguments = ["--trace", str(relative / "requests.csv"),
                                 "--transactions", str(relative / "transactions.txt"),
                                 "--output", str(relative / "request-results.csv"),
                                 "--packet-outcomes", str(relative / "packet-outcomes.csv"),
                                 "--queue-depth", str(cfg["queue_depth"]),
                                 "--control-queue-depth", str(cfg["control_queue_depth"]),
                                 "--link-bytes-per-cycle", str(cfg["link_bytes_per_cycle"]),
                                 "--max-cycles", str(cfg["max_cycles"]), "--warmup-cycles", "4",
                                 "--no-control-socket", "--no-flat-fifo", "--no-wave", "--quiet", "--verilator"]
                    if run != "control-p2":
                        arguments += ["--transaction-event-output", str(relative / "reconfiguration-events.csv")]
                    jobs.append((request, m, run, arguments))
                    continue
                command = [sys.executable, str(ROOT / "hw/python/pifo_simulator.py"),
                           "--transactions", str(path / "transactions.txt"),
                           "--traffic", str(source / "traffic.json"), "--output-dir", str(path),
                           "--queue-depth", str(cfg["queue_depth"]),
                           "--control-queue-depth", str(cfg["control_queue_depth"]),
                           "--link-bytes-per-cycle", str(cfg["link_bytes_per_cycle"]),
                           "--max-cycles", str(cfg["max_cycles"]), "--evaluation-hardware", "--verilator"]
                print(f"START {request} m={m} {run}", flush=True)
                with (path / "simulation.log").open("w") as log:
                    subprocess.run(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT, check=True)
                print(json.dumps(measure(cfg, request, m, run)), flush=True)
    if jobs:
        manifest = RESULTS / "batch-arguments.txt"
        manifest.write_text("\n".join("\t".join(job[3]) for job in jobs) + "\n")
        command = ["sbt", f"runMain rio.sim.EvaluationRequestSimulatorCli --batch {manifest.relative_to(ROOT)}"]
        with (RESULTS / "batch.log").open("w") as log:
            process = subprocess.Popen(command, cwd=ROOT, stdout=subprocess.PIPE,
                                       stderr=subprocess.STDOUT, text=True)
            for line in process.stdout:
                log.write(line)
                log.flush()
                if "[RequestBatch]" in line:
                    print(line.rstrip(), flush=True)
                if "[RequestBatch] done " in line:
                    index = int(line.split("[RequestBatch] done ")[1].split("/")[0]) - 1
                    request, m, run, _ = jobs[index]
                    print(json.dumps(measure(cfg, request, m, run)), flush=True)
            if process.wait():
                raise RuntimeError(f"batch simulation failed; see {RESULTS / 'batch.log'}")
    summarize(cfg)


if __name__ == "__main__":
    main()
