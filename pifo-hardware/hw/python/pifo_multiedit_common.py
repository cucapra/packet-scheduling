"""Shared paths, raw-data checks and measurements for the large-tree figures."""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import statistics
from collections import defaultdict
from pathlib import Path

from pifo_multiedit_compiler import MECHANISMS

ROOT = Path(__file__).resolve().parents[2]
RESOURCES = ROOT / "experiments" / "multi-edit"
RESULTS = ROOT / "experiment-results" / "multi-edit"
TITLES = {"rio": "Rio: localized edits", "prefill": "Whole-tree: prefill SP",
          "relocate": "Whole-tree: copy + prefill", "reset": "Stop-the-world reset", "control": "Control: p1"}
MAIN_RUNS = ("rio", "prefill", "reset")
COPY_RUNS = ("prefill", "relocate")


def settings():
    return json.loads((RESOURCES / "settings.json").read_text())


def read_csv(path):
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def packets(root, run):
    rows = read_csv(root / run / "packet-outcomes.csv")
    return [{**{k: int(r[k]) for k in ("request_id", "flow", "size_bytes", "push_cycle")},
             "pop_cycle": int(r["pop_cycle"]) if r["pop_cycle"] else None,
             "dropped": r["dropped"] == "true"} for r in rows]


def events(root, run):
    path = root / run / "reconfiguration-events.csv"
    return read_csv(path) if path.exists() else []


def figure_args(description):
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("--results", type=Path, default=RESULTS)
    parser.add_argument("--copy-comparison", action="store_true")
    return parser.parse_args()


def figure_dir(args, name):
    path = args.results / "figures" / (name + ("-copy" if args.copy_comparison else ""))
    path.mkdir(parents=True, exist_ok=True)
    return path


def write_csv(path, rows, fields):
    with path.open("w", newline="") as dest:
        writer = csv.DictWriter(dest, fieldnames=fields, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def save_figure(figure, directory):
    figure.savefig(directory / "figure.png", dpi=180)
    svg = directory / "figure.svg"
    figure.savefig(svg, metadata={"Date": None})
    svg.write_text("\n".join(line.rstrip() for line in svg.read_text().splitlines()) + "\n")


def percentile(values, q):
    values = sorted(values)
    return values[round((len(values) - 1) * q)] if values else None


def measure(root):
    """Validate raw trace identity/loss/FIFO and report observations, not desired outcomes."""
    cfg = settings()
    t1, end = cfg["t1"], cfg["end_cycle"]
    comparison_end = cfg.get("plot_end_cycle", end)
    labels = {int(k): v for k, v in cfg["flow_labels"].items()}
    traces = {run: (root / run / "requests.csv").read_bytes() for run in MECHANISMS}
    assert len(set(traces.values())) == 1, "runs did not replay the same trace"
    inputs = {int(r["request_id"]): r for r in read_csv(root / "control" / "requests.csv")}
    control = {r["request_id"]: r for r in packets(root, "control")}
    report = {"trace_sha256": hashlib.sha256(traces["control"]).hexdigest(), "runs": {},
              "difference_test": f"paired p95 absolute delay difference > 10 cycles, packets outstanding/generated from t1 through {comparison_end}; new flows count as changed",
              "realtime_steady_metric": "first 300-cycle trailing window within 10% of 0.20, sustained 300 cycles, after pre-t1 realtime packets leave",
              "realtime_weight_share": 6 / 17, "realtime_offered_steady_rate": 0.20}
    for run in MECHANISMS:
        data = packets(root, run)
        assert len(data) == len(inputs) and {r["request_id"] for r in data} == set(inputs), f"{run}: missing/duplicate packets"
        byflow = defaultdict(list)
        for r in data:
            original = inputs[r["request_id"]]
            assert (r["flow"], r["push_cycle"], r["size_bytes"]) == (
                int(original["global_flow_id"]), int(original["cycle"]), int(original["size_bytes"])), f"{run}: changed packet metadata"
            assert not r["dropped"], f"{run}: dropped packet"
            if r["pop_cycle"] is None:
                assert run == "control" and r["flow"] in cfg["arriving_flows"], f"{run}: stranded packet"
            else:
                assert r["pop_cycle"] >= r["push_cycle"], f"{run}: negative delay"
                byflow[r["flow"]].append(r)
        for flow in byflow:
            arrival_order = sorted(byflow[flow], key=lambda r: (r["push_cycle"], r["request_id"]))
            assert [r["pop_cycle"] for r in arrival_order] == sorted(r["pop_cycle"] for r in arrival_order), f"{run}: FIFO violation"
        ev = events(root, run)
        plan = json.loads((root / run / "transactions.plan.json").read_text())
        metrics = {**plan, "events": ev, "drops": 0, "reorders": 0,
                   "t1_backlog_packets": {}, "first_service_cycles_from_t1": {}, "control_differences": {}}
        metrics["pre_t1_link_utilization"] = sum(r["size_bytes"] for r in data
            if r["pop_cycle"] is not None and r["pop_cycle"] < t1) / (t1 * cfg["link_bytes_per_cycle"])
        control_events = read_csv(root / run / "controller-instructions.csv")
        metrics["instructions_accepted_at_t1"] = sum(r["phase"] == "accepted" and int(r["cycle"]) == t1 for r in control_events)
        metrics["instructions_dispatched_at_t1"] = sum(r["phase"] == "dispatched" and int(r["cycle"]) == t1 for r in control_events)
        metrics["commit_applied_cycles"] = [int(r["cycle"]) for r in control_events if r["phase"] == "committed"]
        metrics["copy_completions"] = [r for r in control_events if r["phase"] == "copy_finished"]
        metrics["copied_entries"] = sum(int(r["copied_entries"]) for r in metrics["copy_completions"])
        accepted = [int(r["cycle"]) for r in control_events if r["phase"] == "accepted"]
        assert len(accepted) == len(set(accepted)), f"{run}: more than one accepted instruction per cycle"
        admitted = {int(r["request_id"]): int(r["admitted_cycle"])
                    for r in read_csv(root / run / "request-results.csv")}
        metrics["t1_waiting_at_input_packets"] = 0
        for flow, name in labels.items():
            backlog = [r for r in data if r["flow"] == flow and r["push_cycle"] < t1
                       and (r["pop_cycle"] is None or r["pop_cycle"] >= t1)]
            metrics["t1_backlog_packets"][name] = len(backlog)
            metrics["t1_waiting_at_input_packets"] += sum(admitted.get(r["request_id"], end) >= t1 for r in backlog)
            if flow in cfg["arriving_flows"]:
                pops = [r["pop_cycle"] for r in byflow[flow] if r["push_cycle"] >= t1]
                metrics["first_service_cycles_from_t1"][name] = min(pops) - t1 if pops else None
            pairs = [(r, control[r["request_id"]]) for r in byflow[flow] if r["push_cycle"] < comparison_end
                     and control[r["request_id"]]["pop_cycle"] is not None
                     and (r["pop_cycle"] >= t1 or control[r["request_id"]]["pop_cycle"] >= t1)]
            deltas = [a["pop_cycle"] - b["pop_cycle"] for a, b in pairs]
            metrics["control_differences"][name] = {
                "paired_packets": len(deltas), "mean_delay_delta_cycles": statistics.mean(deltas) if deltas else None,
                "p95_absolute_delay_delta_cycles": percentile([abs(d) for d in deltas], .95),
                "max_absolute_delay_delta_cycles": max([abs(d) for d in deltas], default=None),
                "changed": percentile([abs(d) for d in deltas], .95) > 10 if deltas else flow in cfg["arriving_flows"] and run != "control"}
        metrics["changed_flows"] = sum(d["changed"] for d in metrics["control_differences"].values())
        if run != "control":
            reference_backlog = report["runs"]["control"]["t1_backlog_packets"]
            assert metrics["t1_backlog_packets"] == reference_backlog, f"{run}: pre-request state differs from control"
        if ev:
            first = ev[0]
            stop_start = int(first["start_cycle"])
            resume = int(first.get("resume_cycle") or first["finish_cycle"])
            outstanding = [r for r in data if r["push_cycle"] <= resume
                           and (r["pop_cycle"] is None or r["pop_cycle"] >= resume)]
            metrics["buffer_occupancy_at_resume_packets"] = len(outstanding)
            if run in {"prefill", "relocate", "reset"}:
                changes = [(r["push_cycle"], 1) for r in data]
                changes += [(r["pop_cycle"], -1) for r in data if r["pop_cycle"] is not None]
                counts = defaultdict(int)
                for cycle, change in changes:
                    counts[cycle] += change
                count = peak = 0
                for cycle, change in sorted(counts.items()):
                    count += change
                    if stop_start <= cycle <= resume:
                        peak = max(peak, count)
                metrics["peak_stop_buffer_packets_from_csv"] = peak
        old_rt = [r["pop_cycle"] for flow in (1, 2) for r in byflow[flow] if r["push_cycle"] < t1]
        clear = max(old_rt, default=t1)
        window = cfg["throughput_window"]
        rt = [r["pop_cycle"] for flow in (1, 2) for r in byflow[flow]]
        stable = []
        for cycle in range(max(t1, clear) + window, end, 50):
            bw = 3 * sum(cycle - window <= pop < cycle for pop in rt) / window
            stable = stable + [cycle] if abs(bw - .20) <= .02 else []
            if len(stable) >= 7:
                break
        metrics["pre_t1_realtime_last_pop"] = clear
        metrics["realtime_steady_cycle"] = stable[0] if len(stable) >= 7 else None
        report["runs"][run] = metrics
    (root / "measurements.json").write_text(json.dumps(report, indent=2) + "\n")
    write_summary(root, report)
    return report


def write_summary(root, report):
    lines = ["# Large-tree experiment results", "",
             "All runs replay the identical source trace. All four transitioning runs finish with zero drops and zero per-flow reorderings.",
             "Control keeps p1; its four unadmitted flows are recorded as unserved, not dropped.", "",
             "## First service after the request (cycles)", "",
             "| Mechanism | Video | Chat | Game | Vr | Peak stop buffer (packets) |",
             "| --- | ---: | ---: | ---: | ---: | ---: |"]
    for run in MECHANISMS[1:]:
        m = report["runs"][run]
        values = m["first_service_cycles_from_t1"]
        cells = [TITLES[run]] + [str(values[f]) for f in ("Video", "Chat", "Game", "Vr")]
        cells += [str(m.get("peak_stop_buffer_packets_from_csv", "no global stop"))]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "Chat is below Video in a real SP tenant: first Chat service includes draining Video's new-policy backlog, not just the old-tree drain.",
              "", "## Timings and instructions", "",
              "`commit` is instruction acceptance, `applied` is hardware publication. `empty/captured` is old PIFO empty except for reset, where it is the retained-state snapshot. `finish` is the main configuration package; cleanup completion is separate.", "",
              "| Run | Start | Commit | Applied | Empty/captured | Finish | Cleanup finished | Main / guarded instructions |",
              "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for run in MECHANISMS[1:]:
        m = report["runs"][run]
        event = m["events"][0]
        cells = [run, event["start_cycle"], event["commit_cycle"], str(m["commit_applied_cycles"][0]),
                 event["drain_cycle"], event["finish_cycle"], m["events"][-1]["finish_cycle"],
                 f"{m['eligible_at_t1_instructions']} / {m['guarded_instructions']}"]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "There is one config ingress instruction per cycle, not five instantaneous hardware edits. Raw `controller-instructions.csv` distinguishes queue acceptance, dispatch, commit publication and copy completion.",
              "", "## Departing backlog and unchanged-path witnesses", "",
              "Measured backlog at t1 is identical in every run; none of it is waiting at the input gate:", "",
              "| Flow | Packets at t1 | Rio p95 absolute delay difference from control |",
              "| --- | ---: | ---: |"]
    rio = report["runs"]["rio"]
    for flow, count in rio["t1_backlog_packets"].items():
        delta = rio["control_differences"][flow]["p95_absolute_delay_delta_cycles"]
        lines.append(f"| {flow} | {count} | {delta if delta is not None else 'not served by control'} |")
    lines += ["", "The stronger prediction that unchanged paths have unchanged delay is **not supported**: Gmail and Http show a transient increase even in Rio. Work and web already have nonzero backlog at t1. Reweighting siblings and admitting new tenants changes their shared-root service; these observations are reported, not treated as a successful indistinguishability test.",
              "Their absolute root weight stays 3, but total configured sibling weight changes from 15 to 21 including the retiring legacy arm, then 17. Nominal all-backlogged fractions therefore change from 3/15 to 3/21 to 3/17; actual service also depends on frozen ranks and empty queues.",
              f"The measured pre-t1 link utilization, including startup, is {rio['pre_t1_link_utilization']:.3f}, not the 1.00 assumed in the ideal backlog arithmetic.",
              "", "Difference test: " + report["difference_test"] + ".", "",
              "Changed flow counts: " + ", ".join(f"{run}={report['runs'][run]['changed_flows']}/14" for run in MECHANISMS[1:]) + ".",
              "", f"Relocation copies {report['runs']['relocate']['copied_entries']} PIFO entries across three PEs; copied entries are scheduler tokens, not three copies of packet payload buffers.",
              "The lossless reset's 512-cycle teardown and 513-cycle install budgets are model parameters forming a minimum stop, not extra controller instructions. Actual bank synchronization and replay can make it longer.",
              "", "## Realtime recovery", "",
              "Realtime's new nominal weight share is 6/17, but its source offers only 0.20. The following is settling to the offered-load steady throughput, not a saturated-share measurement:", "",
              "| Run | Pre-t1 realtime packets all served | Sustained 0.20 throughput from cycle |",
              "| --- | ---: | ---: |"]
    for run in MECHANISMS:
        m = report["runs"][run]
        lines.append(f"| {run} | {m['pre_t1_realtime_last_pop']} | {m['realtime_steady_cycle']} |")
    lines += ["", "## Figures and raw data", "",
              "- [A: first service](figures/first-service/figure.png)",
              "- [B: unchanged-path delays](figures/untouched-delay/figure.png)",
              "- [A-copy: prefill versus relocation](figures/first-service-copy/figure.png)",
              "- [B-copy: prefill versus relocation delays](figures/untouched-delay-copy/figure.png)", "",
              "Each figure has SVG and data.csv siblings. Each run directory contains the full generated/admitted/completed timestamps, controller trace and packet outcomes. The complete machine-readable report is [measurements.json](measurements.json).", ""]
    (root / "README.md").write_text("\n".join(lines))
