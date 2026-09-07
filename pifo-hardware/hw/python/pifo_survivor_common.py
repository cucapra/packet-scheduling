"""Resources, trace generation and measured lifecycle costs for Strict* figures."""
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
from collections import defaultdict
from pathlib import Path

from pifo_survivor_compiler import TOPOLOGY

ROOT = Path(__file__).resolve().parents[2]
RESOURCES = ROOT / "experiments/designated-survivor"
RESULTS = ROOT / "experiment-results/designated-survivor"
TITLES = {"link": "Strict* link", "reserved": "Reserved-PE Strict wrapper"}
COLORS = {"link": "#2878b5", "reserved": "#d45b28"}


def settings():
    return json.loads((RESOURCES / "settings.json").read_text())


def read_csv(path):
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def write_csv(path, rows):
    with path.open("w", newline="") as destination:
        writer = csv.DictWriter(destination, fieldnames=list(rows[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def figure_args(description):
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("--results", type=Path, default=RESULTS)
    return parser.parse_args()


def save_figure(figure, root, name, rows):
    path = root / "figures" / name
    path.mkdir(parents=True, exist_ok=True)
    figure.savefig(path / "figure.png", dpi=180)
    svg = path / "figure.svg"
    figure.savefig(svg, metadata={"Date": None})
    svg.write_text("\n".join(line.rstrip() for line in svg.read_text().splitlines()) + "\n")
    write_csv(path / "data.csv", rows)


def make_traffic(case, pre, cfg):
    if pre < 0:
        raise ValueError("pre-phase duration must be non-negative")
    post = max(cfg["post_min_cycles"], pre + cfg["post_extra_cycles"])
    patterns = []
    for phase, start, duration, rates in (("before", 0, pre, {1: .4, 2: .8}),
                                          ("after", pre, post, {1: .4, 2: .2, 3: .2})):
        for flow, fraction in rates.items():
            if duration == 0:
                continue
            rate = fraction * cfg["link_bytes_per_cycle"] / cfg["packet_size_bytes"]
            patterns.append({"name": f"{cfg['flow_labels'][str(flow)]}-{phase}", "flows": [flow],
                "packets_per_flow": math.ceil(duration * rate), "start_cycle": start,
                "packet_rate": {"distribution": "constant", "unit": "packets_per_cycle_per_flow", "value": rate},
                "packet_size_bytes": {"distribution": "constant", "value": cfg["packet_size_bytes"]}})
    case.mkdir(parents=True, exist_ok=True)
    path = case / "traffic.json"
    path.write_text(json.dumps({"schema": "pifo-traffic-v1", "seed": cfg["seed"], "patterns": patterns}, indent=2) + "\n")
    return path, pre + post


def validate_packets(path):
    raw = read_csv(path / "packet-outcomes.csv")
    inputs = {int(r["request_id"]): r for r in read_csv(path / "requests.csv")}
    assert len(raw) == len(inputs) and {int(r["request_id"]) for r in raw} == set(inputs), "missing/duplicate packets"
    flows = defaultdict(list)
    data = []
    for row in raw:
        assert row["dropped"] == "false" and row["pop_cycle"], f"{path}: dropped/stranded packet"
        r = {k: int(row[k]) for k in ("request_id", "flow", "push_cycle", "pop_cycle", "size_bytes")}
        original = inputs[r["request_id"]]
        assert (r["flow"], r["push_cycle"], r["size_bytes"]) == (
            int(original["global_flow_id"]), int(original["cycle"]), int(original["size_bytes"])), "changed source metadata"
        assert r["pop_cycle"] >= r["push_cycle"], "negative packet delay"
        flows[r["flow"]].append(r)
        data.append(r)
    for rows in flows.values():
        arrival = sorted(rows, key=lambda r: (r["push_cycle"], r["request_id"]))
        assert [r["pop_cycle"] for r in arrival] == sorted(r["pop_cycle"] for r in arrival), "per-flow reordering"
    return data


def peak_buffer(data, start, end):
    changes = defaultdict(int)
    for r in data:
        changes[r["push_cycle"]] += 1
        changes[r["pop_cycle"]] -= 1
    count = peak = 0
    for cycle, change in sorted(changes.items()):
        count += change
        if cycle <= end:
            if cycle < start:
                peak = count
            else:
                peak = max(peak, count)
    return peak


def measure_run(path):
    plan = json.loads((path / "transactions.plan.json").read_text())
    assert plan.get("topology") == TOPOLOGY, f"{path}: stale results without the hardware FIFO layer; rerun this case"
    data = validate_packets(path)
    t1 = plan["t1"]
    ev = read_csv(path / "reconfiguration-events.csv")
    control = read_csv(path / "controller-instructions.csv")
    maintenance = read_csv(path / "maintenance-events.csv")
    accepted = [r for r in control if r["phase"] == "accepted"]
    assert len(accepted) == sum(plan["instruction_counts"].values()), "instruction count differs from compiler"
    assert len({r["cycle"] for r in accepted}) == len(accepted), "multiple instructions accepted in a cycle"
    commits = [int(r["cycle"]) for r in control if r["phase"] == "committed"]
    lifecycle = defaultdict(list)
    for r in maintenance:
        lifecycle[r["event"]].append({k: int(v) for k, v in r.items() if k != "event"})
    first = ev[0]
    metrics = {**plan, "events": ev, "commit_applied_cycles": commits,
        "packets": len(data), "drops": 0, "reorders": 0,
        "t1_backlog_packets": sum(r["push_cycle"] < t1 <= r["pop_cycle"] for r in data),
        "pre_t1_link_utilization": sum(r["size_bytes"] for r in data if r["pop_cycle"] < t1) / (t1 * settings()["link_bytes_per_cycle"]) if t1 else None,
        "t1_backlog_by_flow": {name: sum(r["flow"] == int(f) and r["push_cycle"] < t1 <= r["pop_cycle"] for r in data)
                               for f, name in settings()["flow_labels"].items()},
        "last_packet_pop_cycle": max(r["pop_cycle"] for r in data),
        "configuration_finish_cycle": max(int(r["finish_cycle"]) for r in ev),
        "old_root_drained_cycle": int(first["drain_cycle"]),
        "post_commit_old_drain_cycles": int(first["drain_cycle"]) - commits[0],
        "old_root_tokens_at_publication": lifecycle["drain_watch_armed"][0]["tokens"],
        "zoom_peak_delay_cycles": max(r["pop_cycle"] - r["push_cycle"] for r in data if r["flow"] == 1),
        "global_stop_cycles": 0, "hardware_stop_cycles": 0, "prefill_entries": 0,
        "prefill_write_cycles": 0, "fixed_stop_overhead_cycles": 0,
        "lifecycle": dict(lifecycle)}
    if plan["mechanism"] == "reserved":
        one = lambda name: lifecycle[name][0]
        start, finish = one("driver_stop")["cycle"], one("driver_resume")["cycle"]
        fill, filled = one("prefill_started"), one("prefill_finished")
        count = fill["tokens"]
        assert count == filled["tokens"] == one("stop_snapshot")["tokens"], "prefill violates the root token count invariant"
        assert filled["cycle"] - fill["cycle"] >= count, "prefill is faster than one token per cycle"
        assert filled["cycle"] <= commits[0], "commit published before prefill finished"
        assert len(lifecycle["hardware_stop"]) == 1, "unexpected teardown outage"
        detach = lifecycle["root_detached"][-1]
        clear = one("clear_engine")
        assert clear["engine_id"] == plan["wrapper"][0]
        assert clear["cycle"] > detach["cycle"], "wrapper cleared before root publication"
        if t1 == settings()["figure_a_pre_cycles"]:
            assert detach["tokens"] > 0 and clear["tokens"] > 0, "canonical teardown did not exercise a nonempty wrapper"
        ready = max(int(first["finish_cycle"]), int(first["drain_cycle"]))
        metrics.update({"global_stop_cycles": finish - start,
            "hardware_stop_cycles": one("hardware_resume")["cycle"] - one("hardware_stop")["cycle"],
            "prefill_entries": count, "prefill_write_cycles": filled["tokens"],
            "prefill_command_to_done_cycles": filled["cycle"] - fill["cycle"],
            "fixed_stop_overhead_cycles": finish - start - count,
            "stop_breakdown_cycles": {
                "driver_to_observed_hardware_gate": one("hardware_stop")["cycle"] - start,
                "observed_gate_to_snapshot": one("stop_snapshot")["cycle"] - one("hardware_stop")["cycle"],
                "snapshot_to_prefill_command": fill["cycle"] - one("stop_snapshot")["cycle"],
                "token_writes": count,
                "prefill_command_overhead_to_publication": finish - fill["cycle"] - count},
            "peak_stop_buffer_packets": peak_buffer(data, start, finish),
            "teardown_ready_cycle": ready,
            "teardown_root_change_cycles": detach["cycle"] - ready,
            "teardown_reclaim_cycles": clear["cycle"] + 1 - ready,
            "teardown_all_config_finished_cycles": int(ev[-1]["finish_cycle"]) - ready,
            "wrapper_tokens_at_detachment": detach["tokens"],
            "wrapper_tokens_discarded": clear["tokens"],
            "packets_served_after_reclamation": sum(r["pop_cycle"] > clear["cycle"] for r in data),
            "stop_window_drops": 0, "stop_window_reorders": 0,
            "teardown_window_drops": 0, "teardown_window_reorders": 0})
    else:
        assert not lifecycle["hardware_stop"] and not lifecycle["prefill_started"], "Strict* materialized a wrapper or stopped traffic"
        assert plan["creation_instruction_categories"]["designate"] == 1
    return metrics


def measure(root):
    report = {"cases": {}, "topology": TOPOLOGY, "requested_sweep_pre_cycles": settings()["sweep_pre_cycles"],
              "buffer_note": "4096 packet metadata FIFO entries per flow; each flow also has a real hardware FIFO PIFO on PE 3. The 1024 scheduler-token slots on each PE are shared across its virtual PIFOs, including old/new FIFO versions. The source gate queue is unbounded and measured, not claimed as hardware RAM.",
              "copy_baseline": "Not run: the existing copy datapath supports frozen drain-only relocation, not a complete live-survivor ascent/brain-state migration protocol."}
    for case in sorted(root.glob("pre-*"), key=lambda p: int(p.name.split("-")[1])):
        if not all((case / run / "maintenance-events.csv").exists() for run in TITLES):
            continue
        traces = [(case / run / "requests.csv").read_bytes() for run in TITLES]
        assert traces[0] == traces[1], f"{case}: source traces differ"
        runs = {run: measure_run(case / run) for run in TITLES}
        assert runs["link"]["t1_backlog_packets"] == runs["reserved"]["t1_backlog_packets"], "different pre-request backlogs"
        report["cases"][case.name] = {"trace_sha256": hashlib.sha256(traces[0]).hexdigest(), "runs": runs}
    if not report["cases"]:
        raise ValueError("no complete paired runs to measure")
    report["completed_pre_cycles"] = [c["runs"]["link"]["t1"] for c in report["cases"].values()]
    report["missing_sweep_pre_cycles"] = sorted(set(report["requested_sweep_pre_cycles"]) - set(report["completed_pre_cycles"]))
    (root / "measurements.json").write_text(json.dumps(report, indent=2) + "\n")
    write_summary(root, report)
    return report


def write_summary(root, report):
    lines = ["# Designated-survivor experiment", "",
        "Measured RTL runs, with identical CBR traffic offered during all stops. Packet delay starts at generation, including time waiting at the input gate. All completed runs have zero packet drops and zero per-flow reorderings.", "",
        "Every flow now terminates in its own hardware FIFO node. Push inserts a token into that FIFO as well as each policy node on the path. Pop traverses the policy nodes and performs a separate FIFO pop before packet completion; no policy node routes directly to a packet output.", "",
        "p1 paths are `root (PE 1) → per-flow FIFO (PE 3)`. Under p2b, zoom keeps that shape; gmail and spotify use `root (PE 1) → RR (PE 2) → per-flow FIFO (PE 3)`. Old/new FIFO versions have distinct vPIFO IDs. The reserved wrapper uses PE 4; Strict* leaves it unused. Compiled physical paths are recorded in each transactions.plan.json.", "",
        "Using four PEs also widens the engine ID, increasing each post-mapper bank from 256 to 512 entries. Configuration finish and reclamation include this longer bank synchronization; it does not stop packet service.", "",
        "## Birth: measured stop cycles", "",
        "| Pre-phase | Backlog at t1 | Strict* stop | Reserved stop | Actual prefill N | Entry writes | Fixed overhead | Hardware gate stop | Peak stop buffer |",
        "| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for case in report["cases"].values():
        a, b = case["runs"]["link"], case["runs"]["reserved"]
        lines.append("| " + " | ".join(str(v) for v in (a["t1"], a["t1_backlog_packets"], a["global_stop_cycles"],
            b["global_stop_cycles"], b["prefill_entries"], b["prefill_write_cycles"], b["fixed_stop_overhead_cycles"],
            b["hardware_stop_cycles"], b["peak_stop_buffer_packets"])) + " |")
    lines += ["", "The headline stop includes driver quiescence before the RTL StopWorld gate, configuration, prefill and commit publication. Hardware gate width is listed separately. Entry writes count actual ready/valid insertions, not an estimate. Fixed overhead is the measured total minus those write cycles.",
        "Strict* has no global pop stop and one designate instruction, but compiling/installing the new tree still costs instructions and a short input-only commit barrier. Zero is not a claim that the entire tree change is instantaneous.", "",
        "## Death: root change and nonempty wrapper reclamation", "",
        "Costs start when both the old root is empty and the creation package is ready for another transaction; the drain wait itself is excluded. `finish` includes mapper-bank synchronization, not a packet outage.", "",
        "| Pre-phase | Root change | Reclaimed | All config finished | Tokens at detach | Tokens discarded | Packets served later |",
        "| ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for case in report["cases"].values():
        b = case["runs"]["reserved"]
        lines.append("| " + " | ".join(str(b[k]) for k in ("t1", "teardown_root_change_cycles", "teardown_reclaim_cycles",
            "teardown_all_config_finished_cycles", "wrapper_tokens_at_detachment", "wrapper_tokens_discarded", "packets_served_after_reclamation")) + " |")
    lines += ["", "UpdateRoot publishes the survivor as the port root. ClearPifoEngine then clears the detached wrapper's occupancy counters in one hardware cycle; its redundant tokens are discarded, not copied, and the survivor's packet queues remain intact. Old memory bits need not be individually erased. This substrate therefore supports nonempty death with constant-time logical reclamation; death is not an O(backlog) token-deletion loop. No additional global stop occurs at teardown.", "",
        "## Drain and instruction accounting", "",
        "| Pre-phase | Mechanism | Commit applied | Old root N at publication | Old root empty | Post-commit drain | Main / collapse / reclaim inst | Categories |",
        "| ---: | --- | ---: | ---: | ---: | ---: | --- | --- |"]
    for case in report["cases"].values():
        for name, m in case["runs"].items():
            lines.append(f"| {m['t1']} | {TITLES[name]} | {m['commit_applied_cycles'][0]} | {m['old_root_tokens_at_publication']} | {m['old_root_drained_cycle']} | {m['post_commit_old_drain_cycles']} | " +
                " / ".join(str(m["instruction_counts"].get(k, 0)) for k in ("replace", "collapse", "reclaim")) + f" | {m['creation_instruction_categories']} |")
    lines += ["", "`start` is transaction execution start; `commit` in reconfiguration-events.csv is instruction acceptance, not publication; controller-instructions.csv records actual publication. `drained` means the final old-root token popped (lower-level traversals may still be in flight). `finish` means the package and bank synchronization completed. Each is recorded separately.",
        "Hardware gate register transitions are observed on the following rising-edge sample; both edges have the same offset, so their width is unaffected. The driver stop ends at commit publication, not at completion of mapper-bank synchronization.", ""]
    canonical = report["cases"].get(f"pre-{settings()['figure_a_pre_cycles']}")
    if canonical:
        a, b = canonical["runs"]["link"], canonical["runs"]["reserved"]
        lines += [f"In Figure A, zoom's peak delay is {a['zoom_peak_delay_cycles']} versus {b['zoom_peak_delay_cycles']} cycles: +{b['zoom_peak_delay_cycles'] - a['zoom_peak_delay_cycles']}, compared with the {b['global_stop_cycles']}-cycle stop. The post-commit old-root drains are {a['post_commit_old_drain_cycles']} and {b['post_commit_old_drain_cycles']} cycles.",
            f"Those drains start with {a['old_root_tokens_at_publication']} and {b['old_root_tokens_at_publication']} tokens respectively: Strict* kept serving the old policy while its commands arrived. The difference in token counts and wrapper traversal latency explains why the drain widths need not be exactly equal.",
            f"At t1={a['t1']}, the measured backlog is {a['t1_backlog_packets']} packets and pre-transition link utilization is {a['pre_t1_link_utilization']:.3f}. These values are recomputed for the FIFO-layer topology, not carried forward from the direct-policy-leaf experiment.", ""]
    lines += ["| Pre-phase | Mechanism | Last packet pop | All configuration finished |",
              "| ---: | --- | ---: | ---: |"]
    for case in report["cases"].values():
        for name, m in case["runs"].items():
            lines.append(f"| {m['t1']} | {TITLES[name]} | {m['last_packet_pop_cycle']} | {m['configuration_finish_cycle']} |")
    lines += [
        "", "## Scope and capacity", "", report["buffer_note"], "",
        "The pre-phases remain 0, 1000, 2500 and 5000 cycles, plus the 2000-cycle Figure A run. Rates, sizes, seeds and durations are unchanged from the previous experiment so the FIFO-layer change is isolated. Figure B uses the measured backlog for this topology, not a nominal target or the previous measurements.",
        "The 1000/2000 points were not run: this experiment's 1024-token PE cannot contain 2000 root tokens. Growing that sorted-register RTL and rerunning is required; silently leaving excess packets at the door would not measure a 2000-token prefill.", "",
        "At the zero-pre-phase point, packets may already be entering as the stop starts. The hardware snapshot/prefill count is recorded separately from backlog immediately before t1.", "",
        "One permanently reserved PE is needed per concurrent materialized wrapper. The Strict* run leaves that PE unused to keep the physical simulation shape identical. Reservation avoids descent and ascent but does not avoid the N birth writes.", "", report["copy_baseline"], "",
        "## Figures and raw files", "",
        "- [Figure A: zoom delay](figures/zoom-delay/figure.png) ([SVG](figures/zoom-delay/figure.svg), [plotted CSV](figures/zoom-delay/data.csv)).",
        "- [Figure B: stop versus backlog](figures/prefill-stop/figure.png) ([SVG](figures/prefill-stop/figure.svg), [plotted CSV](figures/prefill-stop/data.csv)).",
        "- [Measurements](measurements.json). Each pre-N directory contains its traffic input; link/ and reserved/ contain direct transactions, compiler accounting, requests.csv, packet-outcomes.csv, request-results.csv, reconfiguration-events.csv, controller-instructions.csv and maintenance-events.csv.", "",
        "Per-flow packet metadata FIFO order is checked end-to-end; the RTL tokens carry flow IDs, not unique packet IDs. These measurements are not a gate-level timing-closure result.", ""]
    (root / "README.md").write_text("\n".join(lines))
