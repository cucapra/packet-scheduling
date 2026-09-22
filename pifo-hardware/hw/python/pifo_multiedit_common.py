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
from pifo_figures.evaluation import commit_rows

ROOT = Path(__file__).resolve().parents[2]
RESOURCES = ROOT / "experiments" / "multi-edit"
RESULTS = ROOT / "experiment-results" / "multi-edit"
TITLES = {"rio": "Rio: localized edits", "prefill": "Whole-tree: prefill SP",
          "relocate": "Whole-tree: copy + prefill", "reset": "Stop-the-world reset",
          "control": "Control: p1", "control-p2": "Control: p2"}
CONTROL_RUNS = ("control", "control-p2")
TRANSITION_RUNS = ("rio", "prefill", "relocate", "reset")
MAIN_RUNS = ("rio", "prefill", "reset")
COPY_RUNS = ("prefill", "relocate")
FIRST_SERVICE_RUNS = ("control-p2", *MAIN_RUNS)


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
    baselines = {baseline: {r["request_id"]: r for r in packets(root, baseline)}
                 for baseline in CONTROL_RUNS}
    report = {"trace_sha256": hashlib.sha256(traces["control"]).hexdigest(), "runs": {},
              "difference_test": f"paired p95 absolute delay difference > 10 cycles, packets outstanding/generated from t1 through {comparison_end}; admission differences count as changed",
              "realtime_steady_metric": "first 300-cycle trailing window within 10% of 0.20, sustained 300 cycles, after pre-t1 realtime packets leave",
              "realtime_weight_share": 6 / 17, "realtime_offered_steady_rate": 0.20}
    for run in MECHANISMS:
        data = packets(root, run)
        plan = json.loads((root / run / "transactions.plan.json").read_text())
        unadmitted = set(plan.get("unadmitted_flows", ()))
        assert len(data) == len(inputs) and {r["request_id"] for r in data} == set(inputs), f"{run}: missing/duplicate packets"
        byflow = defaultdict(list)
        for r in data:
            original = inputs[r["request_id"]]
            assert (r["flow"], r["push_cycle"], r["size_bytes"]) == (
                int(original["global_flow_id"]), int(original["cycle"]), int(original["size_bytes"])), f"{run}: changed packet metadata"
            assert not r["dropped"], f"{run}: dropped packet"
            if r["pop_cycle"] is None:
                assert r["flow"] in unadmitted, f"{run}: stranded packet"
            else:
                assert r["pop_cycle"] >= r["push_cycle"], f"{run}: negative delay"
                byflow[r["flow"]].append(r)
        for flow in byflow:
            arrival_order = sorted(byflow[flow], key=lambda r: (r["push_cycle"], r["request_id"]))
            assert [r["pop_cycle"] for r in arrival_order] == sorted(r["pop_cycle"] for r in arrival_order), f"{run}: FIFO violation"
        ev = events(root, run)
        assert plan.get("experiment_rules") == "shared-replay-guarded-cleanup-v1", f"{run}: stale pre-replay results"
        metrics = {**plan, "events": ev, "drops": 0, "reorders": 0,
                   "commits": commit_rows({run: root / run}),
                   "t1_backlog_packets": {}, "first_service_cycles_from_t1": {},
                   "control_differences": {}, "p2_control_differences": {}}
        metrics["pre_t1_link_utilization"] = sum(r["size_bytes"] for r in data
            if r["pop_cycle"] is not None and r["pop_cycle"] < t1) / (t1 * cfg["link_bytes_per_cycle"])
        control_events = read_csv(root / run / "controller-instructions.csv")
        metrics["instructions_accepted_at_t1"] = sum(r["phase"] == "accepted" and int(r["cycle"]) == t1 for r in control_events)
        metrics["instructions_dispatched_at_t1"] = sum(r["phase"] == "dispatched" and int(r["cycle"]) == t1 for r in control_events)
        metrics["commit_applied_cycles"] = [int(r["cycle"]) for r in control_events if r["phase"] == "committed"]
        metrics["copy_completions"] = [r for r in control_events if r["phase"] == "copy_finished"]
        metrics["copied_entries"] = sum(int(r["copied_entries"]) for r in metrics["copy_completions"])
        copied_pifos = [r for r in read_csv(root / run / "maintenance-events.csv") if r["event"] == "copy_source"]
        metrics["copied_pifos"] = len(copied_pifos)
        metrics["copy_source_pifos"] = copied_pifos
        if run == "relocate":
            assert sum(int(r["tokens"]) for r in copied_pifos) == metrics["copied_entries"], "copy token accounting mismatch"
            metrics["copied_buffered_packets"] = sum(int(r["tokens"]) for r in copied_pifos
                                                      if (r["engine_id"], r["vpifo_id"]) == ("1", "1"))
        accepted = [int(r["cycle"]) for r in control_events if r["phase"] == "accepted"]
        assert len(accepted) == sum(plan["instruction_counts"].values()), f"{run}: accepted instruction count differs from compiler"
        assert len(accepted) == len(set(accepted)), f"{run}: more than one accepted instruction per cycle"
        admitted = {int(r["request_id"]): int(r["admitted_cycle"])
                    for r in read_csv(root / run / "request-results.csv")}
        metrics["t1_waiting_at_input_packets"] = 0
        current = {r["request_id"]: r for r in data}

        def difference(flow, baseline):
            reference = baselines[baseline]
            pairs = [(r, reference[r["request_id"]]) for r in byflow[flow]
                     if r["push_cycle"] < comparison_end
                     and reference[r["request_id"]]["pop_cycle"] is not None
                     and (r["pop_cycle"] >= t1 or reference[r["request_id"]]["pop_cycle"] >= t1)]
            deltas = [a["pop_cycle"] - b["pop_cycle"] for a, b in pairs]
            status_changed = any(
                (r["pop_cycle"] is None) != (reference[r["request_id"]]["pop_cycle"] is None)
                for r in current.values()
                if r["flow"] == flow and r["push_cycle"] < comparison_end
                and (r["pop_cycle"] is None
                     or reference[r["request_id"]]["pop_cycle"] is None
                     or r["pop_cycle"] >= t1
                     or reference[r["request_id"]]["pop_cycle"] >= t1)
            )
            p95 = percentile([abs(delta) for delta in deltas], .95)
            return {
                "paired_packets": len(deltas),
                "mean_delay_delta_cycles": statistics.mean(deltas) if deltas else None,
                "p95_absolute_delay_delta_cycles": p95,
                "max_absolute_delay_delta_cycles": max([abs(delta) for delta in deltas], default=None),
                "admission_status_changed": status_changed,
                "changed": status_changed or (p95 is not None and p95 > 10),
            }

        for flow, name in labels.items():
            backlog = [r for r in data if r["flow"] == flow and r["push_cycle"] < t1
                       and (r["pop_cycle"] is None or r["pop_cycle"] >= t1)]
            metrics["t1_backlog_packets"][name] = len(backlog)
            metrics["t1_waiting_at_input_packets"] += sum(admitted.get(r["request_id"], end) >= t1 for r in backlog)
            if flow in cfg["arriving_flows"]:
                pops = [r["pop_cycle"] for r in byflow[flow] if r["push_cycle"] >= t1]
                metrics["first_service_cycles_from_t1"][name] = min(pops) - t1 if pops else None
            metrics["control_differences"][name] = difference(flow, "control")
            metrics["p2_control_differences"][name] = difference(flow, "control-p2")
        metrics["changed_flows"] = sum(d["changed"] for d in metrics["control_differences"].values())
        metrics["changed_flows_vs_p1"] = metrics["changed_flows"]
        metrics["changed_flows_vs_p2"] = sum(d["changed"] for d in metrics["p2_control_differences"].values())
        if run not in CONTROL_RUNS:
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
             "All six runs replay the identical source trace. All four transitioning runs finish with zero drops and zero per-flow reorderings.",
             "The p1 control leaves the four arriving flows unadmitted. The p2 control starts and stays in p2, so the two legacy flows absent from p2 are unadmitted. Both cases are recorded as unserved, not dropped.", "",
             "## First service after the request (cycles)", "",
             "| Mechanism | Video | Chat | Game | Vr | Peak stop buffer (packets) |",
             "| --- | ---: | ---: | ---: | ---: | ---: |"]
    for run in ("control-p2", *TRANSITION_RUNS):
        m = report["runs"][run]
        values = m["first_service_cycles_from_t1"]
        cells = [TITLES[run]] + [str(values[f]) for f in ("Video", "Chat", "Game", "Vr")]
        cells += [str(m.get("peak_stop_buffer_packets_from_csv", "no global stop"))]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "Chat is below Video in a real SP tenant: first Chat service includes draining Video's new-policy backlog, not just the old-tree drain.",
              "", "## Timings and instructions", "",
              "`commit` is instruction acceptance, `applied` is hardware publication. `empty/captured` is old PIFO empty except for reset, where it is the retained-state snapshot. `install_finish_cycle` is each commit's own replay readiness; `finish_cycle` includes its linked cleanup. Final configuration readiness is the last cleanup/reclamation commit, not the end of packet traffic.", "",
              "| Run | Start | Commit | Applied | Empty/captured | Finish | Cleanup finished | Main / guarded instructions |",
              "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for run in TRANSITION_RUNS:
        m = report["runs"][run]
        event = m["events"][0]
        cells = [run, event["start_cycle"], event["commit_cycle"], str(m["commit_applied_cycles"][0]),
                 event["drain_cycle"], event["finish_cycle"], m["events"][-1]["finish_cycle"],
                 f"{m['eligible_at_t1_instructions']} / {m['guarded_instructions']}"]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "| Run | Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |",
              "| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    for run in TRANSITION_RUNS:
        for c in report["runs"][run]["commits"]:
            lines.append("| " + " | ".join(str(v) for v in (run, c["commit"], c["start_cycle"], c["commit_cycle"],
                c["commit_applied_cycle"], c["ready_for_next_commit"], c["instruction_count"],
                c["commit_cycles"], c["bank_replay_cycles"])) + " |")
    lines += ["", "There is one config ingress instruction per cycle, not five instantaneous hardware edits. Raw `controller-instructions.csv` distinguishes queue acceptance, dispatch, commit publication and copy completion.",
              "", "## Departing backlog and unchanged-path witnesses", "",
              "The four transitioning runs start from p1 and have the same measured backlog at t1 as the p1 control; none of it is waiting at the input gate. The steady-p2 control intentionally has a different pre-t1 policy state.", "",
              "| Flow | Packets at t1 | Rio p95 difference vs p1 | Rio p95 difference vs p2 |",
              "| --- | ---: | ---: | ---: |"]
    rio = report["runs"]["rio"]
    for flow, count in rio["t1_backlog_packets"].items():
        delta = rio["control_differences"][flow]["p95_absolute_delay_delta_cycles"]
        p2_delta = rio["p2_control_differences"][flow]["p95_absolute_delay_delta_cycles"]
        lines.append(f"| {flow} | {count} | {delta if delta is not None else 'not jointly served'} | {p2_delta if p2_delta is not None else 'not jointly served'} |")
    witness_names = [settings()["flow_labels"][str(f)] for f in settings()["untouched_flows"]]
    changed_p1 = [name for name in witness_names if rio["control_differences"][name]["changed"]]
    changed_p2 = [name for name in witness_names if rio["p2_control_differences"][name]["changed"]]
    lines += ["", "Untouched-path witnesses exceeding the stated paired-delay threshold versus p1: " +
              (", ".join(changed_p1) if changed_p1 else "none") + "; versus steady p2: " +
              (", ".join(changed_p2) if changed_p2 else "none") + ". Identical internal paths do not by themselves guarantee identical service at a shared root whose sibling weights and load change. The table reports both controls rather than assuming either is the sole counterfactual.",
              "Their absolute root weight stays 3, but total configured sibling weight changes from 15 to 21 including the retiring legacy arm, then 17. Nominal all-backlogged fractions therefore change from 3/15 to 3/21 to 3/17; actual service also depends on frozen ranks and empty queues.",
              f"The measured pre-t1 link utilization, including startup, is {rio['pre_t1_link_utilization']:.3f}, not the 1.00 assumed in the ideal backlog arithmetic.",
              "", "Difference test: " + report["difference_test"] + ".", "",
              "Changed flow counts versus p1: " + ", ".join(f"{run}={report['runs'][run]['changed_flows_vs_p1']}/14" for run in TRANSITION_RUNS) + ".",
              "Changed flow counts versus p2: " + ", ".join(f"{run}={report['runs'][run]['changed_flows_vs_p2']}/14" for run in TRANSITION_RUNS) + ".",
              "", f"Relocation moves {report['runs']['relocate']['copied_pifos']} occupied virtual PIFOs across three PEs, containing {report['runs']['relocate']['copied_entries']} scheduler tokens for {report['runs']['relocate']['copied_buffered_packets']} buffered packets. The dedicated brain-bypassing read/inject datapath exists only in the evaluation image; payload buffers are not copied.",
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
              "Each figure has SVG, data.csv, complete packets.csv, commits.csv and a self-contained plot.py. Copy the folder elsewhere and run `python plot.py` using only Matplotlib and those local CSVs. Timeline panels show start, acceptance, replay readiness and the shared old-tree drain for every commit; blue/amber/green backgrounds distinguish installation, cleanup and reclamation. Each run directory contains the full generated/admitted/completed timestamps, controller trace and packet outcomes. The complete machine-readable report is [measurements.json](measurements.json).", ""]
    (root / "README.md").write_text("\n".join(lines))
