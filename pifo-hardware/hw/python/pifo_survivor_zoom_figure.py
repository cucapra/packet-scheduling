#!/usr/bin/env python3
"""Figure A: zoom delay for Strict*, reserved-PE and copy/prefill wrappers."""
import json

from pifo_figures.evaluation import commit_rows, export, packet_rows
from pifo_survivor_common import TITLES, figure_args, settings


def main():
    args, cfg = figure_args(__doc__), settings()
    t1 = cfg["figure_a_pre_cycles"]
    report = json.loads((args.results / "measurements.json").read_text())
    case = report["cases"].get(f"pre-{t1}")
    if case is None:
        print(f"Figure A awaits link/reserved/copy pre-{t1} runs")
        return
    paths = {run: args.results / f"pre-{t1}" / run for run in TITLES}
    packets = packet_rows(paths, cfg["flow_labels"])
    end = max(int(e["install_finish_cycle"]) for m in case["runs"].values() for e in m["events"])
    # Show recovery after the longer copy outage as well as the drain markers.
    end = max(end, max(m["old_root_drained_cycle"] for m in case["runs"].values()))
    end += 700 + max(m["global_stop_cycles"] for m in case["runs"].values())
    data = [p for p in packets if p["flow"] == "1" and max(0, t1 - 500) <= int(p["push_cycle"]) <= end]
    export(args.results / "figures/zoom-delay", "zoom", data, packets, commit_rows(paths),
           {"runs": list(TITLES), "titles": TITLES,
            "title": f"zoom: same whole-tree transition, {case['runs']['link']['t1_backlog_packets']} packets at t₁"})


if __name__ == "__main__":
    main()
