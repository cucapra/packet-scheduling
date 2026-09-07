#!/usr/bin/env python3
"""Figure B: measured global pop stop versus pre-request backlog."""
import json

from pifo_figures.evaluation import commit_rows, export, packet_rows
from pifo_survivor_common import TITLES, figure_args, settings


def main():
    args, cfg = figure_args(__doc__), settings()
    report = json.loads((args.results / "measurements.json").read_text())
    cases = {name: c for name, c in report["cases"].items()
             if c["runs"]["link"]["t1"] in cfg["sweep_pre_cycles"]}
    if not cases:
        print("Figure B awaits paired sweep runs")
        return
    fields = ("t1", "t1_backlog_packets", "global_stop_cycles", "hardware_stop_cycles",
              "prefill_entries", "prefill_write_cycles", "fixed_stop_overhead_cycles")
    rows = [{"run": run, **{k: m[k] for k in fields}}
            for c in cases.values() for run, m in c["runs"].items()]
    paths = {f"{case}/{run}": args.results / case / run for case in cases for run in TITLES}
    export(args.results / "figures/prefill-stop", "stop", rows,
           packet_rows(paths, cfg["flow_labels"]), commit_rows(paths),
           {"runs": list(TITLES), "titles": TITLES})


if __name__ == "__main__":
    main()
