#!/usr/bin/env python3
"""Figure A: first service of the four admitted flows, relative to t1."""
import json

from pifo_figures.evaluation import commit_rows, export, packet_rows
from pifo_multiedit_common import COPY_RUNS, FIRST_SERVICE_RUNS, TITLES, figure_args, figure_dir, settings


def main():
    args, cfg = figure_args(__doc__), settings()
    runs = COPY_RUNS if args.copy_comparison else FIRST_SERVICE_RUNS
    report = json.loads((args.results / "measurements.json").read_text())
    names = [cfg["flow_labels"][str(flow)] for flow in cfg["arriving_flows"]]
    rows = [{"run": run, "flow": name,
             "first_service_cycles_from_t1": report["runs"][run]["first_service_cycles_from_t1"][name]}
            for run in runs for name in names]
    paths = {run: args.results / run for run in runs}
    export(figure_dir(args, "first-service"), "first", rows,
           packet_rows(paths, cfg["flow_labels"]), commit_rows(paths),
           {"runs": runs, "flows": names, "titles": TITLES})


if __name__ == "__main__":
    main()
