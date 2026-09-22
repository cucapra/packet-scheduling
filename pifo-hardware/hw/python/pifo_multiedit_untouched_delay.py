#!/usr/bin/env python3
"""Figure B: untouched-flow delays with steady-p1 and steady-p2 controls."""
from pifo_figures.evaluation import commit_rows, export, packet_rows
from pifo_multiedit_common import CONTROL_RUNS, COPY_RUNS, MAIN_RUNS, TITLES, figure_args, figure_dir, settings


def main():
    args, cfg = figure_args(__doc__), settings()
    runs = COPY_RUNS if args.copy_comparison else MAIN_RUNS
    paths = {run: args.results / run for run in (*CONTROL_RUNS, *runs)}
    packets = packet_rows(paths, cfg["flow_labels"])
    selected = {str(flow) for flow in cfg["untouched_flows"]}
    rows = [{**p, "source": p["run"], "run": run} for run in runs
            for p in sorted(packets, key=lambda p: (int(p["push_cycle"]), int(p["request_id"])))
            if p["run"] in (*CONTROL_RUNS, run) and p["flow"] in selected and p["pop_cycle"]]
    export(figure_dir(args, "untouched-delay"), "untouched", rows, packets, commit_rows(paths),
           {"runs": runs, "flows": [cfg["flow_labels"][str(f)] for f in cfg["untouched_flows"]],
            "controls": CONTROL_RUNS, "titles": TITLES,
            "xlim": (0, cfg.get("plot_end_cycle", cfg["end_cycle"]))})


if __name__ == "__main__":
    main()
