#!/usr/bin/env python3
"""Figure A: first service of the four admitted flows, relative to t1."""
import json

from pifo_figures.common import load_pyplot
from pifo_multiedit_common import (
    COPY_RUNS, MAIN_RUNS, TITLES, figure_args, figure_dir, settings, write_csv, save_figure,
)


def main():
    args = figure_args(__doc__)
    path = figure_dir(args, "first-service")
    runs = COPY_RUNS if args.copy_comparison else MAIN_RUNS
    report = json.loads((args.results / "measurements.json").read_text())
    cfg = settings()
    names = [cfg["flow_labels"][str(flow)] for flow in cfg["arriving_flows"]]
    plt, _ = load_pyplot()
    fig, ax = plt.subplots(figsize=(9, 4.8), constrained_layout=True)
    width = .8 / len(runs)
    rows = []
    for i, run in enumerate(runs):
        values = report["runs"][run]["first_service_cycles_from_t1"]
        bars = ax.bar([x - .4 + (i + .5) * width for x in range(len(names))],
                      [values[name] for name in names], width, label=TITLES[run])
        ax.bar_label(bars, fontsize=8, padding=3)
        rows += [{"run": run, "flow": name, "first_service_cycles_from_t1": values[name]} for name in names]
    ax.set_xticks(range(len(names)), names)
    ax.set_ylabel("First service − request time (cycles)")
    ax.set_ylim(0, ax.get_ylim()[1] * 1.15)
    ax.grid(axis="y", alpha=.2)
    ax.set_axisbelow(True)
    ax.legend(fontsize=8, loc="upper left")
    ax.set_title("One request: admitting media and gaming")
    save_figure(fig, path)
    write_csv(path / "data.csv", rows, ["run", "flow", "first_service_cycles_from_t1"])
    plt.close(fig)


if __name__ == "__main__":
    main()
