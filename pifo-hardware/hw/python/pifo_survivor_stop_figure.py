#!/usr/bin/env python3
"""Figure B: measured global pop stop versus pre-request backlog."""
import json
import numpy as np

from pifo_figures.common import load_pyplot
from pifo_survivor_common import TITLES, COLORS, figure_args, save_figure, settings


def main():
    args = figure_args(__doc__)
    report = json.loads((args.results / "measurements.json").read_text())
    cases = [c for c in report["cases"].values()
             if c["runs"]["link"]["t1"] in settings()["sweep_pre_cycles"]]
    if not cases:
        print("Figure B awaits paired sweep runs")
        return
    plt, _ = load_pyplot()
    fig, ax = plt.subplots(figsize=(7.2, 4.7), constrained_layout=True)
    rows = []
    for run, title in TITLES.items():
        data = [case["runs"][run] for case in cases]
        x = [m["t1_backlog_packets"] for m in data]
        y = [m["global_stop_cycles"] for m in data]
        ax.plot(x, y, "o-", color=COLORS[run], label=title, linewidth=1.8)
        rows += [{"mechanism": run, **{k: m[k] for k in ("t1", "t1_backlog_packets", "global_stop_cycles",
                 "hardware_stop_cycles", "prefill_entries", "prefill_write_cycles", "fixed_stop_overhead_cycles")}} for m in data]
        if run == "reserved" and len(x) > 1:
            slope, intercept = np.polyfit(x, y, 1)
            ax.text(.03, .95, f"Measured fit: {slope:.3f} cycles/packet + {intercept:.1f} cycles",
                    transform=ax.transAxes, va="top", fontsize=9)
    ax.set(xlabel="Measured backlog at t₁ (packets)", ylabel="Global pop stop (cycles)",
           title="Wrapper birth: one link versus N token writes")
    ax.set_ylim(-15, max(r["global_stop_cycles"] for r in rows) * 1.15 + 10)
    ax.legend(loc="center left", fontsize=9)
    ax.grid(alpha=.2)
    save_figure(fig, args.results, "prefill-stop", rows)
    plt.close(fig)


if __name__ == "__main__":
    main()
