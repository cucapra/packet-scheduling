#!/usr/bin/env python3
"""Figure A: zoom delay with Strict* and a reserved-PE materialized wrapper."""
import json

from pifo_figures.common import load_pyplot
from pifo_survivor_common import TITLES, COLORS, figure_args, save_figure, settings, validate_packets


def main():
    args = figure_args(__doc__)
    t1 = settings()["figure_a_pre_cycles"]
    report = json.loads((args.results / "measurements.json").read_text())
    case = report["cases"].get(f"pre-{t1}")
    if case is None:
        print(f"Figure A awaits paired pre-{t1} runs")
        return
    runs = case["runs"]
    end = max(m["old_root_drained_cycle"] for m in runs.values()) + 700
    plt, _ = load_pyplot()
    fig, ax = plt.subplots(figsize=(8.3, 4.6), constrained_layout=True)
    rows = []
    for run, title in TITLES.items():
        data = [r for r in validate_packets(args.results / f"pre-{t1}" / run)
                if r["flow"] == 1 and max(0, t1 - 500) <= r["push_cycle"] <= end]
        ax.scatter([r["push_cycle"] for r in data], [r["pop_cycle"] - r["push_cycle"] for r in data],
                   s=10, alpha=.7, color=COLORS[run], label=f"zoom — {title}")
        drain = runs[run]["old_root_drained_cycle"]
        ax.axvline(drain, color=COLORS[run], linestyle=":", linewidth=1)
        ax.text(drain - 10, .98, f"{'Strict*' if run == 'link' else 'Wrapper'}: old root empty",
                transform=ax.get_xaxis_transform(), rotation=90, ha="right", va="top", fontsize=8, color=COLORS[run])
        rows += [{"mechanism": run, "flow": "zoom", "request_id": r["request_id"],
                  "push_cycle": r["push_cycle"], "pop_cycle": r["pop_cycle"],
                  "delay_cycles": r["pop_cycle"] - r["push_cycle"]} for r in data]
    stopped = runs["reserved"]["lifecycle"]
    start, resume = stopped["driver_stop"][0]["cycle"], stopped["driver_resume"][0]["cycle"]
    ax.axvspan(start, resume, color=COLORS["reserved"], alpha=.08)
    ax.text((start + resume) / 2, .25, f"Wrapper stop\n{resume - start} cycles",
            transform=ax.get_xaxis_transform(), ha="center", fontsize=8, color=COLORS["reserved"])
    ax.axvline(t1, color=".35", linestyle="--", linewidth=1)
    ax.set(xlabel="Packet generation / push cycle", ylabel="Per-packet delay (cycles)")
    ax.set_title(f"zoom: same whole-tree transition, {runs['link']['t1_backlog_packets']} packets at t₁")
    ax.legend(fontsize=8, loc="upper right")
    ax.grid(alpha=.2)
    ax.margins(x=.01, y=.06)
    save_figure(fig, args.results, "zoom-delay", rows)
    plt.close(fig)


if __name__ == "__main__":
    main()
