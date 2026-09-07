#!/usr/bin/env python3
"""Figure B: untouched-flow packet delays, shared axes and p1 control overlays."""
from pifo_figures.common import COLORS, load_pyplot
from pifo_multiedit_common import (
    COPY_RUNS, MAIN_RUNS, TITLES, events, figure_args, figure_dir, packets, settings, write_csv, save_figure,
)


def main():
    args = figure_args(__doc__)
    path = figure_dir(args, "untouched-delay")
    runs = COPY_RUNS if args.copy_comparison else MAIN_RUNS
    cfg = settings()
    plt, Line2D = load_pyplot()
    fig, axes = plt.subplots(1, len(runs), figsize=(5 * len(runs), 4.6),
                             sharex=True, sharey=True, constrained_layout=True)
    control = packets(args.results, "control")
    rows = []
    maxdelay = 0
    for axis, run in zip(axes, runs):
        data = packets(args.results, run)
        for index, flow in enumerate(cfg["untouched_flows"]):
            for reference, collection in ((True, control), (False, data)):
                values = sorted([r for r in collection if r["flow"] == flow and r["pop_cycle"] is not None],
                                key=lambda r: (r["push_cycle"], r["request_id"]))
                x = [r["push_cycle"] for r in values]
                y = [r["pop_cycle"] - r["push_cycle"] for r in values]
                maxdelay = max(maxdelay, max(y))
                if reference:
                    axis.plot(x, y, color=COLORS[index], alpha=.65, linestyle="--", linewidth=.8)
                else:
                    axis.scatter(x, y, color=COLORS[index], alpha=.65, s=7, linewidths=0,
                                 label=cfg["flow_labels"][str(flow)])
                rows += [{"run": run, "source": "control" if reference else run,
                          "flow": cfg["flow_labels"][str(flow)], "push_cycle": a,
                          "delay_cycles": b} for a, b in zip(x, y)]
        for event in events(args.results, run)[:1]:
            timing_labels = []
            for key, color, style in (("start_cycle", "black", ":"), ("commit_cycle", "#6f42a8", "--"),
                                      ("drain_cycle", "#20895b", "-."), ("finish_cycle", "#777777", ":")):
                if event[key]:
                    axis.axvline(int(event[key]), color=color, linestyle=style, linewidth=.8, alpha=.7)
                    label = {"start_cycle": "start", "commit_cycle": "commit accepted", "drain_cycle": "captured" if run == "reset" else "old empty",
                             "finish_cycle": "config finished"}[key]
                    timing_labels.append(f"{label}: {event[key]}")
            axis.text(.98, .97, "\n".join(timing_labels), transform=axis.transAxes, ha="right", va="top",
                      fontsize=7, bbox={"facecolor": "white", "edgecolor": "none", "alpha": .75})
        axis.set_title(TITLES[run], fontsize=10)
        axis.set_xlabel("Packet generation cycle")
        axis.grid(alpha=.15)
        axis.set_xlim(0, cfg.get("plot_end_cycle", cfg["end_cycle"]))
    axes[0].set_ylabel("Packet delay: pop − generation (cycles)")
    axes[0].set_ylim(-maxdelay * .025, maxdelay * 1.06)
    handles, labels = axes[0].get_legend_handles_labels()
    handles += [Line2D([], [], color="grey", linestyle="--", label="p1 control")]
    fig.legend(handles=handles, loc="outside upper center", ncol=5, fontsize=9)
    fig.supxlabel(f"Transition/recovery detail; full {cfg['end_cycle']:,}-cycle traffic trace supplied in CSV", fontsize=8)
    save_figure(fig, path)
    write_csv(path / "data.csv", rows, ["run", "source", "flow", "push_cycle", "delay_cycles"])
    plt.close(fig)


if __name__ == "__main__":
    main()
