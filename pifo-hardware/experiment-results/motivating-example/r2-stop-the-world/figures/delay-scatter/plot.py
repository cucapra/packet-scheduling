#!/usr/bin/env python3
"""Replot this folder's CSV: python plot.py. Requires only matplotlib.

All labels, timestamps and styling are local below; no repository or style files
are loaded. Bandwidth CSVs contain the already sampled/smoothed measurements.
"""
import csv
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
from matplotlib import pyplot as plt
plt.rcdefaults()

HERE = Path(__file__).resolve().parent
# Edit these local settings to customize this figure.
# Raw per-packet trace: packets.csv; push_cycle is source generation, not admission.
DPI = 180
TITLE = 'R2: stop the world'
FLOW_LABELS = {1: 'zoom', 2: 'gmail', 3: 'spotify'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'R2: stop the world',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (40, '#ff7f0e', '--', 'C1 commit accepted'),
              (60, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (12, '#9467bd', ':', 'C1 old-tree captured (not drained)'),
              (1036, '#1f77b4', '-', 'C2 start'), (1036, '#ff7f0e', '--', 'C2 commit accepted'),
              (1040, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (12, '#9467bd', ':', 'C2 old-tree captured (not drained)'),
              (1036, '0.4', '--', 'traffic resumed')],
  'spans': [(0, 60, '#dbeafe', 'C1: install commit'),
            (1036, 1040, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2040  ready_for_next_commit=2060\n'
           'C2: start=3036  commit accepted=3036  ready_for_next_commit=3040\n'
           'old tree captured=2012 (shared by C1/C2)\n'
           'published: install=2043, cleanup=3040\n'
           'config=25 inst / 43 cycles to publication\n'
           'cleanup=1 inst / 4 cycles to publication (guard wait included)\n'
           'bank replay: install=17, cleanup=0 cycles; ≤1 instruction accepted/cycle\n'
           'resumed=3036  retained=137  peak buffer=416 packets  stop=1024 cycles',
  'accounting': 'config=25 inst / 43 cycles to publication\n'
                'cleanup=1 inst / 4 cycles to publication (guard wait included)\n'
                'bank replay: install=17, cleanup=0 cycles\n'
                'STW stop=1024 cycles; retained=137; peak buffer=416 packets'}]

with (HERE / 'data.csv').open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")


def event_lines(axis, panel, horizontal=False):
    for start, ready, color, label in panel["spans"]:
        axis.axvspan(start, ready, color=color, alpha=0.65, linewidth=0, zorder=0, label=label)
        if horizontal:
            axis.axhspan(start, ready, color=color, alpha=0.35, linewidth=0, zorder=0)
    for cycle, color, style, label in panel["markers"]:
        width = 2.3 if "ready_for_next_commit" in label else 1.15
        axis.axvline(cycle, color=color, linestyle=style, linewidth=width, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=width, gid="commit-time-y")
    axis.grid(True, color="0.92", linewidth=0.8)


def event_legend(axis, panel):
    handles, labels = axis.get_legend_handles_labels()
    indexed = dict(zip(labels, handles))
    entries = []
    for _, _, _, title in panel["spans"]:
        name = title.split(":")[0]
        entries.append((indexed[title], title))
        entries.extend((indexed[label], f"{label} = {cycle + panel['start']}")
                       for cycle, _, _, label in panel["markers"] if label.startswith(name + " "))
    resumes = {label: cycle + panel["start"] for cycle, _, _, label in panel["markers"]
               if label == "traffic resumed"}
    data = [(handle, f"{label} = {resumes[label]}" if label in resumes else label)
            for handle, label in zip(handles, labels)
            if not label.startswith(tuple(title.split(":")[0] for _, _, _, title in panel["spans"]))]
    if data:
        data_legend = axis.legend(*zip(*data), loc="upper right", fontsize=8)
        axis.add_artist(data_legend)
    key = axis.legend(*zip(*entries), loc="upper center", bbox_to_anchor=(0.5, -0.16),
                      ncol=len(panel["spans"]), fontsize=7.5)
    axis.annotate(panel["accounting"], xy=(0.5, 0), xycoords=key, xytext=(0, -6),
                  textcoords="offset points", ha="center", va="top", fontsize=7,
                  color="0.35", annotation_clip=False)

flows = sorted({int(row["flow"]) for row in rows})
size = (10, 6.5) if len(PANELS) == 1 else (6 * len(PANELS), 5.8)
fig, axes = plt.subplots(1, len(PANELS), figsize=size, sharex=True, sharey=True,
                         squeeze=False, constrained_layout=True)
for axis, panel in zip(axes[0], PANELS):
    selected = rows if len(PANELS) == 1 else [row for row in rows if row["run"] == panel["title"]]
    for index, flow in enumerate(flows):
        color = COLORS[index % len(COLORS)]
        packets = [row for row in selected if int(row["flow"]) == flow]
        completed = [row for row in packets if row["dropped"].lower() == "false"]
        axis.scatter([int(row["push_cycle"]) - panel["start"] for row in completed],
                     [int(row["pop_cycle"]) - int(row["push_cycle"]) for row in completed],
                     s=10, alpha=0.55, color=color, label=FLOW_LABELS.get(flow, f"Flow {flow}"))
        dropped = [row for row in packets if row["dropped"].lower() == "true"]
        if dropped:
            axis.scatter([int(row["push_cycle"]) - panel["start"] for row in dropped],
                         [0] * len(dropped), color=color, marker="x", s=20, label="dropped (y=0)")
    event_lines(axis, panel)
    axis.axhline(0, color="0.45", linewidth=1, linestyle=":")
    axis.margins(x=0.02, y=0.05)
    axis.set_title(panel["title"])
    axis.set_xlabel("Generation cycle relative to reconfiguration start")
    event_legend(axis, panel)
axes[0][0].set_ylabel("Per-packet delay (pop − generation cycles)")
if len(PANELS) > 1:
    fig.suptitle(TITLE)

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
