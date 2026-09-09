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
TITLE = 'R3 whole-tree vs R4 confined throughput (240-cycle Hann window)'
FLOW_LABELS = {1: 'zoom', 2: 'gmail', 3: 'spotify'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'R3: whole-tree replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (32, '#ff7f0e', '--', 'C1 commit accepted'),
              (52, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (431, '#9467bd', ':', 'C1 old-tree-drained'), (52, '#1f77b4', '-', 'C2 start'),
              (68, '#ff7f0e', '--', 'C2 commit accepted'),
              (457, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (431, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 52, '#dbeafe', 'C1: install commit'), (52, 457, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2032  ready_for_next_commit=2052\n'
           'C2: start=2052  commit accepted=2068  ready_for_next_commit=2457\n'
           'old tree drained=2431 (shared by C1/C2)\n'
           'published: install=2035, cleanup=2450\n'
           'config=26 inst / 35 cycles to publication\n'
           'cleanup=16 inst / 398 cycles to publication (guard wait included)\n'
           'bank replay: install=17, cleanup=7 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=26 inst / 35 cycles to publication\n'
                'cleanup=16 inst / 398 cycles to publication (guard wait included)\n'
                'bank replay: install=17, cleanup=7 cycles'},
 {'title': 'R4: confined replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (26, '#ff7f0e', '--', 'C1 commit accepted'),
              (39, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (696, '#9467bd', ':', 'C1 old-tree-drained'), (39, '#1f77b4', '-', 'C2 start'),
              (44, '#ff7f0e', '--', 'C2 commit accepted'),
              (704, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (696, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 39, '#dbeafe', 'C1: install commit'), (39, 704, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2026  ready_for_next_commit=2039\n'
           'C2: start=2039  commit accepted=2044  ready_for_next_commit=2704\n'
           'old tree drained=2696 (shared by C1/C2)\n'
           'published: install=2028, cleanup=2702\n'
           'config=16 inst / 28 cycles to publication\n'
           'cleanup=5 inst / 663 cycles to publication (guard wait included)\n'
           'bank replay: install=11, cleanup=2 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=16 inst / 28 cycles to publication\n'
                'cleanup=5 inst / 663 cycles to publication (guard wait included)\n'
                'bank replay: install=11, cleanup=2 cycles'}]

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

flows = sorted(int(key.split("_")[1]) for key in rows[0]
               if key.startswith("flow_") and key.endswith("_link_fraction"))
fig, axes = plt.subplots(1, len(PANELS), figsize=(7.5 * len(PANELS), 5.8),
                         sharex=True, sharey=True, squeeze=False, constrained_layout=True)
for axis, panel in zip(axes[0], PANELS):
    selected = [row for row in rows if row["run"] == panel["title"]]
    x = [float(row["time_relative_to_start"]) for row in selected]
    axis.plot(x, [float(row["total_link_fraction"]) for row in selected],
              color="black", linewidth=2.2, label="total")
    for index, flow in enumerate(flows):
        axis.plot(x, [float(row[f"flow_{flow}_link_fraction"]) for row in selected],
                  color=COLORS[index % len(COLORS)], linewidth=1.8,
                  label=FLOW_LABELS.get(flow, f"Flow {flow}"))
    event_lines(axis, panel)
    event_legend(axis, panel)
    axis.axhline(1, color="0.55", linewidth=1, linestyle=":")
    axis.set_title(panel["title"])
    axis.set_xlabel("Cycle relative to reconfiguration start")
axes[0][0].set_ylabel("Output throughput / link capacity")
fig.suptitle(TITLE)

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
