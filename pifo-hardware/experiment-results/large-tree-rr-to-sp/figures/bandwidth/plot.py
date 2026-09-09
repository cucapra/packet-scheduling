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
TITLE = 'RR → SP: Hann-smoothed output bandwidth (full_transitive, 512-cycle window)'
FLOW_LABELS = {1: 'high priority', 2: 'low priority'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'RR → SP',
  'start': 241,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (38, '#ff7f0e', '--', 'C1 commit accepted'),
              (57, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (283, '#9467bd', ':', 'C1 old-tree-drained'), (57, '#1f77b4', '-', 'C2 start'),
              (88, '#ff7f0e', '--', 'C2 commit accepted'),
              (335, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (283, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 57, '#dbeafe', 'C1: install commit'), (57, 335, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=241  commit accepted=279  ready_for_next_commit=298\n'
           'C2: start=298  commit accepted=329  ready_for_next_commit=576\n'
           'old tree drained=524 (shared by C1/C2)\n'
           'published: install=281, cleanup=561\n'
           'config=27 inst / 40 cycles to publication\n'
           'cleanup=30 inst / 263 cycles to publication (guard wait included)\n'
           'bank replay: install=17, cleanup=15 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=27 inst / 40 cycles to publication\n'
                'cleanup=30 inst / 263 cycles to publication (guard wait included)\n'
                'bank replay: install=17, cleanup=15 cycles'}]

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

# Plot the cached bandwidth values directly: do not smooth them a second time.
flows = sorted(int(key.split("_")[1]) for key in rows[0]
               if key.startswith("flow_") and key.endswith("_link_fraction"))
x = [float(row["time_relative_to_start"]) for row in rows]
fig, axes = plt.subplots(2, 1, figsize=(11, 8), sharex=True, constrained_layout=True)
axes[0].plot(x, [float(row["total_link_fraction"]) for row in rows],
             color="tab:blue", linewidth=2, label="Total bandwidth")
axes[0].axhline(1, color="0.45", linestyle=":", linewidth=1, label="Link capacity")
for index, flow in enumerate(flows):
    axes[1].plot(x, [float(row[f"flow_{flow}_link_fraction"]) for row in rows],
                 color=COLORS[index % len(COLORS)], linewidth=2,
                 label=FLOW_LABELS.get(flow, f"Flow {flow}"))
axes[0].legend(loc="upper right", fontsize=8)
for axis in axes:
    event_lines(axis, PANELS[0])
    axis.set_ylim(bottom=0)
event_legend(axes[1], PANELS[0])
axes[0].set_title(TITLE)
axes[0].set_ylabel("Aggregate bandwidth / link capacity")
axes[1].set_ylabel("Per-flow bandwidth / link capacity")
axes[1].set_xlabel("Time relative to reconfiguration start (cycles)")

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
