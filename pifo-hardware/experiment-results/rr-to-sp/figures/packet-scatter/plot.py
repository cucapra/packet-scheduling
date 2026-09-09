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
TITLE = 'Packet input–output scatter: RR → SP (full_transitive)'
FLOW_LABELS = {1: 'A', 2: 'B'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'RR → SP',
  'start': 600,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (19, '#ff7f0e', '--', 'C1 commit accepted'),
              (26, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (299, '#9467bd', ':', 'C1 old-tree-drained'), (26, '#1f77b4', '-', 'C2 start'),
              (33, '#ff7f0e', '--', 'C2 commit accepted'),
              (309, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (299, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 26, '#dbeafe', 'C1: install commit'), (26, 309, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=600  commit accepted=619  ready_for_next_commit=626\n'
           'C2: start=626  commit accepted=633  ready_for_next_commit=909\n'
           'old tree drained=899 (shared by C1/C2)\n'
           'published: install=621, cleanup=906\n'
           'config=9 inst / 21 cycles to publication\n'
           'cleanup=6 inst / 280 cycles to publication (guard wait included)\n'
           'bank replay: install=5, cleanup=3 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=9 inst / 21 cycles to publication\n'
                'cleanup=6 inst / 280 cycles to publication (guard wait included)\n'
                'bank replay: install=5, cleanup=3 cycles'}]

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

flows = sorted({int(row["flow_id"]) for row in rows})
fig, axis = plt.subplots(figsize=(8, 8), constrained_layout=True)
values = [cycle for cycle, *_ in PANELS[0]["markers"]]
for index, flow in enumerate(flows):
    selected = [row for row in rows if int(row["flow_id"]) == flow]
    x = [float(row["input_relative_to_start"]) for row in selected]
    y = [float(row["output_relative_to_start"]) for row in selected]
    values.extend(x + y)
    axis.scatter(x, y, s=30, alpha=0.75, edgecolors="none",
                 color=COLORS[index % len(COLORS)], label=FLOW_LABELS.get(flow, f"Flow {flow}"))
padding = max(1, (max(values) - min(values)) * 0.04)
limits = (min(values) - padding, max(values) + padding)
axis.plot(limits, limits, color="tab:blue", linewidth=1, alpha=0.45, label="y = x")
event_lines(axis, PANELS[0], horizontal=True)
axis.set(xlim=limits, ylim=limits, title=TITLE,
         xlabel="Packet input time relative to reconfiguration start (cycles)",
         ylabel="Packet output time relative to reconfiguration start (cycles)")
axis.set_aspect("equal", adjustable="box")
event_legend(axis, PANELS[0])

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
