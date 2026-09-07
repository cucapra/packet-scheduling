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
TITLE = 'R2–R4 packet-delay comparison (shared axes)'
FLOW_LABELS = {1: 'zoom', 2: 'gmail', 3: 'spotify'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'R2: stop the world',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (31, '#ff7f0e', '--', 'C1 commit accepted'),
              (44, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (13, '#9467bd', ':', 'C1 old-tree captured (not drained)'),
              (1037, '#1f77b4', '-', 'C2 start'), (1038, '#ff7f0e', '--', 'C2 commit accepted'),
              (1041, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (13, '#9467bd', ':', 'C2 old-tree captured (not drained)'),
              (1037, '0.4', '--', 'traffic resumed')],
  'spans': [(0, 44, '#dbeafe', 'C1: install commit'),
            (1037, 1041, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2031  ready_for_next_commit=2044\n'
           'C2: start=3037  commit accepted=3038  ready_for_next_commit=3041\n'
           'old tree captured=2013 (shared by C1/C2)\n'
           'published: install=2034, cleanup=3040\n'
           'config=16 inst / 34 cycles to publication\n'
           'cleanup=1 inst / 3 cycles to publication (guard wait included)\n'
           'bank replay: install=10, cleanup=1 cycles; ≤1 instruction accepted/cycle\n'
           'resumed=3037  retained=137  peak buffer=417 packets  stop=1024 cycles',
  'accounting': 'config=16 inst / 34 cycles to publication\n'
                'cleanup=1 inst / 3 cycles to publication (guard wait included)\n'
                'bank replay: install=10, cleanup=1 cycles\n'
                'STW stop=1024 cycles; retained=137; peak buffer=417 packets'},
 {'title': 'R3: whole-tree replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (27, '#ff7f0e', '--', 'C1 commit accepted'),
              (41, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (431, '#9467bd', ':', 'C1 old-tree-drained'), (41, '#1f77b4', '-', 'C2 start'),
              (54, '#ff7f0e', '--', 'C2 commit accepted'),
              (452, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (431, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 41, '#dbeafe', 'C1: install commit'), (41, 452, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2027  ready_for_next_commit=2041\n'
           'C2: start=2041  commit accepted=2054  ready_for_next_commit=2452\n'
           'old tree drained=2431 (shared by C1/C2)\n'
           'published: install=2030, cleanup=2447\n'
           'config=17 inst / 30 cycles to publication\n'
           'cleanup=12 inst / 406 cycles to publication (guard wait included)\n'
           'bank replay: install=11, cleanup=5 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=17 inst / 30 cycles to publication\n'
                'cleanup=12 inst / 406 cycles to publication (guard wait included)\n'
                'bank replay: install=11, cleanup=5 cycles'},
 {'title': 'R4: confined replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'C1 start'), (18, '#ff7f0e', '--', 'C1 commit accepted'),
              (28, '#2ca02c', '-.', 'C1 ready_for_next_commit'),
              (705, '#9467bd', ':', 'C1 old-tree-drained'), (28, '#1f77b4', '-', 'C2 start'),
              (34, '#ff7f0e', '--', 'C2 commit accepted'),
              (713, '#2ca02c', '-.', 'C2 ready_for_next_commit'),
              (705, '#9467bd', ':', 'C2 old-tree-drained')],
  'spans': [(0, 28, '#dbeafe', 'C1: install commit'), (28, 713, '#ffedd5', 'C2: cleanup commit')],
  'notes': 'C1: start=2000  commit accepted=2018  ready_for_next_commit=2028\n'
           'C2: start=2028  commit accepted=2034  ready_for_next_commit=2713\n'
           'old tree drained=2705 (shared by C1/C2)\n'
           'published: install=2021, cleanup=2711\n'
           'config=10 inst / 21 cycles to publication\n'
           'cleanup=5 inst / 683 cycles to publication (guard wait included)\n'
           'bank replay: install=7, cleanup=2 cycles; ≤1 instruction accepted/cycle',
  'accounting': 'config=10 inst / 21 cycles to publication\n'
                'cleanup=5 inst / 683 cycles to publication (guard wait included)\n'
                'bank replay: install=7, cleanup=2 cycles'}]

with (HERE / 'packets.csv').open(newline="", encoding="utf-8-sig") as stream:
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
            for handle, label in zip(handles, labels) if not label.startswith(("C1", "C2"))]
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
