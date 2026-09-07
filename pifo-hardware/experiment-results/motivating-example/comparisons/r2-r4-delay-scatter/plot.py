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
  'markers': [(0, '#1f77b4', '-', 'start'), (31, '#ff7f0e', '--', 'commit accepted'),
              (13, '#9467bd', ':', 'old tree captured'),
              (1038, '#2ca02c', '-.', 'traffic resumed (legacy finish)')],
  'notes': 'start=2000  commit accepted=2031  drain=2013  finish=3038\n'
           'config=16 inst / 31 cycles to acceptance; ≤1 instruction accepted/cycle\n'
           'resumed=3038  retained=137  peak buffer=417 packets  stop=1025 cycles'},
 {'title': 'R3: whole-tree replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'start'), (27, '#ff7f0e', '--', 'commit accepted'),
              (431, '#9467bd', ':', 'old tree drained'),
              (287, '#2ca02c', '-.', 'finish: double-buffer cleanup done')],
  'notes': 'start=2000  commit accepted=2027  drain=2431  finish=2287\n'
           'config=17 inst / 27 cycles to acceptance; ≤1 instruction accepted/cycle'},
 {'title': 'R4: confined replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'start'), (18, '#ff7f0e', '--', 'commit accepted'),
              (705, '#9467bd', ':', 'old tree drained'),
              (278, '#2ca02c', '-.', 'finish: double-buffer cleanup done')],
  'notes': 'start=2000  commit accepted=2018  drain=2705  finish=2278\n'
           'config=10 inst / 18 cycles to acceptance; ≤1 instruction accepted/cycle'}]

with (HERE / 'packets.csv').open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")


def event_lines(axis, panel, horizontal=False):
    for cycle, color, style, label in panel["markers"]:
        axis.axvline(cycle, color=color, linestyle=style, linewidth=1.15, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=1.15)
    axis.grid(True, color="0.92", linewidth=0.8)

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
    axis.legend(loc="best", markerscale=1.5, fontsize=8)
    axis.text(0.01, 0.01, panel["notes"], transform=axis.transAxes,
              va="bottom", fontsize=7, color="0.35")
axes[0][0].set_ylabel("Per-packet delay (pop − generation cycles)")
if len(PANELS) > 1:
    fig.suptitle(TITLE)

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
