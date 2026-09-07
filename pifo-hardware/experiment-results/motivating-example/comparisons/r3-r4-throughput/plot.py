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
  'markers': [(0, '#1f77b4', '-', 'start'), (27, '#ff7f0e', '--', 'commit accepted'),
              (431, '#9467bd', ':', 'old tree drained'),
              (704, '#2ca02c', '-.', 'finish: double-buffer cleanup done')],
  'notes': 'start=2000  commit accepted=2027  drain=2431  finish=2704\n'
           'published: install=2030, cleanup=2447\n'
           'config=17 inst / 30 cycles to publication\n'
           'cleanup=12 inst / 160 cycles to publication (guard wait included)\n'
           'bank cleanup: install=257, cleanup=257 cycles; ≤1 instruction accepted/cycle'},
 {'title': 'R4: confined replace',
  'start': 2000,
  'markers': [(0, '#1f77b4', '-', 'start'), (18, '#ff7f0e', '--', 'commit accepted'),
              (705, '#9467bd', ':', 'old tree drained'),
              (968, '#2ca02c', '-.', 'finish: double-buffer cleanup done')],
  'notes': 'start=2000  commit accepted=2018  drain=2705  finish=2968\n'
           'published: install=2021, cleanup=2711\n'
           'config=10 inst / 21 cycles to publication\n'
           'cleanup=5 inst / 433 cycles to publication (guard wait included)\n'
           'bank cleanup: install=257, cleanup=257 cycles; ≤1 instruction accepted/cycle'}]

with (HERE / 'data.csv').open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")


def event_lines(axis, panel, horizontal=False):
    for cycle, color, style, label in panel["markers"]:
        axis.axvline(cycle, color=color, linestyle=style, linewidth=1.15, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=1.15)
    axis.grid(True, color="0.92", linewidth=0.8)

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
    axis.axhline(1, color="0.55", linewidth=1, linestyle=":")
    axis.set_title(panel["title"])
    axis.set_xlabel("Cycle relative to reconfiguration start")
    axis.text(0.01, 0.01, panel["notes"], transform=axis.transAxes,
              va="bottom", fontsize=7, color="0.35")
axes[0][0].set_ylabel("Output throughput / link capacity")
axes[0][-1].legend(loc="best", fontsize=8)
fig.suptitle(TITLE)

fig.savefig(HERE / 'figure.svg', bbox_inches="tight")
fig.savefig(HERE / 'figure.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
