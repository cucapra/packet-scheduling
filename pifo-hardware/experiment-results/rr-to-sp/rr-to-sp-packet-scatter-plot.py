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
# Raw per-packet trace: rr-to-sp-packets.csv; push_cycle is source generation, not admission.
# Legacy archive: recorded completions only; unavailable packet sizes are blank.
DPI = 180
TITLE = 'Packet input–output scatter: RR → SP'
FLOW_LABELS = {1: 'A', 2: 'B'}
COLORS = ('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22',
 '#17becf')
PANELS = [{'title': 'RR → SP',
  'start': 320,
  'markers': [(0, '#1f77b4', '-', 'start'), (10, '#ff7f0e', '--', 'commit accepted'),
              (619, '#9467bd', ':', 'old tree drained'),
              (4109, '#2ca02c', '-.', 'finish: double-buffer cleanup done')],
  'notes': 'start=320  commit accepted=330  drain=939  finish=4429\n'
           'config=10 inst / 10 cycles to acceptance; ≤1 instruction accepted/cycle'}]

with (HERE / 'packet-times.csv').open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")


def event_lines(axis, panel, horizontal=False):
    for cycle, color, style, label in panel["markers"]:
        axis.axvline(cycle, color=color, linestyle=style, linewidth=1.15, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=1.15)
    axis.grid(True, color="0.92", linewidth=0.8)

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
axis.legend(loc="best", fontsize=8)
axis.text(0.99, 0.03, PANELS[0]["notes"], transform=axis.transAxes,
          ha="right", va="bottom", fontsize=8, color="0.35")

fig.savefig(HERE / 'rr-to-sp-packet-scatter.svg', bbox_inches="tight")
fig.savefig(HERE / 'rr-to-sp-packet-scatter.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
