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
TITLE = 'RR → SP: saved bandwidth samples'
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

with (HERE / 'bandwidth.csv').open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")


def event_lines(axis, panel, horizontal=False):
    for cycle, color, style, label in panel["markers"]:
        axis.axvline(cycle, color=color, linestyle=style, linewidth=1.15, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=1.15)
    axis.grid(True, color="0.92", linewidth=0.8)

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
for axis in axes:
    event_lines(axis, PANELS[0])
    axis.set_ylim(bottom=0)
    axis.legend(loc="best", fontsize=8)
axes[0].set_title(TITLE)
axes[0].set_ylabel("Aggregate bandwidth / link capacity")
axes[1].set_ylabel("Per-flow bandwidth / link capacity")
axes[1].set_xlabel("Time relative to reconfiguration start (cycles)")
axes[0].text(0.99, 0.03, PANELS[0]["notes"], transform=axes[0].transAxes,
             ha="right", va="bottom", fontsize=8, color="0.35")

fig.savefig(HERE / 'rr-to-sp-bandwidth.svg', bbox_inches="tight")
fig.savefig(HERE / 'rr-to-sp-bandwidth.png', dpi=DPI, bbox_inches="tight")
plt.close(fig)
