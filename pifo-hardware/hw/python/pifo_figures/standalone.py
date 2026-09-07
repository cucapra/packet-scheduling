"""Emit small, independent CSV-to-Matplotlib scripts into figure folders.

Only the exporter imports repo helpers. The emitted file contains literal plot
settings and needs just its local CSV plus Matplotlib, even outside this repo.
"""

from __future__ import annotations

from pathlib import Path
from pprint import pformat
from typing import Mapping, Sequence

from pifo_figures.common import (
    COLORS, FigurePaths, PolicyEvent, timeline_markers, timeline_spans, timeline_notes, timing_text,
)


def write_plot_script(
    paths: FigurePaths,
    kind: str,
    panels: Sequence[tuple[str, PolicyEvent]],
    labels: Mapping[int, str],
    dpi: int,
    title: str,
    *,
    data_name: str | None = None,
    packet_trace_name: str = "packets.csv",
) -> Path:
    """Write plot.py; do not run it or modify any existing figure/CSV."""
    panel_settings = []
    for panel_title, event in panels:
        panel_settings.append(dict(title=panel_title, start=event.start_cycle,
                                   markers=timeline_markers(event), spans=timeline_spans(event),
                                   notes=timing_text(event), accounting=timeline_notes(event)))

    settings = {
        "DPI": dpi,
        "TITLE": title,
        "FLOW_LABELS": dict(labels),
        "COLORS": COLORS,
        "PANELS": panel_settings,
    }
    source = '''#!/usr/bin/env python3
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
'''
    source += f"# Raw per-packet trace: {packet_trace_name}; push_cycle is source generation, not admission.\n"
    if packet_trace_name != "packets.csv":
        source += "# Legacy archive: recorded completions only; unavailable packet sizes are blank.\n"
    source += "\n".join(f"{key} = {pformat(value, width=100, compact=True, sort_dicts=False)}" for key, value in settings.items())
    source += f'''\n
with (HERE / {data_name or paths.data.name!r}).open(newline="", encoding="utf-8-sig") as stream:
    rows = list(csv.DictReader(stream))
if not rows:
    raise ValueError("The figure CSV has no data rows")
'''
    source += _MARKERS + _BODIES[kind]
    source += f'''
fig.savefig(HERE / {paths.svg.name!r}, bbox_inches="tight")
fig.savefig(HERE / {paths.png.name!r}, dpi=DPI, bbox_inches="tight")
plt.close(fig)
'''
    name = "plot.py" if paths.svg.stem == "figure" else paths.svg.stem + "-plot.py"
    script = paths.data.parent / name
    script.parent.mkdir(parents=True, exist_ok=True)
    script.write_text(source, encoding="utf-8")
    return script


_MARKERS = '''

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

'''

_BANDWIDTH = '''# Plot the cached bandwidth values directly: do not smooth them a second time.
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
'''

_SCATTER = '''flows = sorted({int(row["flow_id"]) for row in rows})
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
'''

_DELAY = '''flows = sorted({int(row["flow"]) for row in rows})
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
'''

_THROUGHPUT_COMPARISON = '''flows = sorted(int(key.split("_")[1]) for key in rows[0]
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
'''

_BODIES = {"bandwidth": _BANDWIDTH, "packet-scatter": _SCATTER,
           "delay": _DELAY, "throughput-comparison": _THROUGHPUT_COMPARISON}
