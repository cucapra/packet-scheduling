"""Packet input/output scatter data and rendering."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path
from typing import Iterable, Mapping, Sequence

from pifo_figures.common import (
    COLORS,
    COMMIT_COLOR,
    DRAIN_COLOR,
    FINISH_COLOR,
    START_COLOR,
    FigureInputs,
    FigurePaths,
    PacketLike,
    PacketTiming,
    PlotArea,
    PolicyEvent,
    Svg,
    add_common_arguments,
    draw_axes,
    event_label,
    figure_paths,
    flow_name,
    legend,
    load_figure_inputs,
    load_pyplot,
    nice_ticks,
    rasterize_svg,
    scatter_output_markers,
    select_renderer,
    transition_markers,
)


def write_data(
    path: Path, packets: Iterable[PacketTiming], event: PolicyEvent
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(
            (
                "request_id",
                "flow_id",
                "input_cycle",
                "output_cycle",
                "input_relative_to_start",
                "output_relative_to_start",
            )
        )
        for packet in sorted(packets, key=lambda item: item.request_id):
            writer.writerow(
                (
                    packet.request_id,
                    packet.flow_id,
                    packet.input_cycle,
                    packet.output_cycle,
                    packet.input_cycle - event.start_cycle,
                    packet.output_cycle - event.start_cycle,
                )
            )


def render_matplotlib(
    paths: FigurePaths,
    packets: Sequence[PacketTiming],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    plt, Line2D = load_pyplot()
    colors = plt.get_cmap("tab10")
    flow_ids = sorted({packet.flow_id for packet in packets})
    transition_commit = event.commit_cycle - event.start_cycle
    transition_finish = event.finish_cycle - event.start_cycle
    transition_drain = (
        event.drain_cycle - event.start_cycle
        if event.drain_cycle is not None
        else None
    )

    figure, axis = plt.subplots(figsize=(8, 8), constrained_layout=True)
    all_values: list[int] = []
    input_values: list[int] = []
    for index, flow_id in enumerate(flow_ids):
        flow_packets = [packet for packet in packets if packet.flow_id == flow_id]
        inputs = [packet.input_cycle - event.start_cycle for packet in flow_packets]
        outputs = [packet.output_cycle - event.start_cycle for packet in flow_packets]
        input_values.extend(inputs)
        all_values.extend(inputs)
        all_values.extend(outputs)
        axis.scatter(
            inputs,
            outputs,
            s=30,
            alpha=0.75,
            color=colors(index % 10),
            edgecolors="none",
            label=flow_name(flow_id, labels),
        )

    marker_values = [0, transition_commit]
    if transition_drain is not None:
        marker_values.append(transition_drain)
    common_min = min(*all_values, *marker_values)
    common_max = max(*all_values, *marker_values)
    common_padding = max(1.0, (common_max - common_min) * 0.04)
    plot_min = common_min - common_padding
    plot_max = common_max + common_padding
    axis.plot(
        [plot_min, plot_max],
        [plot_min, plot_max],
        color="tab:blue",
        linewidth=1,
        alpha=0.45,
        label="y = x",
    )
    if transition_commit > 0:
        axis.axvspan(0, transition_commit, color="tab:blue", alpha=0.08)
    if transition_drain is not None and transition_drain > transition_commit:
        axis.axvspan(
            transition_commit,
            transition_drain,
            color="tab:purple",
            alpha=0.05,
        )
    axis.axvline(0, color="tab:blue", linewidth=1.5)
    axis.axhline(0, color="tab:blue", linewidth=1.5)
    axis.axvline(
        transition_commit,
        color="tab:orange",
        linewidth=1.3,
        linestyle="--",
    )
    axis.axhline(
        transition_commit,
        color="tab:orange",
        linewidth=1.3,
        linestyle="--",
    )
    visible_input = [*input_values, 0, transition_commit]
    if transition_drain is not None:
        visible_input.append(transition_drain)
    show_finish = min(visible_input) <= transition_finish <= max(visible_input)
    axis.set_xlim(plot_min, plot_max)
    axis.set_ylim(plot_min, plot_max)
    axis.set_aspect("equal", adjustable="box")
    if show_finish:
        axis.axvline(
            transition_finish,
            color="tab:green",
            linewidth=1.2,
            alpha=0.8,
        )
    if transition_drain is not None:
        axis.axvline(
            transition_drain,
            color="tab:purple",
            linewidth=1.3,
            linestyle=":",
        )
        axis.axhline(
            transition_drain,
            color="tab:purple",
            linewidth=1.3,
            linestyle=":",
        )
    axis.grid(True, color="0.92", linewidth=0.8)
    axis.set_title(f"Packet input–output scatter: {event.label} ({event.mode})")
    axis.set_xlabel("Packet input time relative to reconfiguration start (cycles)")
    axis.set_ylabel("Packet output time relative to reconfiguration start (cycles)")
    _add_matplotlib_legend(axis, Line2D, event, show_finish)
    axis.text(
        0.99,
        0.03,
        _timing_text(event, unicode_limit=True),
        transform=axis.transAxes,
        ha="right",
        va="bottom",
        fontsize="small",
        color="0.35",
    )
    figure.savefig(paths.svg, bbox_inches="tight")
    figure.savefig(paths.png, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def _add_matplotlib_legend(axis, line_type, event: PolicyEvent, show_finish: bool) -> None:
    handles, labels = axis.get_legend_handles_labels()
    handles.extend(
        [
            line_type([0], [0], color="tab:blue", linewidth=1.5),
            line_type([0], [0], color="tab:orange", linewidth=1.3, linestyle="--"),
        ]
    )
    labels.extend(("start", "commit accepted"))
    if show_finish:
        handles.append(
            line_type([0], [0], color="tab:green", linewidth=1.2, alpha=0.8)
        )
        labels.append("finish")
    if event.drain_cycle is not None:
        handles.append(
            line_type([0], [0], color="tab:purple", linewidth=1.3, linestyle=":")
        )
        labels.append("old tree drained")
    axis.legend(handles, labels, loc="best")


def render_svg(
    path: Path,
    packets: Sequence[PacketLike],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    width = max(1050, round(8 * dpi))
    height = width
    scale = width / 1440
    font = max(15.0, 22 * scale)
    svg = Svg(width, height)
    flow_ids = sorted({packet.flow_id for packet in packets})
    x_values = [packet.input_cycle - event.start_cycle for packet in packets]
    y_values = [packet.output_cycle - event.start_cycle for packet in packets]
    marker_values = [0, event.commit_cycle - event.start_cycle]
    if event.drain_cycle is not None:
        marker_values.append(event.drain_cycle - event.start_cycle)
    common_min = min(*x_values, *y_values, *marker_values)
    common_max = max(*x_values, *y_values, *marker_values)
    padding = max(1.0, (common_max - common_min) * 0.04)
    common_min -= padding
    common_max += padding
    left = 145 * scale
    top = 90 * scale
    right = 50 * scale
    bottom = 115 * scale
    plot_size = min(width - left - right, height - top - bottom)
    area = PlotArea(
        left,
        top,
        plot_size,
        plot_size,
        common_min,
        common_max,
        common_min,
        common_max,
    )
    ticks = nice_ticks(common_min, common_max, 8)
    svg.text(
        width / 2,
        45 * scale,
        f"Packet input–output scatter: {event_label(event)} ({event.mode})",
        font * 1.3,
        anchor="middle",
        weight="bold",
    )
    svg.text(
        width / 2,
        72 * scale,
        _timing_text(event, unicode_limit=False),
        font * 0.68,
        anchor="middle",
        color="#555555",
    )
    finish_value = event.finish_cycle - event.start_cycle
    input_min = min(*x_values, *marker_values)
    input_max = max(*x_values, *marker_values)
    show_finish = input_min <= finish_value <= input_max
    draw_axes(svg, area, ticks, ticks, True, font * 0.8)
    transition_markers(svg, area, event, include_finish=show_finish)
    scatter_output_markers(svg, area, event)
    svg.line(
        area.sx(common_min),
        area.sy(common_min),
        area.sx(common_max),
        area.sy(common_max),
        "#6baed6",
        1.8 * scale,
        opacity=0.7,
    )
    for index, flow_id in enumerate(flow_ids):
        color = COLORS[index % len(COLORS)]
        for packet in packets:
            if packet.flow_id == flow_id:
                svg.circle(
                    area.sx(packet.input_cycle - event.start_cycle),
                    area.sy(packet.output_cycle - event.start_cycle),
                    max(3.0, 6 * scale),
                    color,
                    opacity=0.72,
                )
    svg.text(
        area.x + area.width / 2,
        height - 30 * scale,
        "Packet input time relative to reconfiguration start (cycles)",
        font,
        anchor="middle",
    )
    svg.text(
        38 * scale,
        area.y + area.height / 2,
        "Packet output time relative to reconfiguration start (cycles)",
        font,
        anchor="middle",
        rotate=-90,
    )
    entries = [
        (flow_name(flow_id, labels), COLORS[index % len(COLORS)], None)
        for index, flow_id in enumerate(flow_ids)
    ]
    entries.extend(
        (
            ("start", START_COLOR, None),
            ("commit accepted", COMMIT_COLOR, "8,6"),
            ("y = x", "#6baed6", None),
        )
    )
    if show_finish:
        entries.append(("finish", FINISH_COLOR, None))
    if event.drain_cycle is not None:
        entries.append(("old tree drained", DRAIN_COLOR, "3,5"))
    legend(
        svg,
        area.x + 18 * scale,
        area.y + 18 * scale,
        entries,
        font * 0.72,
    )
    svg.write(path)


def _timing_text(event: PolicyEvent, unicode_limit: bool) -> str:
    limit = "≤" if unicode_limit else "<="
    instruction_text = (
        f"  config={event.instruction_count} inst @ {limit}1 accepted/cycle"
        if event.instruction_count is not None
        else ""
    )
    return (
        f"start={event.start_cycle}  commit={event.commit_cycle}  "
        f"drain={event.drain_cycle if event.drain_cycle is not None else '-'}  "
        f"finish={event.finish_cycle}{instruction_text}"
    )


def generate(inputs: FigureInputs) -> FigurePaths:
    renderer = select_renderer()
    paths = figure_paths(inputs.output_dir)
    inputs.output_dir.mkdir(parents=True, exist_ok=True)
    write_data(paths.data, inputs.packets, inputs.event)
    if renderer == "matplotlib":
        render_matplotlib(
            paths,
            inputs.packets,
            inputs.event,
            inputs.labels,
            inputs.dpi,
        )
    else:
        render_svg(
            paths.svg,
            inputs.packets,
            inputs.event,
            inputs.labels,
            inputs.dpi,
        )
        rasterize_svg(paths.svg, paths.png)
    return paths


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Generate the packet input/output scatter figure."
    )
    add_common_arguments(parser)
    return parser


def main() -> None:
    args = build_parser().parse_args()
    try:
        paths = generate(load_figure_inputs(args))
    except (OSError, RuntimeError, ValueError) as error:
        raise SystemExit(f"error: {error}") from error
    print("Generated packet scatter figure:")
    for path in (paths.data, paths.svg, paths.png):
        print(f"  {path}")
