"""Bandwidth figure data and rendering."""

from __future__ import annotations

import argparse
import csv
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Mapping, Sequence

from pifo_figures.common import (
    COLORS,
    BandwidthLike,
    FigureInputs,
    FigurePaths,
    PacketTiming,
    PlotArea,
    PolicyEvent,
    Svg,
    add_common_arguments,
    draw_axes,
    draw_timeline,
    event_label,
    figure_paths,
    flow_name,
    legend,
    line_path,
    load_figure_inputs,
    load_pyplot,
    nice_ticks,
    rasterize_svg,
    select_renderer,
    transition_markers,
    timing_text,
    timeline_legend,
    timeline_markers,
    timeline_spans,
    write_packet_outcomes,
)
from pifo_figures.standalone import write_plot_script


@dataclass(frozen=True)
class BandwidthSample:
    window_start_cycle: int
    window_end_cycle: int
    time_relative_to_start: float
    total_bytes_per_cycle: float
    total_link_fraction: float
    flow_bytes_per_cycle: Mapping[int, float]
    flow_link_fraction: Mapping[int, float]


def build_samples(
    packets: Sequence[PacketTiming],
    event: PolicyEvent,
    window_cycles: int,
    sample_cycles: int,
    link_bytes_per_cycle: float,
) -> tuple[list[int], list[BandwidthSample]]:
    """Convolve completion-byte impulses with a normalized Hann window."""
    if not packets:
        raise ValueError("cannot calculate bandwidth without packets")
    if window_cycles < 3:
        raise ValueError("--window-cycles must be at least 3")
    if sample_cycles <= 0:
        raise ValueError("--sample-cycles must be positive")
    if sample_cycles > window_cycles:
        raise ValueError("--sample-cycles cannot exceed --window-cycles")
    if not math.isfinite(link_bytes_per_cycle) or link_bytes_per_cycle <= 0:
        raise ValueError("--link-bytes-per-cycle must be finite and positive")

    flow_ids = sorted({packet.flow_id for packet in packets})
    kernel = _hann_kernel(window_cycles)
    left_extent = (window_cycles - 1) // 2
    right_extent = window_cycles - 1 - left_extent
    timing_cycles = [event.commit_cycle]
    if event.drain_cycle is not None:
        timing_cycles.append(event.drain_cycle)
    first_unaligned = min(
        event.start_cycle,
        *(packet.input_cycle for packet in packets),
        *(packet.output_cycle - right_extent for packet in packets),
    )
    last_unaligned = max(
        *timing_cycles,
        *(packet.output_cycle + left_extent for packet in packets),
    )
    first_cycle = event.start_cycle + math.floor(
        (first_unaligned - event.start_cycle) / sample_cycles
    ) * sample_cycles
    last_cycle = event.start_cycle + math.ceil(
        (last_unaligned - event.start_cycle) / sample_cycles
    ) * sample_cycles
    cycle_count = last_cycle - first_cycle + 1
    per_flow = {flow_id: [0.0] * cycle_count for flow_id in flow_ids}
    for packet in packets:
        first_index = packet.output_cycle - right_extent - first_cycle
        rates = per_flow[packet.flow_id]
        for offset, weight in enumerate(kernel):
            rates[first_index + offset] += packet.size_bytes * weight

    samples: list[BandwidthSample] = []
    for cycle in range(first_cycle, last_cycle + 1, sample_cycles):
        index = cycle - first_cycle
        rates = {
            flow_id: per_flow[flow_id][index]
            for flow_id in flow_ids
        }
        fractions = {
            flow_id: rate / link_bytes_per_cycle
            for flow_id, rate in rates.items()
        }
        total_rate = sum(rates.values())
        samples.append(
            BandwidthSample(
                window_start_cycle=cycle - left_extent,
                window_end_cycle=cycle + right_extent + 1,
                time_relative_to_start=cycle - event.start_cycle,
                total_bytes_per_cycle=total_rate,
                total_link_fraction=total_rate / link_bytes_per_cycle,
                flow_bytes_per_cycle=rates,
                flow_link_fraction=fractions,
            )
        )
    return flow_ids, samples


def _hann_kernel(window_cycles: int) -> list[float]:
    weights = [
        0.5 - 0.5 * math.cos(2 * math.pi * index / (window_cycles - 1))
        for index in range(window_cycles)
    ]
    total = sum(weights)
    return [weight / total for weight in weights]


def write_data(
    path: Path,
    flow_ids: Sequence[int],
    samples: Iterable[BandwidthSample],
) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(
            (
                "window_start_cycle",
                "window_end_cycle",
                "time_relative_to_start",
                "total_bytes_per_cycle",
                "total_link_fraction",
                *(f"flow_{flow_id}_bytes_per_cycle" for flow_id in flow_ids),
                *(f"flow_{flow_id}_link_fraction" for flow_id in flow_ids),
            )
        )
        for sample in samples:
            writer.writerow(
                (
                    sample.window_start_cycle,
                    sample.window_end_cycle,
                    f"{sample.time_relative_to_start:g}",
                    f"{sample.total_bytes_per_cycle:.12g}",
                    f"{sample.total_link_fraction:.12g}",
                    *(
                        f"{sample.flow_bytes_per_cycle[flow_id]:.12g}"
                        for flow_id in flow_ids
                    ),
                    *(
                        f"{sample.flow_link_fraction[flow_id]:.12g}"
                        for flow_id in flow_ids
                    ),
                )
            )


def render_matplotlib(
    paths: FigurePaths,
    flow_ids: Sequence[int],
    samples: Sequence[BandwidthSample],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
    title: str | None = None,
) -> None:
    plt, _ = load_pyplot()
    colors = plt.get_cmap("tab10")
    x_values = [sample.time_relative_to_start for sample in samples]
    figure, (total_axis, flow_axis) = plt.subplots(
        2, 1, figsize=(11, 8), sharex=True, constrained_layout=True
    )
    total_axis.plot(
        x_values,
        [sample.total_link_fraction for sample in samples],
        color="tab:blue",
        linewidth=2,
        label="Total bandwidth",
    )
    total_axis.axhline(
        1.0,
        color="0.45",
        linewidth=1,
        linestyle=":",
        label="Link capacity",
    )
    for index, flow_id in enumerate(flow_ids):
        flow_axis.plot(
            x_values,
            [sample.flow_link_fraction[flow_id] for sample in samples],
            color=colors(index % 10),
            linewidth=2,
            label=flow_name(flow_id, labels),
        )

    total_axis.legend(loc="upper right")
    for axis in (total_axis, flow_axis):
        draw_timeline(axis, event)
        axis.grid(True, color="0.9", linewidth=0.8)
        axis.set_ylim(bottom=0)

    window_cycles = samples[0].window_end_cycle - samples[0].window_start_cycle
    heading = title or event.label
    total_axis.set_title(
        f"{heading}: Hann-smoothed output bandwidth "
        f"({event.mode}, {window_cycles}-cycle window)"
    )
    total_axis.set_ylabel("Aggregate bandwidth / link capacity")
    flow_axis.set_ylabel("Per-flow bandwidth / link capacity")
    flow_axis.set_xlabel("Time relative to reconfiguration start (cycles)")
    timeline_legend(flow_axis, event)
    figure.savefig(paths.svg, bbox_inches="tight")
    figure.savefig(paths.png, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def render_svg(
    path: Path,
    flow_ids: Sequence[int],
    samples: Sequence[BandwidthLike],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    width = max(1100, round(11 * dpi))
    height = max(800, round(8 * dpi))
    scale = width / 1980
    font = max(15.0, 22 * scale)
    svg = Svg(width, height)

    x_min = min(0, min(sample.time_relative_to_start for sample in samples))
    x_max = max(
        max(sample.time_relative_to_start for sample in samples),
        event.finish_cycle - event.start_cycle,
        (event.drain_cycle or event.start_cycle) - event.start_cycle,
    )
    values = [sample.total_link_fraction for sample in samples]
    values.extend(
        sample.flow_link_fraction[flow_id]
        for sample in samples
        for flow_id in flow_ids
    )
    y_max = max(1.05, max(values) * 1.08)
    left = 150 * scale
    right = 45 * scale
    top = 105 * scale + (len(_timing_text(event, False).splitlines()) - 1) * font * 0.9
    bottom = 110 * scale
    gap = 95 * scale
    panel_height = (height - top - bottom - gap) / 2
    panel_width = width - left - right
    total_area = PlotArea(left, top, panel_width, panel_height, x_min, x_max, 0, y_max)
    flow_area = PlotArea(
        left,
        top + panel_height + gap,
        panel_width,
        panel_height,
        x_min,
        x_max,
        0,
        y_max,
    )
    x_ticks = nice_ticks(x_min, x_max, 9)
    y_ticks = nice_ticks(0, y_max, 5)

    svg.text(
        width / 2,
        48 * scale,
        f"{event_label(event)}: Hann-smoothed output bandwidth "
        f"({event.mode}, "
        f"{samples[0].window_end_cycle - samples[0].window_start_cycle}"
        "-cycle window)",
        font * 1.35,
        anchor="middle",
        weight="bold",
    )
    svg.text(
        width / 2,
        78 * scale,
        _timing_text(event, unicode_limit=False),
        font * 0.72,
        anchor="middle",
        color="#555555",
    )
    for area, show_x in ((total_area, False), (flow_area, True)):
        draw_axes(svg, area, x_ticks, y_ticks, show_x, font * 0.8)
        transition_markers(svg, area, event)

    total_points = [
        (
            sample.time_relative_to_start,
            sample.total_link_fraction,
        )
        for sample in samples
    ]
    svg.path(line_path(total_area, total_points), COLORS[0], width=3 * scale)
    if total_area.y_min <= 1 <= total_area.y_max:
        svg.line(
            total_area.x,
            total_area.sy(1),
            total_area.x + total_area.width,
            total_area.sy(1),
            "#666666",
            1.5 * scale,
            dash="3,5",
        )
    for index, flow_id in enumerate(flow_ids):
        points = [
            (
                sample.time_relative_to_start,
                sample.flow_link_fraction[flow_id],
            )
            for sample in samples
        ]
        svg.path(
            line_path(flow_area, points),
            COLORS[index % len(COLORS)],
            width=3 * scale,
        )

    svg.text(
        38 * scale,
        total_area.y + total_area.height / 2,
        "Aggregate bandwidth / link capacity",
        font,
        anchor="middle",
        rotate=-90,
    )
    svg.text(
        38 * scale,
        flow_area.y + flow_area.height / 2,
        "Per-flow bandwidth / link capacity",
        font,
        anchor="middle",
        rotate=-90,
    )
    svg.text(
        flow_area.x + flow_area.width / 2,
        height - 30 * scale,
        "Time relative to reconfiguration start (cycles)",
        font,
        anchor="middle",
    )
    legend(
        svg,
        total_area.x + total_area.width - 500 * scale,
        total_area.y + 18 * scale,
        (("Total bandwidth", COLORS[0], None), ("Link capacity", "#666666", "3,5"),
         *((label + " (background)", color, None) for _, _, color, label in timeline_spans(event))),
        font * 0.78,
    )
    flow_entries = tuple(
        (flow_name(flow_id, labels), COLORS[index % len(COLORS)], None)
        for index, flow_id in enumerate(flow_ids)
    )
    legend(
        svg,
        flow_area.x + flow_area.width - 240 * scale,
        flow_area.y + 18 * scale,
        flow_entries,
        font * 0.78,
    )
    _svg_marker_labels(svg, total_area, event, font, scale)
    svg.write(path)


def _svg_marker_labels(
    svg: Svg,
    area: PlotArea,
    event: PolicyEvent,
    font: float,
    scale: float,
) -> None:
    for index, (value, color, _, label) in enumerate(timeline_markers(event)):
        x = area.sx(value)
        if area.x <= x <= area.x + area.width:
            near_right = x > area.x + area.width * 0.67
            svg.text(
                x + (-7 if near_right else 7) * scale,
                area.y + (22 + index * 25) * scale,
                label,
                font * 0.72,
                color=color,
                weight="bold",
                anchor="end" if near_right else "start",
            )


def _timing_text(event: PolicyEvent, unicode_limit: bool) -> str:
    return timing_text(event, unicode_limit)


def generate(
    inputs: FigureInputs,
    window_cycles: int,
    sample_cycles: int,
    link_bytes_per_cycle: float,
) -> FigurePaths:
    renderer = select_renderer()
    paths = figure_paths(inputs.output_dir)
    inputs.output_dir.mkdir(parents=True, exist_ok=True)
    flow_ids, samples = build_samples(
        inputs.packets,
        inputs.event,
        window_cycles,
        sample_cycles,
        link_bytes_per_cycle,
    )
    write_data(paths.data, flow_ids, samples)
    write_packet_outcomes(paths.packets, inputs.packet_outcomes, inputs.labels)
    write_plot_script(
        paths, "bandwidth", [(inputs.event.label, inputs.event)], inputs.labels, inputs.dpi,
        f"{inputs.event.label}: Hann-smoothed output bandwidth "
        f"({inputs.event.mode}, {window_cycles}-cycle window)",
    )
    if renderer == "matplotlib":
        render_matplotlib(
            paths, flow_ids, samples, inputs.event, inputs.labels, inputs.dpi
        )
    else:
        render_svg(
            paths.svg,
            flow_ids,
            samples,
            inputs.event,
            inputs.labels,
            inputs.dpi,
        )
        rasterize_svg(paths.svg, paths.png)
    return paths


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Generate the bandwidth figure.")
    add_common_arguments(parser)
    parser.add_argument("--link-bytes-per-cycle", type=float, required=True)
    parser.add_argument("--window-cycles", type=int, required=True)
    parser.add_argument("--sample-cycles", type=int, required=True)
    return parser


def main() -> None:
    args = build_parser().parse_args()
    try:
        paths = generate(
            load_figure_inputs(args),
            args.window_cycles,
            args.sample_cycles,
            args.link_bytes_per_cycle,
        )
    except (OSError, RuntimeError, ValueError) as error:
        raise SystemExit(f"error: {error}") from error
    print("Generated bandwidth figure:")
    for path in (paths.data, paths.packets, paths.svg, paths.png, paths.data.parent / "plot.py"):
        print(f"  {path}")
