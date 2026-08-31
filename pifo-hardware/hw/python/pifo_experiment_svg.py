"""Small dependency-free SVG renderer for PIFO experiment figures.

Matplotlib remains the preferred renderer. This module provides a deterministic
fallback for simulation hosts that have no Python plotting packages installed.
"""

from __future__ import annotations

import html
import math
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping, Protocol, Sequence


COLORS = (
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
    "#e377c2",
    "#7f7f7f",
    "#bcbd22",
    "#17becf",
)
START_COLOR = "#1f77b4"
COMMIT_COLOR = "#ff7f0e"
FINISH_COLOR = "#2ca02c"
DRAIN_COLOR = "#9467bd"


class EventLike(Protocol):
    before: str
    after: str
    name: str
    mode: str
    start_cycle: int
    commit_cycle: int
    finish_cycle: int
    drain_cycle: int | None
    instruction_count: int | None


class PacketLike(Protocol):
    flow_id: int
    input_cycle: int
    output_cycle: int


class BandwidthLike(Protocol):
    window_start_cycle: int
    window_end_cycle: int
    total_link_fraction: float
    flow_link_fraction: Mapping[int, float]


@dataclass(frozen=True)
class PlotArea:
    x: float
    y: float
    width: float
    height: float
    x_min: float
    x_max: float
    y_min: float
    y_max: float

    def sx(self, value: float) -> float:
        return self.x + (value - self.x_min) * self.width / (
            self.x_max - self.x_min
        )

    def sy(self, value: float) -> float:
        return self.y + self.height - (value - self.y_min) * self.height / (
            self.y_max - self.y_min
        )


class Svg:
    def __init__(self, width: int, height: int) -> None:
        self.width = width
        self.height = height
        self.parts = [
            '<?xml version="1.0" encoding="UTF-8"?>',
            (
                f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" '
                f'height="{height}" viewBox="0 0 {width} {height}">'
            ),
            '<rect width="100%" height="100%" fill="white"/>',
        ]

    def add(self, value: str) -> None:
        self.parts.append(value)

    def line(
        self,
        x1: float,
        y1: float,
        x2: float,
        y2: float,
        color: str,
        width: float = 1,
        dash: str | None = None,
        opacity: float = 1,
    ) -> None:
        dash_attribute = f' stroke-dasharray="{dash}"' if dash else ""
        self.add(
            f'<line x1="{x1:.2f}" y1="{y1:.2f}" x2="{x2:.2f}" '
            f'y2="{y2:.2f}" stroke="{color}" stroke-width="{width:.2f}" '
            f'opacity="{opacity:.3f}"{dash_attribute}/>'
        )

    def rect(
        self,
        x: float,
        y: float,
        width: float,
        height: float,
        fill: str,
        opacity: float = 1,
        stroke: str | None = None,
    ) -> None:
        stroke_attribute = f' stroke="{stroke}"' if stroke else ""
        self.add(
            f'<rect x="{x:.2f}" y="{y:.2f}" width="{width:.2f}" '
            f'height="{height:.2f}" fill="{fill}" opacity="{opacity:.3f}"'
            f'{stroke_attribute}/>'
        )

    def text(
        self,
        x: float,
        y: float,
        value: str,
        size: float,
        anchor: str = "start",
        color: str = "#111111",
        weight: str = "normal",
        rotate: float | None = None,
    ) -> None:
        transform = (
            f' transform="rotate({rotate:.1f} {x:.2f} {y:.2f})"'
            if rotate is not None
            else ""
        )
        self.add(
            f'<text x="{x:.2f}" y="{y:.2f}" font-family="DejaVu Sans, sans-serif" '
            f'font-size="{size:.2f}" text-anchor="{anchor}" fill="{color}" '
            f'font-weight="{weight}"{transform}>{html.escape(value)}</text>'
        )

    def path(
        self,
        commands: str,
        color: str,
        width: float = 2,
        dash: str | None = None,
        opacity: float = 1,
    ) -> None:
        dash_attribute = f' stroke-dasharray="{dash}"' if dash else ""
        self.add(
            f'<path d="{commands}" fill="none" stroke="{color}" '
            f'stroke-width="{width:.2f}" stroke-linejoin="round" '
            f'stroke-linecap="round" opacity="{opacity:.3f}"{dash_attribute}/>'
        )

    def circle(
        self,
        x: float,
        y: float,
        radius: float,
        fill: str,
        opacity: float = 1,
    ) -> None:
        self.add(
            f'<circle cx="{x:.2f}" cy="{y:.2f}" r="{radius:.2f}" '
            f'fill="{fill}" opacity="{opacity:.3f}"/>'
        )

    def write(self, path: Path) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("\n".join((*self.parts, "</svg>", "")), encoding="utf-8")


def _nice_ticks(low: float, high: float, target: int = 7) -> list[float]:
    if not math.isfinite(low) or not math.isfinite(high) or high <= low:
        return [low]
    raw_step = (high - low) / max(1, target)
    magnitude = 10 ** math.floor(math.log10(raw_step))
    normalized = raw_step / magnitude
    if normalized <= 1:
        factor = 1
    elif normalized <= 2:
        factor = 2
    elif normalized <= 5:
        factor = 5
    else:
        factor = 10
    step = factor * magnitude
    first = math.ceil(low / step - 1e-12) * step
    ticks: list[float] = []
    value = first
    while value <= high + step * 1e-9:
        ticks.append(0.0 if abs(value) < step * 1e-9 else value)
        value += step
    return ticks


def _tick_label(value: float) -> str:
    if abs(value) >= 1000:
        return f"{value:.0f}"
    if abs(value) >= 10:
        return f"{value:.0f}"
    if abs(value) >= 1:
        return f"{value:.1f}".rstrip("0").rstrip(".")
    return f"{value:.2f}".rstrip("0").rstrip(".")


def _flow_name(flow_id: int, labels: Mapping[int, str]) -> str:
    return labels.get(flow_id, f"Flow {flow_id}")


def _draw_axes(
    svg: Svg,
    area: PlotArea,
    x_ticks: Sequence[float],
    y_ticks: Sequence[float],
    show_x_labels: bool,
    font_size: float,
) -> None:
    for tick in x_ticks:
        x = area.sx(tick)
        svg.line(x, area.y, x, area.y + area.height, "#e5e7eb", 1)
        if show_x_labels:
            svg.line(x, area.y + area.height, x, area.y + area.height + 6, "#222222")
            svg.text(
                x,
                area.y + area.height + font_size + 10,
                _tick_label(tick),
                font_size,
                anchor="middle",
            )
    for tick in y_ticks:
        y = area.sy(tick)
        svg.line(area.x, y, area.x + area.width, y, "#e5e7eb", 1)
        svg.line(area.x - 6, y, area.x, y, "#222222")
        svg.text(
            area.x - 12,
            y + font_size * 0.35,
            _tick_label(tick),
            font_size,
            anchor="end",
        )
    svg.rect(area.x, area.y, area.width, area.height, "none", stroke="#222222")


def _transition_markers(
    svg: Svg,
    area: PlotArea,
    event: EventLike,
    include_finish: bool = True,
) -> None:
    commit = event.commit_cycle - event.start_cycle
    finish = event.finish_cycle - event.start_cycle
    shade_start = max(0.0, area.x_min)
    shade_finish = min(float(commit), area.x_max)
    if shade_finish > shade_start:
        svg.rect(
            area.sx(shade_start),
            area.y,
            area.sx(shade_finish) - area.sx(shade_start),
            area.height,
            START_COLOR,
            opacity=0.08,
        )
    if event.drain_cycle is not None:
        drain = event.drain_cycle - event.start_cycle
        coexist_start = max(float(commit), area.x_min)
        coexist_finish = min(float(drain), area.x_max)
        if coexist_finish > coexist_start:
            svg.rect(
                area.sx(coexist_start),
                area.y,
                area.sx(coexist_finish) - area.sx(coexist_start),
                area.height,
                DRAIN_COLOR,
                opacity=0.05,
            )
    markers: list[tuple[float, str, str | None]] = [
        (0.0, START_COLOR, None),
        (float(commit), COMMIT_COLOR, "8,6"),
    ]
    if include_finish:
        markers.append((float(finish), FINISH_COLOR, None))
    if event.drain_cycle is not None:
        markers.append(
            (float(event.drain_cycle - event.start_cycle), DRAIN_COLOR, "3,5")
        )
    for value, color, dash in markers:
        if area.x_min <= value <= area.x_max:
            svg.line(
                area.sx(value),
                area.y,
                area.sx(value),
                area.y + area.height,
                color,
                2,
                dash=dash,
                opacity=0.9,
            )


def _scatter_output_transition_markers(
    svg: Svg, area: PlotArea, event: EventLike
) -> None:
    markers: list[tuple[float, str, str | None]] = [
        (0.0, START_COLOR, None),
        (
            float(event.commit_cycle - event.start_cycle),
            COMMIT_COLOR,
            "8,6",
        ),
    ]
    if event.drain_cycle is not None:
        markers.append(
            (
                float(event.drain_cycle - event.start_cycle),
                DRAIN_COLOR,
                "3,5",
            )
        )
    for value, color, dash in markers:
        if area.y_min <= value <= area.y_max:
            svg.line(
                area.x,
                area.sy(value),
                area.x + area.width,
                area.sy(value),
                color,
                2,
                dash=dash,
                opacity=0.9,
            )


def _step_path(area: PlotArea, points: Sequence[tuple[float, float]]) -> str:
    if not points:
        return ""
    commands = [f"M {area.sx(points[0][0]):.2f} {area.sy(points[0][1]):.2f}"]
    previous_x, _ = points[0]
    for x, y in points[1:]:
        midpoint = (previous_x + x) / 2
        commands.append(f"H {area.sx(midpoint):.2f}")
        commands.append(f"V {area.sy(y):.2f}")
        commands.append(f"H {area.sx(x):.2f}")
        previous_x = x
    return " ".join(commands)


def _legend(
    svg: Svg,
    x: float,
    y: float,
    entries: Sequence[tuple[str, str, str | None]],
    font_size: float,
) -> None:
    line_height = font_size + 10
    width = max(170.0, max(len(label) for label, _, _ in entries) * font_size * 0.58 + 70)
    height = line_height * len(entries) + 18
    svg.rect(x, y, width, height, "#ffffff", opacity=0.9, stroke="#c7c7c7")
    for index, (label, color, dash) in enumerate(entries):
        row_y = y + 15 + line_height * index + font_size * 0.55
        svg.line(x + 14, row_y - 4, x + 52, row_y - 4, color, 3, dash=dash)
        svg.text(x + 62, row_y, label, font_size)


def _event_label(event: EventLike) -> str:
    if event.before and event.after:
        return f"{event.before} → {event.after}"
    return event.name or event.mode


def render_bandwidth_svg(
    path: Path,
    flow_ids: Sequence[int],
    samples: Sequence[BandwidthLike],
    event: EventLike,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    width = max(1100, round(11 * dpi))
    height = max(800, round(8 * dpi))
    scale = width / 1980
    font = max(15.0, 22 * scale)
    svg = Svg(width, height)

    x_min = min(sample.window_start_cycle for sample in samples) - event.start_cycle
    x_max = max(sample.window_end_cycle for sample in samples) - event.start_cycle
    values = [sample.total_link_fraction for sample in samples]
    values.extend(
        sample.flow_link_fraction[flow_id]
        for sample in samples
        for flow_id in flow_ids
    )
    y_max = max(1.05, max(values) * 1.08)
    left = 150 * scale
    right = 45 * scale
    top = 105 * scale
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
    x_ticks = _nice_ticks(x_min, x_max, 9)
    y_ticks = _nice_ticks(0, y_max, 5)

    instruction_text = (
        f"  config={event.instruction_count} inst @ <=1 accepted/cycle"
        if event.instruction_count is not None
        else ""
    )
    svg.text(
        width / 2,
        48 * scale,
        f"{_event_label(event)}: output bandwidth ({event.mode})",
        font * 1.35,
        anchor="middle",
        weight="bold",
    )
    svg.text(
        width / 2,
        78 * scale,
        (
            f"start={event.start_cycle}  commit={event.commit_cycle}  "
            f"drain={event.drain_cycle if event.drain_cycle is not None else '-'}  "
            f"finish={event.finish_cycle}{instruction_text}"
        ),
        font * 0.72,
        anchor="middle",
        color="#555555",
    )
    for area, show_x in ((total_area, False), (flow_area, True)):
        _draw_axes(svg, area, x_ticks, y_ticks, show_x, font * 0.8)
        _transition_markers(svg, area, event)

    total_points = [
        (
            ((sample.window_start_cycle + sample.window_end_cycle) / 2)
            - event.start_cycle,
            sample.total_link_fraction,
        )
        for sample in samples
    ]
    svg.path(_step_path(total_area, total_points), COLORS[0], width=3 * scale)
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
                ((sample.window_start_cycle + sample.window_end_cycle) / 2)
                - event.start_cycle,
                sample.flow_link_fraction[flow_id],
            )
            for sample in samples
        ]
        svg.path(
            _step_path(flow_area, points),
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
    _legend(
        svg,
        total_area.x + total_area.width - 270 * scale,
        total_area.y + 18 * scale,
        (("Total bandwidth", COLORS[0], None), ("Link capacity", "#666666", "3,5")),
        font * 0.78,
    )
    flow_entries = tuple(
        (_flow_name(flow_id, labels), COLORS[index % len(COLORS)], None)
        for index, flow_id in enumerate(flow_ids)
    )
    _legend(
        svg,
        flow_area.x + flow_area.width - 240 * scale,
        flow_area.y + 18 * scale,
        flow_entries,
        font * 0.78,
    )
    marker_text: list[tuple[str, int, str]] = [
        ("start", 0, START_COLOR),
        ("commit accepted", event.commit_cycle - event.start_cycle, COMMIT_COLOR),
        ("finish", event.finish_cycle - event.start_cycle, FINISH_COLOR),
    ]
    if event.drain_cycle is not None:
        marker_text.append(
            (
                "old tree drained",
                event.drain_cycle - event.start_cycle,
                DRAIN_COLOR,
            )
        )
    for index, (label, value, color) in enumerate(marker_text):
        x = total_area.sx(value)
        if total_area.x <= x <= total_area.x + total_area.width:
            svg.text(
                x + 7 * scale,
                total_area.y + (22 + index * 25) * scale,
                label,
                font * 0.72,
                color=color,
                weight="bold",
            )
    svg.write(path)


def render_scatter_svg(
    path: Path,
    packets: Sequence[PacketLike],
    event: EventLike,
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
    marker_values = [
        0,
        event.commit_cycle - event.start_cycle,
    ]
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
    x_ticks = _nice_ticks(common_min, common_max, 8)
    y_ticks = x_ticks
    instruction_text = (
        f"  config={event.instruction_count} inst @ <=1 accepted/cycle"
        if event.instruction_count is not None
        else ""
    )
    svg.text(
        width / 2,
        45 * scale,
        f"Packet input–output scatter: {_event_label(event)} ({event.mode})",
        font * 1.3,
        anchor="middle",
        weight="bold",
    )
    svg.text(
        width / 2,
        72 * scale,
        (
            f"start={event.start_cycle}  commit={event.commit_cycle}  "
            f"drain={event.drain_cycle if event.drain_cycle is not None else '-'}  "
            f"finish={event.finish_cycle}{instruction_text}"
        ),
        font * 0.68,
        anchor="middle",
        color="#555555",
    )
    finish_value = event.finish_cycle - event.start_cycle
    input_min = min(*x_values, *marker_values)
    input_max = max(*x_values, *marker_values)
    show_finish = input_min <= finish_value <= input_max
    _draw_axes(svg, area, x_ticks, y_ticks, True, font * 0.8)
    _transition_markers(svg, area, event, include_finish=show_finish)
    _scatter_output_transition_markers(svg, area, event)
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
        (_flow_name(flow_id, labels), COLORS[index % len(COLORS)], None)
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
    _legend(
        svg,
        area.x + 18 * scale,
        area.y + 18 * scale,
        entries,
        font * 0.72,
    )
    svg.write(path)


def rasterize_svg(svg_path: Path, png_path: Path) -> None:
    ffmpeg = shutil.which("ffmpeg")
    if ffmpeg is None:
        raise RuntimeError(
            "matplotlib is unavailable and ffmpeg is required to rasterize SVG figures"
        )
    subprocess.run(
        [
            ffmpeg,
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-i",
            str(svg_path),
            "-frames:v",
            "1",
            str(png_path),
        ],
        check=True,
    )
