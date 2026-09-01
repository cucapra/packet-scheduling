"""Shared input, CLI, and drawing support for PIFO experiment figures."""

from __future__ import annotations

import argparse
import csv
import html
import math
import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping, Protocol, Sequence


RESULT_FIELDS = {
    "request_id",
    "global_flow_id",
    "size_bytes",
    "arrival_cycle",
    "completed_cycle",
}
EVENT_BASE_FIELDS = {
    "event",
    "from_policy",
    "to_policy",
    "scheduled_cycle",
}
EVENT_TIMING_FIELDS = {"start_cycle", "commit_cycle", "finish_cycle"}
LEGACY_EVENT_TIMING_FIELDS = {"request_cycle", "complete_cycle"}

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


@dataclass(frozen=True)
class PacketTiming:
    request_id: int
    flow_id: int
    size_bytes: int
    input_cycle: int
    output_cycle: int


@dataclass(frozen=True)
class PolicyEvent:
    before: str
    after: str
    scheduled_cycle: int
    start_cycle: int
    commit_cycle: int
    finish_cycle: int
    name: str = "policy-change"
    mode: str = "full_transitive"
    drain_cycle: int | None = None
    instruction_count: int | None = None

    @property
    def label(self) -> str:
        if self.before and self.after:
            return f"{self.before} → {self.after}"
        return self.name or self.mode


@dataclass(frozen=True)
class FigureInputs:
    packets: tuple[PacketTiming, ...]
    event: PolicyEvent
    labels: Mapping[int, str]
    dpi: int
    output_dir: Path


@dataclass(frozen=True)
class FigurePaths:
    data: Path
    svg: Path
    png: Path


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
    time_relative_to_start: float
    total_link_fraction: float
    flow_link_fraction: Mapping[int, float]


def add_common_arguments(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--results", type=Path, required=True)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument(
        "--flow-labels",
        help="Optional comma-separated FLOW:LABEL pairs, for example 1:A,2:B.",
    )
    parser.add_argument("--dpi", type=int, default=180)


def load_figure_inputs(args: argparse.Namespace) -> FigureInputs:
    if args.dpi <= 0:
        raise ValueError("--dpi must be positive")
    return FigureInputs(
        packets=tuple(read_packet_results(args.results)),
        event=read_policy_event(args.events),
        labels=parse_flow_mapping(args.flow_labels),
        dpi=args.dpi,
        output_dir=args.output_dir.resolve(),
    )


def figure_paths(output_dir: Path) -> FigurePaths:
    return FigurePaths(
        data=output_dir / "data.csv",
        svg=output_dir / "figure.svg",
        png=output_dir / "figure.png",
    )


def parse_flow_mapping(value: str | None) -> dict[int, str]:
    if not value:
        return {}
    result: dict[int, str] = {}
    for item in value.split(","):
        pair = item.strip().split(":", 1)
        if len(pair) != 2 or not pair[1].strip():
            raise ValueError(f"invalid FLOW:LABEL pair {item!r}")
        flow_id = parse_int(pair[0])
        if flow_id in result:
            raise ValueError(f"duplicate label for flow {flow_id}")
        result[flow_id] = pair[1].strip()
    return result


def read_packet_results(path: Path) -> list[PacketTiming]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        fields = set(reader.fieldnames or ())
        missing = RESULT_FIELDS.difference(fields)
        if missing:
            raise ValueError(
                f"{path}: missing result fields: {', '.join(sorted(missing))}"
            )

        packets: list[PacketTiming] = []
        seen_ids: set[int] = set()
        for line_number, row in enumerate(reader, start=2):
            try:
                packet = PacketTiming(
                    request_id=parse_int(row["request_id"]),
                    flow_id=parse_int(row["global_flow_id"]),
                    size_bytes=parse_int(row["size_bytes"]),
                    input_cycle=parse_int(row["arrival_cycle"]),
                    output_cycle=parse_int(row["completed_cycle"]),
                )
                _validate_packet(packet, seen_ids)
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error
            packets.append(packet)
            seen_ids.add(packet.request_id)
    if not packets:
        raise ValueError(f"{path}: no completed packets")
    return packets


def _validate_packet(packet: PacketTiming, seen_ids: set[int]) -> None:
    if packet.request_id in seen_ids:
        raise ValueError(f"duplicate request ID {packet.request_id}")
    if packet.flow_id < 0:
        raise ValueError("flow ID must be non-negative")
    if packet.size_bytes <= 0:
        raise ValueError("packet size must be positive")
    if packet.input_cycle < 0:
        raise ValueError("input cycle must be non-negative")
    if packet.output_cycle < packet.input_cycle:
        raise ValueError("output cycle precedes input cycle")


def read_policy_event(path: Path) -> PolicyEvent:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        fields = set(reader.fieldnames or ())
        missing = EVENT_BASE_FIELDS.difference(fields)
        if missing:
            raise ValueError(
                f"{path}: missing event fields: {', '.join(sorted(missing))}"
            )
        has_current_timing = EVENT_TIMING_FIELDS.issubset(fields)
        has_legacy_timing = LEGACY_EVENT_TIMING_FIELDS.issubset(fields)
        if not has_current_timing and not has_legacy_timing:
            raise ValueError(
                f"{path}: missing event timing fields: "
                + ", ".join(sorted(EVENT_TIMING_FIELDS))
            )
        rows = [
            row
            for row in reader
            if row.get("event")
            in {"policy_switch", "reconfiguration", "transaction_package"}
        ]
    if len(rows) != 1:
        raise ValueError(
            f"{path}: expected exactly one reconfiguration event, got {len(rows)}"
        )
    row = rows[0]
    if has_current_timing:
        start_cycle = parse_int(row["start_cycle"])
        commit_cycle = parse_int(row["commit_cycle"])
        finish_cycle = parse_int(row["finish_cycle"])
    else:
        start_cycle = parse_int(row["request_cycle"])
        commit_cycle = start_cycle
        finish_cycle = parse_int(row["complete_cycle"])
    event = PolicyEvent(
        before=row["from_policy"].strip(),
        after=row["to_policy"].strip(),
        scheduled_cycle=parse_int(row["scheduled_cycle"]),
        start_cycle=start_cycle,
        commit_cycle=commit_cycle,
        finish_cycle=finish_cycle,
        name=(row.get("name") or "policy-change").strip(),
        mode=(row.get("mode") or "full_transitive").strip(),
        drain_cycle=(
            parse_int(row["drain_cycle"])
            if (row.get("drain_cycle") or "").strip()
            else None
        ),
        instruction_count=(
            parse_int(row["instruction_count"])
            if (row.get("instruction_count") or "").strip()
            else None
        ),
    )
    _validate_event(path, row, event)
    return event


def _validate_event(
    path: Path, row: Mapping[str, str], event: PolicyEvent
) -> None:
    if not event.name:
        raise ValueError(f"{path}: event name must not be empty")
    if event.scheduled_cycle < 0:
        raise ValueError(f"{path}: scheduled cycle must be non-negative")
    if event.start_cycle < event.scheduled_cycle:
        raise ValueError(f"{path}: start cycle precedes scheduled cycle")
    if event.commit_cycle < event.start_cycle:
        raise ValueError(f"{path}: commit cycle precedes start cycle")
    if event.finish_cycle < event.commit_cycle:
        raise ValueError(f"{path}: finish cycle precedes commit cycle")
    if event.drain_cycle is not None and event.drain_cycle < event.commit_cycle:
        raise ValueError(f"{path}: drain cycle precedes commit cycle")
    if event.instruction_count is not None and event.instruction_count <= 0:
        raise ValueError(f"{path}: instruction count must be positive")
    duration_raw = (row.get("drain_duration_cycles") or "").strip()
    if duration_raw:
        if event.drain_cycle is None:
            raise ValueError(f"{path}: drain duration is present without drain cycle")
        if parse_int(duration_raw) != event.drain_cycle - event.commit_cycle:
            raise ValueError(
                f"{path}: drain duration does not match drain and commit cycles"
            )


def parse_int(value: str) -> int:
    try:
        return int(value.strip(), 0)
    except ValueError as error:
        raise ValueError(f"invalid integer {value!r}") from error


def flow_name(flow_id: int, labels: Mapping[int, str]) -> str:
    return labels.get(flow_id, f"Flow {flow_id}")


def load_pyplot():
    os.environ.setdefault("MPLBACKEND", "Agg")
    try:
        import matplotlib

        matplotlib.use("Agg")
        from matplotlib import pyplot as plt
        from matplotlib.lines import Line2D
    except ModuleNotFoundError as error:
        raise RuntimeError("matplotlib is unavailable") from error
    return plt, Line2D


def select_renderer() -> str:
    try:
        load_pyplot()
        return "matplotlib"
    except RuntimeError:
        if shutil.which("ffmpeg") is not None:
            return "svg"
        raise RuntimeError(
            "rendering requires matplotlib, or ffmpeg for the SVG fallback"
        )


def rasterize_svg(svg_path: Path, png_path: Path) -> None:
    ffmpeg = shutil.which("ffmpeg")
    if ffmpeg is None:
        raise RuntimeError("ffmpeg is required to rasterize an SVG figure")
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
            f'<text x="{x:.2f}" y="{y:.2f}" '
            'font-family="DejaVu Sans, sans-serif" '
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
            f'stroke-linecap="round" opacity="{opacity:.3f}"'
            f'{dash_attribute}/>'
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


def nice_ticks(low: float, high: float, target: int = 7) -> list[float]:
    if not math.isfinite(low) or not math.isfinite(high) or high <= low:
        return [low]
    raw_step = (high - low) / max(1, target)
    magnitude = 10 ** math.floor(math.log10(raw_step))
    normalized = raw_step / magnitude
    factor = 1 if normalized <= 1 else 2 if normalized <= 2 else 5 if normalized <= 5 else 10
    step = factor * magnitude
    first = math.ceil(low / step - 1e-12) * step
    ticks: list[float] = []
    value = first
    while value <= high + step * 1e-9:
        ticks.append(0.0 if abs(value) < step * 1e-9 else value)
        value += step
    return ticks


def tick_label(value: float) -> str:
    if abs(value) >= 10:
        return f"{value:.0f}"
    if abs(value) >= 1:
        return f"{value:.1f}".rstrip("0").rstrip(".")
    return f"{value:.2f}".rstrip("0").rstrip(".")


def draw_axes(
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
                tick_label(tick),
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
            tick_label(tick),
            font_size,
            anchor="end",
        )
    svg.rect(area.x, area.y, area.width, area.height, "none", stroke="#222222")


def transition_markers(
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


def scatter_output_markers(svg: Svg, area: PlotArea, event: EventLike) -> None:
    markers: list[tuple[float, str, str | None]] = [
        (0.0, START_COLOR, None),
        (float(event.commit_cycle - event.start_cycle), COMMIT_COLOR, "8,6"),
    ]
    if event.drain_cycle is not None:
        markers.append(
            (float(event.drain_cycle - event.start_cycle), DRAIN_COLOR, "3,5")
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


def line_path(area: PlotArea, points: Sequence[tuple[float, float]]) -> str:
    if not points:
        return ""
    commands = [f"M {area.sx(points[0][0]):.2f} {area.sy(points[0][1]):.2f}"]
    for x, y in points[1:]:
        commands.append(f"L {area.sx(x):.2f} {area.sy(y):.2f}")
    return " ".join(commands)


def legend(
    svg: Svg,
    x: float,
    y: float,
    entries: Sequence[tuple[str, str, str | None]],
    font_size: float,
) -> None:
    line_height = font_size + 10
    width = max(
        170.0,
        max(len(label) for label, _, _ in entries) * font_size * 0.58 + 70,
    )
    height = line_height * len(entries) + 18
    svg.rect(x, y, width, height, "#ffffff", opacity=0.9, stroke="#c7c7c7")
    for index, (label, color, dash) in enumerate(entries):
        row_y = y + 15 + line_height * index + font_size * 0.55
        svg.line(x + 14, row_y - 4, x + 52, row_y - 4, color, 3, dash=dash)
        svg.text(x + 62, row_y, label, font_size)


def event_label(event: EventLike) -> str:
    if event.before and event.after:
        return f"{event.before} → {event.after}"
    return event.name or event.mode
