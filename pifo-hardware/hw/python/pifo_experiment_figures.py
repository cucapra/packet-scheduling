#!/usr/bin/env python3
"""Run and plot a cycle-scheduled PIFO reconfiguration experiment.

The default smoke experiment changes the full tree from round robin (the
equal-weight WFQ hardware mode) to strict priority. Direct controller packages
are also supported. Both figures are derived from one completion CSV.
"""

from __future__ import annotations

import argparse
import csv
import math
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Iterable, Mapping, Sequence

from pifo_experiment_config import (
    PACKET_RATE_UNIT,
    DistributionSpec,
    ExperimentConfig,
    InitialTreeConfig,
    NodePolicyChangeConfig,
    PlotConfig,
    PolicyChangeConfig,
    SimulationConfig,
    TrafficConfig,
    TreeNodeConfig,
    generate_distributed_requests,
    load_experiment_config,
    write_effective_config,
)
from pifo_experiment_transactions import (
    build_transaction_plan,
    write_controller_commands,
)
from pifo_experiment_svg import (
    rasterize_svg,
    render_bandwidth_svg,
    render_scatter_svg,
)
from pifo_experiment_verify import run_verification
from request_trace import Request, write_trace


HARDWARE_ROOT = Path(__file__).resolve().parents[2]
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
EVENT_TIMING_FIELDS = {
    "start_cycle",
    "commit_cycle",
    "finish_cycle",
}
LEGACY_EVENT_TIMING_FIELDS = {
    "request_cycle",
    "complete_cycle",
}


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
    def request_cycle(self) -> int:
        """Compatibility alias for artifacts produced before commit logging."""

        return self.start_cycle

    @property
    def complete_cycle(self) -> int:
        """Compatibility alias for artifacts produced before commit logging."""

        return self.finish_cycle

    @property
    def label(self) -> str:
        if self.before and self.after:
            return f"{self.before} → {self.after}"
        return self.name or self.mode


@dataclass(frozen=True)
class BandwidthSample:
    window_start_cycle: int
    window_end_cycle: int
    time_relative_to_start: float
    total_bytes_per_cycle: float
    total_link_fraction: float
    flow_bytes_per_cycle: Mapping[int, float]
    flow_link_fraction: Mapping[int, float]


def _parse_int(value: str) -> int:
    try:
        return int(value.strip(), 0)
    except ValueError as error:
        raise ValueError(f"invalid integer {value!r}") from error


def parse_flow_ids(value: str) -> list[int]:
    flow_ids = [_parse_int(item) for item in value.split(",") if item.strip()]
    if not flow_ids:
        raise ValueError("at least one flow ID is required")
    if len(set(flow_ids)) != len(flow_ids):
        raise ValueError("flow IDs must be unique")
    if any(flow_id < 0 for flow_id in flow_ids):
        raise ValueError("flow IDs must be non-negative")
    return flow_ids


def parse_flow_mapping(value: str | None) -> dict[int, str]:
    if not value:
        return {}
    result: dict[int, str] = {}
    for item in value.split(","):
        pair = item.strip().split(":", 1)
        if len(pair) != 2 or not pair[1].strip():
            raise ValueError(f"invalid FLOW:LABEL pair {item!r}")
        flow_id = _parse_int(pair[0])
        if flow_id in result:
            raise ValueError(f"duplicate label for flow {flow_id}")
        result[flow_id] = pair[1].strip()
    return result


def parse_priorities(
    value: str | None,
    flow_ids: Sequence[int],
    max_packet_priority: int,
) -> dict[int, int]:
    if max_packet_priority <= 1:
        raise ValueError("max packet priority must be greater than one")
    if value:
        result: dict[int, int] = {}
        for item in value.split(","):
            pair = item.strip().split(":", 1)
            if len(pair) != 2:
                raise ValueError(f"invalid FLOW:PRIORITY pair {item!r}")
            flow_id = _parse_int(pair[0])
            if flow_id in result:
                raise ValueError(f"duplicate priority for flow {flow_id}")
            result[flow_id] = _parse_int(pair[1])
        missing = set(flow_ids).difference(result)
        if missing:
            raise ValueError(
                "strict priorities are missing flow IDs: "
                + ", ".join(str(flow_id) for flow_id in sorted(missing))
            )
    else:
        count = max(1, len(flow_ids))
        step = max(1, max_packet_priority // count)
        result = {
            flow_id: min(max_packet_priority - 1, 1 + index * step)
            for index, flow_id in enumerate(sorted(flow_ids))
        }
    if any(
        priority <= 0 or priority >= max_packet_priority
        for priority in result.values()
    ):
        raise ValueError(
            f"strict priorities must be in [1, {max_packet_priority - 1}]"
        )
    return result


def generate_requests(
    flow_ids: Sequence[int],
    packets_per_flow: int,
    start_cycle: int,
    arrival_gap_cycles: int,
    packet_size_bytes: int,
) -> list[Request]:
    if packets_per_flow <= 0:
        raise ValueError("packets per flow must be positive")
    if start_cycle < 0:
        raise ValueError("start cycle must be non-negative")
    if arrival_gap_cycles <= 0:
        raise ValueError("arrival gap must be positive")
    if packet_size_bytes <= 0:
        raise ValueError("packet size must be positive")

    requests: list[Request] = []
    request_id = 1
    for packet_index in range(packets_per_flow):
        cycle = start_cycle + packet_index * arrival_gap_cycles
        for flow_id in flow_ids:
            requests.append(
                Request(
                    cycle=cycle,
                    request_id=request_id,
                    global_flow_id=flow_id,
                    size_bytes=packet_size_bytes,
                )
            )
            request_id += 1
    return requests


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
                    request_id=_parse_int(row["request_id"]),
                    flow_id=_parse_int(row["global_flow_id"]),
                    size_bytes=_parse_int(row["size_bytes"]),
                    input_cycle=_parse_int(row["arrival_cycle"]),
                    output_cycle=_parse_int(row["completed_cycle"]),
                )
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
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error
            packets.append(packet)
            seen_ids.add(packet.request_id)
    if not packets:
        raise ValueError(f"{path}: no completed packets")
    return packets


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
            expected = sorted(EVENT_TIMING_FIELDS)
            raise ValueError(
                f"{path}: missing event timing fields: {', '.join(expected)}"
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
        start_cycle = _parse_int(row["start_cycle"])
        commit_cycle = _parse_int(row["commit_cycle"])
        finish_cycle = _parse_int(row["finish_cycle"])
    else:
        # Old files did not identify the CommitMapper instruction. Treat the
        # request itself as the only known transaction marker when replotting.
        start_cycle = _parse_int(row["request_cycle"])
        commit_cycle = start_cycle
        finish_cycle = _parse_int(row["complete_cycle"])
    event = PolicyEvent(
        before=row["from_policy"].strip(),
        after=row["to_policy"].strip(),
        scheduled_cycle=_parse_int(row["scheduled_cycle"]),
        start_cycle=start_cycle,
        commit_cycle=commit_cycle,
        finish_cycle=finish_cycle,
        name=(row.get("name") or "policy-change").strip(),
        mode=(row.get("mode") or "full_transitive").strip(),
        drain_cycle=(
            _parse_int(row["drain_cycle"])
            if (row.get("drain_cycle") or "").strip()
            else None
        ),
        instruction_count=(
            _parse_int(row["instruction_count"])
            if (row.get("instruction_count") or "").strip()
            else None
        ),
    )
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
        if _parse_int(duration_raw) != event.drain_cycle - event.commit_cycle:
            raise ValueError(f"{path}: drain duration does not match drain and commit cycles")
    return event


def build_bandwidth_samples(
    packets: Sequence[PacketTiming],
    event: PolicyEvent,
    bin_cycles: int,
    link_bytes_per_cycle: float,
) -> tuple[list[int], list[BandwidthSample]]:
    if not packets:
        raise ValueError("cannot calculate bandwidth without packets")
    if bin_cycles <= 0:
        raise ValueError("bin cycles must be positive")
    if not math.isfinite(link_bytes_per_cycle) or link_bytes_per_cycle <= 0:
        raise ValueError("link bytes per cycle must be finite and positive")

    flow_ids = sorted({packet.flow_id for packet in packets})
    first_cycle = min(event.start_cycle, *(packet.input_cycle for packet in packets))
    timing_cycles = [event.commit_cycle]
    if event.drain_cycle is not None:
        timing_cycles.append(event.drain_cycle)
    last_cycle = max(*timing_cycles, *(packet.output_cycle for packet in packets))
    first_offset = first_cycle - event.start_cycle
    start_cycle = event.start_cycle + math.floor(first_offset / bin_cycles) * bin_cycles
    bin_count = max(1, (last_cycle - start_cycle) // bin_cycles + 1)

    per_bin = [dict.fromkeys(flow_ids, 0) for _ in range(bin_count)]
    for packet in packets:
        index = (packet.output_cycle - start_cycle) // bin_cycles
        per_bin[index][packet.flow_id] += packet.size_bytes

    samples: list[BandwidthSample] = []
    for index, flow_bytes in enumerate(per_bin):
        window_start = start_cycle + index * bin_cycles
        window_end = window_start + bin_cycles
        rates = {
            flow_id: byte_count / bin_cycles
            for flow_id, byte_count in flow_bytes.items()
        }
        fractions = {
            flow_id: rate / link_bytes_per_cycle
            for flow_id, rate in rates.items()
        }
        total_rate = sum(rates.values())
        samples.append(
            BandwidthSample(
                window_start_cycle=window_start,
                window_end_cycle=window_end,
                time_relative_to_start=(
                    (window_start + window_end) / 2 - event.start_cycle
                ),
                total_bytes_per_cycle=total_rate,
                total_link_fraction=total_rate / link_bytes_per_cycle,
                flow_bytes_per_cycle=rates,
                flow_link_fraction=fractions,
            )
        )
    return flow_ids, samples


def write_packet_times(
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


def write_bandwidth(
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


def _load_pyplot():
    os.environ.setdefault("MPLBACKEND", "Agg")
    try:
        import matplotlib

        matplotlib.use("Agg")
        from matplotlib import pyplot as plt
        from matplotlib.lines import Line2D
    except ModuleNotFoundError as error:
        raise RuntimeError(
            "matplotlib is required to render figures; install it with "
            "python3 -m pip install matplotlib"
        ) from error
    return plt, Line2D


def _select_renderer() -> str:
    try:
        _load_pyplot()
        return "matplotlib"
    except RuntimeError:
        if shutil.which("ffmpeg") is not None:
            return "svg"
        raise


def _flow_name(flow_id: int, labels: Mapping[int, str]) -> str:
    return labels.get(flow_id, f"Flow {flow_id}")


def plot_bandwidth(
    path: Path,
    flow_ids: Sequence[int],
    samples: Sequence[BandwidthSample],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    plt, _ = _load_pyplot()
    colors = plt.get_cmap("tab10")
    x_values = [sample.time_relative_to_start for sample in samples]
    transition_commit = event.commit_cycle - event.start_cycle
    transition_finish = event.finish_cycle - event.start_cycle
    transition_drain = (
        event.drain_cycle - event.start_cycle
        if event.drain_cycle is not None
        else None
    )
    visible_min = min(x_values)
    visible_max = max(x_values)

    figure, (total_axis, flow_axis) = plt.subplots(
        2, 1, figsize=(11, 8), sharex=True, constrained_layout=True
    )
    total_axis.plot(
        x_values,
        [sample.total_link_fraction for sample in samples],
        color="tab:blue",
        linewidth=2,
        drawstyle="steps-mid",
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
            drawstyle="steps-mid",
            label=_flow_name(flow_id, labels),
        )

    for axis in (total_axis, flow_axis):
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
        axis.axvline(
            transition_commit,
            color="tab:orange",
            linewidth=1.3,
            linestyle="--",
        )
        if visible_min <= transition_finish <= visible_max:
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
        axis.grid(True, color="0.9", linewidth=0.8)
        axis.set_ylim(bottom=0)

    total_axis.set_title(f"{event.label}: output bandwidth ({event.mode})")
    total_axis.set_ylabel("Aggregate bandwidth / link capacity")
    total_axis.legend(loc="best")
    flow_axis.set_ylabel("Per-flow bandwidth / link capacity")
    flow_axis.set_xlabel("Time relative to reconfiguration start (cycles)")
    flow_axis.legend(loc="best", ncol=min(4, len(flow_ids)))
    total_axis.annotate(
        "start",
        xy=(0, 1),
        xycoords=("data", "axes fraction"),
        xytext=(5, -8),
        textcoords="offset points",
        va="top",
        color="tab:blue",
    )
    total_axis.annotate(
        "commit accepted",
        xy=(transition_commit, 1),
        xycoords=("data", "axes fraction"),
        xytext=(5, -25),
        textcoords="offset points",
        va="top",
        color="tab:orange",
    )
    if visible_min <= transition_finish <= visible_max:
        total_axis.annotate(
            "finish",
            xy=(transition_finish, 1),
            xycoords=("data", "axes fraction"),
            xytext=(5, -42),
            textcoords="offset points",
            va="top",
            color="tab:green",
        )
    if transition_drain is not None:
        total_axis.annotate(
            "old tree drained",
            xy=(transition_drain, 1),
            xycoords=("data", "axes fraction"),
            xytext=(5, -59),
            textcoords="offset points",
            va="top",
            color="tab:purple",
        )
    instruction_text = (
        f"  config={event.instruction_count} inst @ ≤1 accepted/cycle"
        if event.instruction_count is not None
        else ""
    )
    timing = (
        f"start={event.start_cycle}  commit={event.commit_cycle}  "
        f"drain={event.drain_cycle if event.drain_cycle is not None else '-'}  "
        f"finish={event.finish_cycle}{instruction_text}"
    )
    total_axis.text(
        0.99,
        0.03,
        timing,
        transform=total_axis.transAxes,
        ha="right",
        va="bottom",
        fontsize="small",
        color="0.35",
    )
    figure.savefig(path, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def plot_packet_scatter(
    path: Path,
    packets: Sequence[PacketTiming],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    plt, Line2D = _load_pyplot()
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
            label=_flow_name(flow_id, labels),
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
    input_min = min(visible_input)
    input_max = max(visible_input)
    show_finish = input_min <= transition_finish <= input_max
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

    handles, legend_labels = axis.get_legend_handles_labels()
    handles.extend(
        [
            Line2D([0], [0], color="tab:blue", linewidth=1.5),
            Line2D([0], [0], color="tab:orange", linewidth=1.3, linestyle="--"),
            *(
                [Line2D([0], [0], color="tab:green", linewidth=1.2, alpha=0.8)]
                if show_finish
                else []
            ),
            *(
                [
                    Line2D(
                        [0],
                        [0],
                        color="tab:purple",
                        linewidth=1.3,
                        linestyle=":",
                    )
                ]
                if transition_drain is not None
                else []
            ),
        ]
    )
    legend_labels.extend(("start", "commit accepted"))
    if show_finish:
        legend_labels.append("finish")
    if transition_drain is not None:
        legend_labels.append("old tree drained")
    instruction_text = (
        f"  config={event.instruction_count} inst @ ≤1 accepted/cycle"
        if event.instruction_count is not None
        else ""
    )
    axis.text(
        0.99,
        0.03,
        (
            f"start={event.start_cycle}  commit={event.commit_cycle}  "
            f"drain={event.drain_cycle if event.drain_cycle is not None else '-'}  "
            f"finish={event.finish_cycle}{instruction_text}"
        ),
        transform=axis.transAxes,
        ha="right",
        va="bottom",
        fontsize="small",
        color="0.35",
    )
    axis.legend(handles, legend_labels, loc="best")
    figure.savefig(path, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def generate_figures(
    results_path: Path,
    event_path: Path,
    output_dir: Path,
    link_bytes_per_cycle: float,
    bin_cycles: int,
    labels: Mapping[int, str],
    dpi: int,
) -> list[Path]:
    # Select the renderer before writing a partial set of derived artifacts.
    # Matplotlib is preferred; the SVG fallback uses FFmpeg only for rasterizing.
    renderer = _select_renderer()
    packets = read_packet_results(results_path)
    event = read_policy_event(event_path)
    flow_ids, samples = build_bandwidth_samples(
        packets, event, bin_cycles, link_bytes_per_cycle
    )
    output_dir.mkdir(parents=True, exist_ok=True)
    slug_source = (
        f"{event.before}-to-{event.after}"
        if event.before and event.after
        else event.name
    )
    slug = "".join(
        character.lower() if character.isalnum() else "-"
        for character in slug_source
    ).strip("-") or "reconfiguration"
    packet_csv = output_dir / "packet-times.csv"
    bandwidth_csv = output_dir / "bandwidth.csv"
    bandwidth_figure = output_dir / f"{slug}-bandwidth.png"
    scatter_figure = output_dir / f"{slug}-packet-scatter.png"
    bandwidth_svg = output_dir / f"{slug}-bandwidth.svg"
    scatter_svg = output_dir / f"{slug}-packet-scatter.svg"

    write_packet_times(packet_csv, packets, event)
    write_bandwidth(bandwidth_csv, flow_ids, samples)
    if renderer == "matplotlib":
        plot_bandwidth(bandwidth_figure, flow_ids, samples, event, labels, dpi)
        plot_packet_scatter(scatter_figure, packets, event, labels, dpi)
        return [packet_csv, bandwidth_csv, bandwidth_figure, scatter_figure]

    print("Matplotlib unavailable; rendering SVG figures with the FFmpeg fallback.")
    render_bandwidth_svg(
        bandwidth_svg, flow_ids, samples, event, labels, dpi
    )
    render_scatter_svg(scatter_svg, packets, event, labels, dpi)
    rasterize_svg(bandwidth_svg, bandwidth_figure)
    rasterize_svg(scatter_svg, scatter_figure)
    return [
        packet_csv,
        bandwidth_csv,
        bandwidth_svg,
        scatter_svg,
        bandwidth_figure,
        scatter_figure,
    ]


def _quote_sbt(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def _legacy_experiment_config(args: argparse.Namespace) -> ExperimentConfig:
    flow_ids = parse_flow_ids(args.flows)
    priorities = parse_priorities(
        args.strict_priorities, flow_ids, args.max_packet_priority
    )
    labels = parse_flow_mapping(args.flow_labels)
    before = args.policy_before.strip().upper()
    after = args.policy_after.strip().upper()
    initial_tree = InitialTreeConfig(
        root="root",
        nodes={
            "root": TreeNodeConfig(
                engine_id=1,
                vpifo_id=10,
                policy=before,
                flow_state=priorities if before == "SP" else {},
            )
        },
        flow_paths={flow_id: ("root",) for flow_id in flow_ids},
    )
    return ExperimentConfig(
        output_dir=args.output_dir or Path("experiment-results/rr-to-sp"),
        seed=1,
        traffic=TrafficConfig(
            flow_ids=tuple(flow_ids),
            packets_per_flow=args.packets_per_flow,
            start_cycle=args.start_cycle,
            packet_rate=DistributionSpec(
                distribution="constant",
                value=1.0 / args.arrival_gap_cycles
                if args.arrival_gap_cycles > 0
                else 0.0,
                unit=PACKET_RATE_UNIT,
            ),
            packet_size_bytes=DistributionSpec(
                distribution="constant",
                value=float(args.packet_size_bytes),
            ),
        ),
        initial_tree=initial_tree,
        reconfiguration=PolicyChangeConfig(
            cycle=args.switch_cycle,
            name="policy-change",
            before_label=before,
            after_label=after,
            changes={
                "root": NodePolicyChangeConfig(
                    policy=after,
                    flow_state=priorities if after == "SP" else {},
                )
            },
        ),
        simulation=SimulationConfig(
            link_bytes_per_cycle=args.link_bytes_per_cycle,
            queue_depth=args.queue_depth,
            max_cycles=args.max_cycles,
            max_packet_priority=args.max_packet_priority,
        ),
        plot=PlotConfig(
            bandwidth_bin_cycles=args.bin_cycles,
            flow_labels=labels,
            dpi=args.dpi,
        ),
    )


def _config_from_run_args(args: argparse.Namespace) -> ExperimentConfig:
    if args.config is not None:
        config = load_experiment_config(args.config, args.output_dir)
    else:
        config = _legacy_experiment_config(args)
    return replace(config, output_dir=config.output_dir.resolve())


def run_experiment(args: argparse.Namespace) -> None:
    config = _config_from_run_args(args)
    requests = generate_distributed_requests(config.traffic, config.seed)
    transaction = build_transaction_plan(config)
    # Check plotting support before starting an expensive RTL run.
    _select_renderer()

    output_dir = config.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    effective_config_path = output_dir / "experiment-config.json"
    trace_path = output_dir / "requests.csv"
    results_path = output_dir / "request-results.csv"
    event_path = output_dir / "reconfiguration-events.csv"
    initial_commands_path = output_dir / "initial-tree.commands"
    transaction_commands_path = output_dir / "reconfiguration.commands"
    write_effective_config(effective_config_path, config)
    write_controller_commands(initial_commands_path, transaction.initial_commands)
    write_controller_commands(
        transaction_commands_path, transaction.transaction_commands
    )
    with trace_path.open("w", newline="", encoding="utf-8") as destination:
        write_trace(requests, destination)

    sbt_path = shutil.which(args.sbt)
    if sbt_path is None:
        raise RuntimeError(f"could not find sbt executable {args.sbt!r}")
    simulator_args = [
        "--trace",
        str(trace_path),
        "--output",
        str(results_path),
        "--control-file",
        str(initial_commands_path),
        "--no-flat-fifo",
        "--root-engine",
        str(transaction.root_engine_id),
        "--root-vpifo",
        str(transaction.root_vpifo_id),
        "--scheduled-transaction",
        str(transaction_commands_path),
        "--transaction-event-output",
        str(event_path),
        "--transaction-cycle",
        str(transaction.cycle),
        "--transaction-name",
        transaction.name,
        "--transaction-mode",
        transaction.mode,
        "--link-bytes-per-cycle",
        str(config.simulation.link_bytes_per_cycle),
        "--queue-depth",
        str(config.simulation.queue_depth),
        "--max-cycles",
        str(config.simulation.max_cycles),
        "--max-packet-priority",
        str(config.simulation.max_packet_priority),
        "--num-engines",
        str(config.simulation.num_engines),
        "--num-vpifos",
        str(config.simulation.num_vpifos),
        "--fifo-depth",
        str(config.simulation.fifo_depth),
        "--prefetch-buffer-depth",
        str(config.simulation.prefetch_buffer_depth),
        "--no-control-socket",
        "--no-wave",
        "--quiet",
    ]
    if transaction.before_label:
        simulator_args.extend(["--transaction-before", transaction.before_label])
    if transaction.after_label:
        simulator_args.extend(["--transaction-after", transaction.after_label])
    if transaction.drain_engine_id is not None:
        assert transaction.drain_vpifo_id is not None
        simulator_args.extend(
            [
                "--transaction-drain-root",
                f"{transaction.drain_engine_id}:{transaction.drain_vpifo_id}",
            ]
        )
    sbt_command = "runMain rio.sim.RequestSimulatorCli " + " ".join(
        _quote_sbt(value) for value in simulator_args
    )
    print(
        f"Running {transaction.before_label or transaction.name} → "
        f"{transaction.after_label or transaction.mode} ({transaction.mode}) "
        f"with {len(requests)} packets..."
    )
    subprocess.run([sbt_path, sbt_command], cwd=HARDWARE_ROOT, check=True)

    generated = generate_figures(
        results_path=results_path,
        event_path=event_path,
        output_dir=output_dir,
        link_bytes_per_cycle=config.simulation.link_bytes_per_cycle,
        bin_cycles=config.plot.bandwidth_bin_cycles,
        labels=config.plot.flow_labels or {},
        dpi=config.plot.dpi,
    )
    verification_paths: tuple[Path, ...] = ()
    verification_report: Mapping[str, object] | None = None
    if config.verification is not None:
        verification_paths = (
            output_dir / "phase-verification.json",
            output_dir / "phase-verification.md",
        )
        verification_report = run_verification(
            config,
            results_path,
            event_path,
            verification_paths[0],
            verification_paths[1],
        )
    print("Generated:")
    for path in (
        effective_config_path,
        trace_path,
        initial_commands_path,
        transaction_commands_path,
        results_path,
        event_path,
        *generated,
        *verification_paths,
    ):
        print(f"  {path}")
    if verification_report is not None:
        status = "PASS" if verification_report["passed"] else "FAIL"
        print(f"Phase verification: {status}")
        if not verification_report["passed"]:
            raise RuntimeError(
                f"phase verification failed; see {verification_paths[1]}"
            )


def validate_experiment_config(args: argparse.Namespace) -> None:
    config = load_experiment_config(args.config)
    requests = generate_distributed_requests(config.traffic, config.seed)
    transaction = build_transaction_plan(config)
    cycles = [request.cycle for request in requests]
    sizes = [request.size_bytes for request in requests]
    print(f"Valid experiment config: {args.config}")
    print(
        f"  packets={len(requests)} "
        f"flows={','.join(map(str, config.traffic.flow_ids))} "
        f"trace_cycles={min(cycles)}..{max(cycles)} "
        f"sizes={min(sizes)}..{max(sizes)} bytes"
    )
    print(
        f"  packet_rate={config.traffic.packet_rate.to_dict()} seed={config.seed}"
    )
    print(f"  packet_size_bytes={config.traffic.packet_size_bytes.to_dict()}")
    print(
        f"  reconfiguration={transaction.mode} name={transaction.name} "
        f"labels={transaction.before_label or '-'}->{transaction.after_label or '-'} "
        f"scheduled_cycle={transaction.cycle} "
        f"commands={len(transaction.transaction_commands)}"
    )
    print(
        f"  tree_nodes={len(config.initial_tree.nodes)} "
        f"engines={config.simulation.num_engines} "
        f"path_depths={','.join(str(len(path)) for path in config.initial_tree.flow_paths.values())}"
    )
    if transaction.drain_engine_id is not None:
        print(
            f"  old_tree_drain_root={transaction.drain_engine_id}:"
            f"{transaction.drain_vpifo_id}"
        )
    if config.verification is not None:
        verification = config.verification
        print(
            "  verification="
            f"staging>={verification.minimum_staging_cycles} "
            f"old_backlog>={verification.minimum_old_backlog_packets} "
            f"drain>={verification.minimum_drain_cycles} "
            f"packets_per_phase>={verification.minimum_packets_per_phase}"
        )


def plot_existing(args: argparse.Namespace) -> None:
    generated = generate_figures(
        results_path=args.results,
        event_path=args.events,
        output_dir=args.output_dir,
        link_bytes_per_cycle=args.link_bytes_per_cycle,
        bin_cycles=args.bin_cycles,
        labels=parse_flow_mapping(args.flow_labels),
        dpi=args.dpi,
    )
    print("Generated:")
    for path in generated:
        print(f"  {path}")


def verify_existing(args: argparse.Namespace) -> None:
    config = load_experiment_config(args.config)
    report = run_verification(
        config,
        args.results,
        args.events,
        args.output_dir / "phase-verification.json",
        args.output_dir / "phase-verification.md",
    )
    print("Phase verification: " + ("PASS" if report["passed"] else "FAIL"))
    if not report["passed"]:
        raise RuntimeError(
            f"phase verification failed; see {args.output_dir / 'phase-verification.md'}"
        )


def _add_plot_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--link-bytes-per-cycle",
        type=float,
        default=64.0,
        help="Output link rate used to normalize bandwidth (default 64).",
    )
    parser.add_argument(
        "--bin-cycles",
        type=int,
        default=64,
        help="Bandwidth aggregation window in cycles (default 64).",
    )
    parser.add_argument(
        "--flow-labels",
        help="Optional comma-separated FLOW:LABEL pairs, for example 1:A,2:B.",
    )
    parser.add_argument(
        "--dpi", type=int, default=180, help="Figure DPI (default 180)."
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)

    run = commands.add_parser(
        "run", help="Generate traffic, run one reconfiguration, and render both figures."
    )
    run.add_argument(
        "--config",
        type=Path,
        help=(
            "JSON experiment config. When supplied, it replaces the legacy traffic, "
            "policy, simulation, and plot flags below."
        ),
    )
    run.add_argument(
        "--output-dir",
        type=Path,
        help=(
            "Artifact directory. Overrides output_dir in --config; legacy default is "
            "experiment-results/rr-to-sp."
        ),
    )
    run.add_argument(
        "--flows", default="1,2", help="Comma-separated flow IDs (default 1,2)."
    )
    run.add_argument("--packets-per-flow", type=int, default=80)
    run.add_argument("--start-cycle", type=int, default=0)
    run.add_argument("--arrival-gap-cycles", type=int, default=8)
    run.add_argument("--packet-size-bytes", type=int, default=256)
    run.add_argument("--switch-cycle", type=int, default=320)
    run.add_argument("--policy-before", default="RR")
    run.add_argument("--policy-after", default="SP")
    run.add_argument(
        "--strict-priorities",
        help="FLOW:PRIORITY pairs. Defaults to ascending flow ID priority.",
    )
    run.add_argument("--queue-depth", type=int, default=256)
    run.add_argument("--max-cycles", type=int, default=100000)
    run.add_argument(
        "--max-packet-priority",
        type=int,
        default=65536,
        help="Rank range; 65536 prevents RR rank wrap in the default trace.",
    )
    run.add_argument("--sbt", default="sbt", help="sbt executable (default sbt).")
    _add_plot_options(run)
    run.set_defaults(handler=run_experiment)

    plot = commands.add_parser(
        "plot", help="Render figures from existing result and reconfiguration-event CSVs."
    )
    plot.add_argument("--results", type=Path, required=True)
    plot.add_argument("--events", type=Path, required=True)
    plot.add_argument("--output-dir", type=Path, required=True)
    _add_plot_options(plot)
    plot.set_defaults(handler=plot_existing)

    validate = commands.add_parser(
        "validate",
        help=(
            "Validate a JSON config and preview its generated traffic without "
            "running RTL."
        ),
    )
    validate.add_argument("config", type=Path, help="Experiment JSON file.")
    validate.set_defaults(handler=validate_experiment_config)

    verify = commands.add_parser(
        "verify", help="Verify RR/commit/drain/SP phases in an existing run."
    )
    verify.add_argument("--config", type=Path, required=True)
    verify.add_argument("--results", type=Path, required=True)
    verify.add_argument("--events", type=Path, required=True)
    verify.add_argument("--output-dir", type=Path, required=True)
    verify.set_defaults(handler=verify_existing)
    return parser


def main() -> None:
    args = build_parser().parse_args()
    try:
        args.handler(args)
    except (OSError, RuntimeError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"error: {error}") from error


if __name__ == "__main__":
    main()
