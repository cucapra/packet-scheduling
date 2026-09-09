"""Shared data loading and rendering for the four motivating-example runs."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Mapping, Sequence

from pifo_figures.bandwidth import BandwidthSample, build_samples, write_data
from pifo_figures.common import (
    COLORS,
    FigurePaths,
    PACKET_TRACE_FIELDS,
    PacketOutcome,
    PolicyEvent,
    flow_name,
    load_pyplot,
    commit_accounting,
    drain_label,
    finish_label,
    completed_timings,
    packet_outcome_row,
    read_packet_outcomes,
    write_packet_outcomes,
)
from pifo_figures.standalone import write_plot_script


def throughput_samples(
    outcomes: Sequence[PacketOutcome],
    event: PolicyEvent,
    window_cycles: int,
    sample_cycles: int,
    link_bytes_per_cycle: float,
) -> tuple[list[int], list[BandwidthSample]]:
    return build_samples(
        completed_timings(outcomes),
        event,
        window_cycles,
        sample_cycles,
        link_bytes_per_cycle,
    )


def render_throughput(
    paths: FigurePaths,
    outcomes: Sequence[PacketOutcome],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
    window_cycles: int,
    sample_cycles: int,
    link_bytes_per_cycle: float,
    title: str,
) -> None:
    from pifo_figures.bandwidth import render_matplotlib

    paths.data.parent.mkdir(parents=True, exist_ok=True)
    flow_ids, samples = throughput_samples(
        outcomes,
        event,
        window_cycles,
        sample_cycles,
        link_bytes_per_cycle,
    )
    write_data(paths.data, flow_ids, samples)
    write_packet_outcomes(paths.packets, outcomes, labels)
    write_plot_script(
        paths, "bandwidth", [(title, event)], labels, dpi,
        f"{title}: Hann-smoothed output bandwidth ({event.mode}, {window_cycles}-cycle window)",
    )
    render_matplotlib(paths, flow_ids, samples, event, labels, dpi, title)


def render_delay_scatter(
    paths: FigurePaths,
    outcomes: Sequence[PacketOutcome],
    event: PolicyEvent,
    labels: Mapping[int, str],
    dpi: int,
    title: str,
) -> None:
    paths.data.parent.mkdir(parents=True, exist_ok=True)
    write_packet_outcomes(paths.data, outcomes, labels)
    write_packet_outcomes(paths.packets, outcomes, labels)
    write_plot_script(paths, "delay", [(title, event)], labels, dpi, title)
    plt, line_type = load_pyplot()
    figure, axis = plt.subplots(figsize=(10, 6.5), constrained_layout=True)
    flow_ids = sorted({outcome.flow_id for outcome in outcomes})
    _draw_delay_panel(axis, outcomes, event, labels, flow_ids, line_type)
    axis.set_title(title)
    figure.savefig(paths.svg, bbox_inches="tight")
    figure.savefig(paths.png, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def render_delay_comparison(
    paths: FigurePaths,
    runs: Sequence[tuple[str, Sequence[PacketOutcome], PolicyEvent]],
    labels: Mapping[int, str],
    dpi: int,
) -> None:
    paths.data.parent.mkdir(parents=True, exist_ok=True)
    _write_comparison_manifest(paths.data, runs)
    write_comparison_packets(paths.packets, runs, labels)
    write_plot_script(
        paths, "delay", [(title, event) for title, _, event in runs], labels, dpi,
        "R2–R4 packet-delay comparison (shared axes)", data_name="packets.csv",
    )
    plt, line_type = load_pyplot()
    figure, axes = plt.subplots(
        1, len(runs), figsize=(18, 5.8), sharex=True, sharey=True, constrained_layout=True
    )
    flow_ids = sorted(
        {outcome.flow_id for _, outcomes, _ in runs for outcome in outcomes}
    )
    for axis, (title, outcomes, event) in zip(axes, runs):
        _draw_delay_panel(axis, outcomes, event, labels, flow_ids, line_type)
        axis.set_title(title)
    axes[0].set_ylabel("Per-packet delay (pop − generation cycles)")
    for axis in axes:
        axis.set_xlabel("Generation cycle relative to reconfiguration start")
    figure.suptitle("R2–R4 packet-delay comparison (shared axes)")
    figure.savefig(paths.svg, bbox_inches="tight")
    figure.savefig(paths.png, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def render_throughput_comparison(
    paths: FigurePaths,
    runs: Sequence[
        tuple[str, Sequence[PacketOutcome], PolicyEvent]
    ],
    labels: Mapping[int, str],
    dpi: int,
    window_cycles: int,
    sample_cycles: int,
    link_bytes_per_cycle: float,
) -> None:
    paths.data.parent.mkdir(parents=True, exist_ok=True)
    write_comparison_packets(paths.packets, runs, labels)
    plt, line_type = load_pyplot()
    figure, axes = plt.subplots(
        1, len(runs), figsize=(15, 5.8), sharex=True, sharey=True, constrained_layout=True
    )
    all_rows: list[tuple[str, BandwidthSample]] = []
    flow_ids = sorted(
        {outcome.flow_id for _, outcomes, _ in runs for outcome in outcomes}
    )
    for axis, (title, outcomes, event) in zip(axes, runs):
        _, samples = throughput_samples(
            outcomes,
            event,
            window_cycles,
            sample_cycles,
            link_bytes_per_cycle,
        )
        all_rows.extend((title, sample) for sample in samples)
        x_values = [sample.time_relative_to_start for sample in samples]
        axis.plot(
            x_values,
            [sample.total_link_fraction for sample in samples],
            color="black",
            linewidth=2.2,
            label="total",
        )
        for index, flow_id in enumerate(flow_ids):
            axis.plot(
                x_values,
                [sample.flow_link_fraction.get(flow_id, 0.0) for sample in samples],
                color=COLORS[index % len(COLORS)],
                linewidth=1.8,
                label=flow_name(flow_id, labels),
            )
        _draw_event_lines(axis, event)
        axis.axhline(1.0, color="0.55", linewidth=1, linestyle=":")
        axis.grid(True, color="0.9", linewidth=0.8)
        axis.set_title(title)
        axis.set_xlabel("Cycle relative to reconfiguration start")
    axes[0].set_ylabel("Output throughput / link capacity")
    axes[-1].legend(loc="best")
    figure.suptitle(
        f"R3 whole-tree vs R4 confined throughput ({window_cycles}-cycle Hann window)"
    )
    _write_throughput_comparison(paths.data, all_rows, flow_ids)
    write_plot_script(
        paths, "throughput-comparison", [(title, event) for title, _, event in runs], labels, dpi,
        f"R3 whole-tree vs R4 confined throughput ({window_cycles}-cycle Hann window)",
    )
    figure.savefig(paths.svg, bbox_inches="tight")
    figure.savefig(paths.png, dpi=dpi, bbox_inches="tight")
    plt.close(figure)


def _draw_delay_panel(
    axis,
    outcomes: Sequence[PacketOutcome],
    event: PolicyEvent,
    labels: Mapping[int, str],
    flow_ids: Sequence[int],
    line_type,
) -> None:
    for index, flow_id in enumerate(flow_ids):
        completed = [
            outcome
            for outcome in outcomes
            if outcome.flow_id == flow_id and not outcome.dropped
        ]
        axis.scatter(
            [outcome.push_cycle - event.start_cycle for outcome in completed],
            [outcome.delay for outcome in completed],
            s=10,
            alpha=0.55,
            color=COLORS[index % len(COLORS)],
            label=flow_name(flow_id, labels),
        )
        dropped = [
            outcome
            for outcome in outcomes
            if outcome.flow_id == flow_id and outcome.dropped
        ]
        if dropped:
            axis.scatter(
                [outcome.push_cycle - event.start_cycle for outcome in dropped],
                [0] * len(dropped),
                s=20,
                marker="x",
                linewidths=0.8,
                color=COLORS[index % len(COLORS)],
            )
    _draw_event_lines(axis, event)
    axis.axhline(0, color="0.45", linewidth=1, linestyle=":")
    axis.grid(True, color="0.92", linewidth=0.8)
    axis.margins(x=0.02, y=0.05)
    handles, legend_labels = axis.get_legend_handles_labels()
    if any(outcome.dropped for outcome in outcomes):
        handles.append(line_type([0], [0], color="0.25", marker="x", linestyle="None"))
        legend_labels.append("dropped (shown at y=0)")
    axis.legend(handles, legend_labels, loc="best", markerscale=1.5)
    if event.mode == "stop_the_world":
        axis.text(
            0.02,
            0.98,
            f"retained at capture: {event.retained_packets} packets\n"
            f"peak buffer occupancy: {event.peak_buffer_occupancy_packets} packets",
            transform=axis.transAxes,
            va="top",
            fontsize=8.5,
            bbox={"boxstyle": "round", "facecolor": "white", "alpha": 0.8},
        )
    axis.set_xlabel("Generation cycle relative to reconfiguration start")
    axis.set_ylabel("Per-packet delay (pop − generation cycles)")


def _draw_event_lines(axis, event: PolicyEvent) -> None:
    markers = (
        (0, "tab:blue", "-", "start"),
        (event.commit_cycle - event.start_cycle, "tab:orange", "--", "commit accepted"),
        (event.drain_cycle - event.start_cycle, "tab:purple", ":", drain_label(event))
        if event.drain_cycle is not None
        else None,
        (event.finish_cycle - event.start_cycle, "tab:green", "-.", finish_label(event)),
    )
    for marker in markers:
        if marker is None:
            continue
        value, color, style, _label = marker
        axis.axvline(
            value,
            color=color,
            linewidth=1.15,
            linestyle=style,
            alpha=0.9,
            label=_label,
        )
    if event.resume_cycle is not None:
        axis.axvline(event.resume_cycle - event.start_cycle, color="0.4", linestyle="--", label="traffic resumed")
    accounting = commit_accounting(event)
    if accounting:
        axis.text(0.01, 0.01, accounting.replace("; ", "\n"), transform=axis.transAxes,
                  va="bottom", fontsize=7, color="0.35")


def write_comparison_packets(
    path: Path,
    runs: Sequence[tuple[str, Sequence[PacketOutcome], PolicyEvent]],
    labels: Mapping[int, str] | None = None,
) -> None:
    """Include every run's raw packets, even for averaged throughput figures."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(("run", *PACKET_TRACE_FIELDS))
        for title, outcomes, _ in runs:
            for packet in sorted(outcomes, key=lambda packet: packet.request_id):
                writer.writerow((title, *packet_outcome_row(packet, labels or {})))


def _write_comparison_manifest(
    path: Path,
    runs: Sequence[tuple[str, Sequence[PacketOutcome], PolicyEvent]],
) -> None:
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(("run", "packets", "dropped", "start", "commit", "drain", "finish",
                         "instruction_count", "commit_cycles", "cleanup_instruction_count", "cleanup_commit_cycles"))
        for title, outcomes, event in runs:
            writer.writerow(
                (
                    title,
                    len(outcomes),
                    sum(outcome.dropped for outcome in outcomes),
                    event.start_cycle,
                    event.commit_cycle,
                    event.drain_cycle if event.drain_cycle is not None else "",
                    event.finish_cycle,
                    event.instruction_count,
                    event.commit_cycles,
                    event.cleanup_instruction_count,
                    event.cleanup_commit_cycles,
                )
            )


def _write_throughput_comparison(
    path: Path,
    rows: Sequence[tuple[str, BandwidthSample]],
    flow_ids: Sequence[int],
) -> None:
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(
            ("run", "time_relative_to_start", "total_link_fraction")
            + tuple(f"flow_{flow_id}_link_fraction" for flow_id in flow_ids)
        )
        for run, sample in rows:
            writer.writerow(
                (
                    run,
                    sample.time_relative_to_start,
                    sample.total_link_fraction,
                    *(sample.flow_link_fraction.get(flow_id, 0.0) for flow_id in flow_ids),
                )
            )
