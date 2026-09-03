"""Shared data loading and rendering for the four motivating-example runs."""

from __future__ import annotations

import csv
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping, Sequence

from pifo_figures.bandwidth import BandwidthSample, build_samples, write_data
from pifo_figures.common import (
    COLORS,
    FigurePaths,
    PacketTiming,
    PolicyEvent,
    flow_name,
    load_pyplot,
)


OUTCOME_FIELDS = {
    "request_id",
    "flow",
    "size_bytes",
    "push_cycle",
    "pop_cycle",
    "dropped",
}


@dataclass(frozen=True)
class PacketOutcome:
    request_id: int
    flow_id: int
    size_bytes: int
    push_cycle: int
    pop_cycle: int | None
    dropped: bool

    @property
    def delay(self) -> int | None:
        return None if self.pop_cycle is None else self.pop_cycle - self.push_cycle


def read_packet_outcomes(path: Path) -> list[PacketOutcome]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        missing = OUTCOME_FIELDS.difference(reader.fieldnames or ())
        if missing:
            raise ValueError(
                f"{path}: missing packet outcome fields: {', '.join(sorted(missing))}"
            )
        outcomes: list[PacketOutcome] = []
        seen: set[int] = set()
        for line_number, row in enumerate(reader, start=2):
            try:
                dropped = _parse_bool(row["dropped"])
                pop_text = row["pop_cycle"].strip()
                outcome = PacketOutcome(
                    request_id=int(row["request_id"], 0),
                    flow_id=int(row["flow"], 0),
                    size_bytes=int(row["size_bytes"], 0),
                    push_cycle=int(row["push_cycle"], 0),
                    pop_cycle=int(pop_text, 0) if pop_text else None,
                    dropped=dropped,
                )
                _validate_outcome(outcome, seen)
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error
            outcomes.append(outcome)
            seen.add(outcome.request_id)
    if not outcomes:
        raise ValueError(f"{path}: no packet outcomes")
    return outcomes


def completed_timings(outcomes: Sequence[PacketOutcome]) -> list[PacketTiming]:
    return [
        PacketTiming(
            request_id=outcome.request_id,
            flow_id=outcome.flow_id,
            size_bytes=outcome.size_bytes,
            input_cycle=outcome.push_cycle,
            output_cycle=outcome.pop_cycle,
        )
        for outcome in outcomes
        if not outcome.dropped and outcome.pop_cycle is not None
    ]


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
    _write_delay_data(paths.data, outcomes, event)
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
    axes[0].set_ylabel("Per-packet delay (pop − push cycles)")
    for axis in axes:
        axis.set_xlabel("Push cycle relative to reconfiguration start")
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
    axis.set_xlabel("Push cycle relative to reconfiguration start")
    axis.set_ylabel("Per-packet delay (pop − push cycles)")


def _draw_event_lines(axis, event: PolicyEvent) -> None:
    finish_label = "traffic resumed" if event.mode == "stop_the_world" else "config sync done"
    drain_label = "old tree captured" if event.mode == "stop_the_world" else "old tree drained"
    markers = (
        (0, "tab:blue", "-", "start"),
        (event.commit_cycle - event.start_cycle, "tab:orange", "--", "commit accepted"),
        (event.drain_cycle - event.start_cycle, "tab:purple", ":", drain_label)
        if event.drain_cycle is not None
        else None,
        (event.finish_cycle - event.start_cycle, "tab:green", "-.", finish_label),
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


def _write_delay_data(
    path: Path, outcomes: Sequence[PacketOutcome], event: PolicyEvent
) -> None:
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(
            ("request_id", "flow", "push_cycle", "pop_cycle", "delay_cycles", "dropped")
        )
        for outcome in outcomes:
            writer.writerow(
                (
                    outcome.request_id,
                    outcome.flow_id,
                    outcome.push_cycle,
                    outcome.pop_cycle if outcome.pop_cycle is not None else "",
                    outcome.delay if outcome.delay is not None else "",
                    str(outcome.dropped).lower(),
                )
            )


def _write_comparison_manifest(
    path: Path,
    runs: Sequence[tuple[str, Sequence[PacketOutcome], PolicyEvent]],
) -> None:
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(("run", "packets", "dropped", "start", "commit", "drain", "finish"))
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


def _validate_outcome(outcome: PacketOutcome, seen: set[int]) -> None:
    if outcome.request_id in seen:
        raise ValueError(f"duplicate request ID {outcome.request_id}")
    if outcome.flow_id < 0 or outcome.size_bytes <= 0 or outcome.push_cycle < 0:
        raise ValueError("flow/push must be non-negative and size must be positive")
    if outcome.dropped != (outcome.pop_cycle is None):
        raise ValueError("dropped must be true exactly when pop_cycle is blank")
    if outcome.pop_cycle is not None and outcome.pop_cycle < outcome.push_cycle:
        raise ValueError("pop_cycle precedes push_cycle")


def _parse_bool(value: str) -> bool:
    normalized = value.strip().lower()
    if normalized == "true":
        return True
    if normalized == "false":
        return False
    raise ValueError(f"invalid boolean {value!r}")
