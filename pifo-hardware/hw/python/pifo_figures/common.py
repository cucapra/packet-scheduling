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
from typing import Iterable, Mapping, Protocol, Sequence

from request_trace import read_trace


RESULT_FIELDS = {
    "request_id",
    "global_flow_id",
    "size_bytes",
    "arrival_cycle",
    "completed_cycle",
}
OUTCOME_FIELDS = {"request_id", "flow", "size_bytes", "push_cycle", "pop_cycle", "dropped"}
PACKET_TRACE_FIELDS = (
    "request_id", "flow", "flow_name", "size_bytes", "push_cycle", "pop_cycle", "delay_cycles", "dropped",
)
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
FINISH_LABEL = "ready_for_next_commit"
COMMIT_BACKGROUNDS = ("#dbeafe", "#ffedd5")


@dataclass(frozen=True)
class PacketTiming:
    request_id: int
    flow_id: int
    size_bytes: int
    input_cycle: int
    output_cycle: int


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
            raise ValueError(f"{path}: missing packet outcome fields: {', '.join(sorted(missing))}")
        outcomes: list[PacketOutcome] = []
        seen: set[int] = set()
        for line_number, row in enumerate(reader, start=2):
            try:
                dropped = row["dropped"].strip().lower()
                if dropped not in {"true", "false"}:
                    raise ValueError(f"invalid boolean {row['dropped']!r}")
                pop = row["pop_cycle"].strip()
                outcome = PacketOutcome(
                    int(row["request_id"], 0), int(row["flow"], 0), int(row["size_bytes"], 0),
                    int(row["push_cycle"], 0), int(pop, 0) if pop else None, dropped == "true",
                )
                if outcome.request_id in seen:
                    raise ValueError(f"duplicate request ID {outcome.request_id}")
                if min(outcome.request_id, outcome.flow_id, outcome.push_cycle) < 0 or outcome.size_bytes <= 0:
                    raise ValueError("ID/flow/push must be non-negative and size must be positive")
                if outcome.dropped != (outcome.pop_cycle is None):
                    raise ValueError("dropped must be true exactly when pop_cycle is blank")
                if outcome.delay is not None and outcome.delay < 0:
                    raise ValueError("pop_cycle precedes push_cycle")
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error
            outcomes.append(outcome)
            seen.add(outcome.request_id)
    if not outcomes:
        raise ValueError(f"{path}: no packet outcomes")
    return outcomes


def completed_timings(outcomes: Sequence[PacketOutcome]) -> list[PacketTiming]:
    return [
        PacketTiming(p.request_id, p.flow_id, p.size_bytes, p.push_cycle, p.pop_cycle)
        for p in outcomes if not p.dropped and p.pop_cycle is not None
    ]


def outcomes_from_timings(packets: Sequence[PacketTiming]) -> tuple[PacketOutcome, ...]:
    return tuple(PacketOutcome(p.request_id, p.flow_id, p.size_bytes, p.input_cycle, p.output_cycle, False)
                 for p in packets)


def packet_outcome_row(packet: PacketOutcome, labels: Mapping[int, str]) -> tuple:
    return (packet.request_id, packet.flow_id, flow_name(packet.flow_id, labels), packet.size_bytes,
            packet.push_cycle, packet.pop_cycle, packet.delay, str(packet.dropped).lower())


def write_packet_outcomes(
    path: Path, outcomes: Iterable[PacketOutcome], labels: Mapping[int, str] | None = None,
) -> None:
    """One row per generated packet; blank pop/delay for drops, never admission-time delay."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.writer(destination, lineterminator="\n")
        writer.writerow(PACKET_TRACE_FIELDS)
        writer.writerows(packet_outcome_row(p, labels or {}) for p in sorted(outcomes, key=lambda p: p.request_id))


def read_run_packet_outcomes(results: Path, outcomes: Path | None = None) -> tuple[PacketOutcome, ...]:
    """Prefer the complete simulator trace; only reconstruct provably lossless legacy runs."""
    packets = read_packet_results(results)
    outcome_path = outcomes if outcomes is not None else results.with_name("packet-outcomes.csv")
    requests_path = results.with_name("requests.csv")
    if outcomes is not None or outcome_path.exists():
        trace = tuple(read_packet_outcomes(outcome_path))
        completed = {p.request_id: p for p in completed_timings(trace)}
        if completed != {p.request_id: p for p in packets}:
            raise ValueError(f"{outcome_path}: completed outcomes disagree with {results}")
    else:
        if not requests_path.exists():
            raise ValueError("provide --outcomes (or a matching requests.csv); completion results alone cannot account for drops")
        trace = outcomes_from_timings(packets)
    if requests_path.exists():
        expected = {r.request_id: (r.global_flow_id, r.size_bytes, r.cycle) for r in read_trace(requests_path)}
        actual = {p.request_id: (p.flow_id, p.size_bytes, p.push_cycle) for p in trace}
        if actual != expected:
            raise ValueError(f"{results}: packet outcomes must cover every generated request with its generation cycle; "
                             "missing outcomes cannot be inferred as dropped")
    return trace


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
    dropped_packets: int = 0
    retained_packets: int = 0
    peak_buffer_occupancy_packets: int = 0
    minimum_stop_cycles: int = 0
    stop_duration_cycles: int | None = None
    commit_applied_cycle: int | None = None
    commit_cycles: int | None = None
    bank_cleanup_cycles: int | None = None
    install_finish_cycle: int | None = None
    resume_cycle: int | None = None
    cleanup_start_cycle: int | None = None
    cleanup_commit_cycle: int | None = None
    cleanup_applied_cycle: int | None = None
    cleanup_finish_cycle: int | None = None
    cleanup_instruction_count: int | None = None
    cleanup_commit_cycles: int | None = None
    cleanup_bank_cleanup_cycles: int | None = None

    @property
    def traffic_resume_cycle(self) -> int:
        # Old CSVs used finish for STW resume. New ones record it separately.
        return self.resume_cycle if self.resume_cycle is not None else self.finish_cycle

    @property
    def label(self) -> str:
        if self.before and self.after:
            return f"{self.before} → {self.after}"
        return self.name or self.mode


def drain_label(event: PolicyEvent) -> str:
    if event.mode == "stop_the_world":
        return "old tree captured"
    if event.mode == "in_place":
        return "no old-tree drain required"
    return "old tree drained"


def finish_label(event: PolicyEvent) -> str:
    if event.mode == "stop_the_world" and event.install_finish_cycle is None:
        return "traffic resumed (legacy finish)"
    return FINISH_LABEL


def commit_windows(event: PolicyEvent) -> list[tuple[str, int, int, int]]:
    """Separate bank readiness after installation from guarded reclamation.

    Old traces contain one commit only. A cleanup commit shares the retired
    tree's drain event; it does not create a second tree to drain.
    """
    ready = getattr(event, "install_finish_cycle", None)
    windows = [("C1", event.start_cycle, event.commit_cycle,
                event.finish_cycle if ready is None else ready)]
    cleanup_start = getattr(event, "cleanup_start_cycle", None)
    if cleanup_start is not None:
        windows.append(("C2", cleanup_start, event.cleanup_commit_cycle, event.cleanup_finish_cycle))
    return windows


def timeline_markers(event: PolicyEvent) -> list[tuple[int, str, str, str]]:
    markers = []
    for name, start, accepted, ready in commit_windows(event):
        markers.extend((
            (start - event.start_cycle, START_COLOR, "-", f"{name} start"),
            (accepted - event.start_cycle, COMMIT_COLOR, "--", f"{name} commit accepted"),
            (ready - event.start_cycle, FINISH_COLOR, "-.", f"{name} {finish_label(event)}"),
        ))
        if event.drain_cycle is not None:
            label = "old-tree-drained"
            if event.mode == "in_place":
                label += " (not required)"
            elif event.mode == "stop_the_world":
                label = "old-tree captured (not drained)"
            markers.append((event.drain_cycle - event.start_cycle, DRAIN_COLOR, ":", f"{name} {label}"))
    if getattr(event, "resume_cycle", None) is not None:
        markers.append((event.resume_cycle - event.start_cycle, "0.4", "--", "traffic resumed"))
    return markers


def timeline_spans(event: PolicyEvent) -> list[tuple[int, int, str, str]]:
    return [(start - event.start_cycle, ready - event.start_cycle, COMMIT_BACKGROUNDS[index],
             f"{name}: {'install' if index == 0 else 'cleanup'} commit")
            for index, (name, start, _, ready) in enumerate(commit_windows(event))]


def draw_timeline(axis, event: PolicyEvent, horizontal: bool = False) -> None:
    """Time markers on x (and on y only when y is absolute output time)."""
    for start, ready, color, label in timeline_spans(event):
        axis.axvspan(start, ready, color=color, alpha=0.65, linewidth=0, zorder=0, label=label)
        if horizontal:
            axis.axhspan(start, ready, color=color, alpha=0.35, linewidth=0, zorder=0)
    for cycle, color, style, label in timeline_markers(event):
        width = 2.3 if FINISH_LABEL in label else 1.15
        axis.axvline(cycle, color=color, linestyle=style, linewidth=width, label=label)
        if horizontal:
            axis.axhline(cycle, color=color, linestyle=style, linewidth=width, gid="commit-time-y")


def timeline_legend(axis, event: PolicyEvent) -> None:
    """Keep the two commit legends out of the packet data; label exact cycles."""
    handles, labels = axis.get_legend_handles_labels()
    indexed = dict(zip(labels, handles))
    entries = []
    for span in timeline_spans(event):
        name = span[3].split(":")[0]
        entries.append((indexed[span[3]], span[3]))
        entries.extend((indexed[label], f"{label} = {cycle + event.start_cycle}")
                       for cycle, _, _, label in timeline_markers(event) if label.startswith(name + " "))
    data = [(handle, f"traffic resumed = {event.resume_cycle}" if label == "traffic resumed" else label)
            for handle, label in zip(handles, labels) if not label.startswith(("C1", "C2"))]
    if data:
        data_legend = axis.legend(*zip(*data), loc="upper right", fontsize=8)
        axis.add_artist(data_legend)
    key = axis.legend(*zip(*entries), loc="upper center", bbox_to_anchor=(0.5, -0.16),
                      ncol=len(commit_windows(event)), fontsize=7.5)
    axis.annotate(timeline_notes(event), xy=(0.5, 0), xycoords=key, xytext=(0, -6),
                  textcoords="offset points", ha="center", va="top", fontsize=7,
                  color="0.35", annotation_clip=False)


def commit_accounting(event: PolicyEvent) -> str:
    parts = []
    if event.instruction_count is not None:
        cycles = event.commit_cycles
        endpoint = "publication" if cycles is not None else "acceptance"
        if cycles is None:
            cycles = event.commit_cycle - event.start_cycle
        parts.append(f"config={event.instruction_count} inst / {cycles} cycles to {endpoint}")
    if event.cleanup_instruction_count is not None:
        parts.append(
            f"cleanup={event.cleanup_instruction_count} inst / "
            f"{event.cleanup_commit_cycles} cycles to publication (guard wait included)"
        )
    if event.bank_cleanup_cycles is not None:
        banks = f"bank replay: install={event.bank_cleanup_cycles}"
        if event.cleanup_bank_cleanup_cycles is not None:
            banks += f", cleanup={event.cleanup_bank_cleanup_cycles}"
        parts.append(banks + " cycles")
    return "; ".join(parts)


def timeline_notes(event: PolicyEvent) -> str:
    notes = commit_accounting(event).replace("; ", "\n")
    if event.stop_duration_cycles is not None:
        notes += (f"\nSTW stop={event.stop_duration_cycles} cycles; retained={event.retained_packets}; "
                  f"peak buffer={event.peak_buffer_occupancy_packets} packets")
    return notes


def timing_text(event: PolicyEvent, unicode_limit: bool = True) -> str:
    text = "\n".join(
        f"{name}: start={start}  commit accepted={accepted}  ready_for_next_commit={ready}"
        for name, start, accepted, ready in commit_windows(event)
    )
    text += f"\n{drain_label(event)}={event.drain_cycle if event.drain_cycle is not None else '-'}"
    if len(commit_windows(event)) > 1:
        text += " (shared by C1/C2)"
    if event.commit_applied_cycle is not None:
        text += f"\npublished: install={event.commit_applied_cycle}"
    if event.cleanup_applied_cycle is not None:
        text += f", cleanup={event.cleanup_applied_cycle}"
    accounting = commit_accounting(event)
    if accounting:
        limit = "≤" if unicode_limit else "<="
        text += "\n" + accounting.replace("; ", "\n")
        text += f"; {limit}1 instruction accepted/cycle"
    if event.stop_duration_cycles is not None:
        text += (
            f"\nresumed={event.traffic_resume_cycle}  retained={event.retained_packets}  "
            f"peak buffer={event.peak_buffer_occupancy_packets} packets  "
            f"stop={event.stop_duration_cycles} cycles"
        )
    if event.dropped_packets:
        text += f"  dropped={event.dropped_packets}"
    return text


@dataclass(frozen=True)
class FigureInputs:
    packets: tuple[PacketTiming, ...]
    event: PolicyEvent
    labels: Mapping[int, str]
    dpi: int
    output_dir: Path
    outcomes: tuple[PacketOutcome, ...] | None = None

    @property
    def packet_outcomes(self) -> tuple[PacketOutcome, ...]:
        return self.outcomes if self.outcomes is not None else outcomes_from_timings(self.packets)


@dataclass(frozen=True)
class FigurePaths:
    data: Path
    svg: Path
    png: Path

    @property
    def packets(self) -> Path:
        return self.data.parent / "packets.csv"


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
    parser.add_argument("--outcomes", type=Path,
                        help="Complete packet trace, including drops. Defaults to packet-outcomes.csv beside --results.")
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
    outcomes = read_run_packet_outcomes(args.results, args.outcomes)
    return FigureInputs(
        packets=tuple(completed_timings(outcomes)),
        event=read_policy_event(args.events),
        labels=parse_flow_mapping(args.flow_labels),
        dpi=args.dpi,
        output_dir=args.output_dir.resolve(),
        outcomes=outcomes,
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
        dropped_packets=(
            parse_int(row["dropped_packets"])
            if (row.get("dropped_packets") or "").strip()
            else 0
        ),
        retained_packets=(
            parse_int(row["retained_packets"])
            if (row.get("retained_packets") or "").strip()
            else 0
        ),
        peak_buffer_occupancy_packets=(
            parse_int(row["peak_buffer_occupancy_packets"])
            if (row.get("peak_buffer_occupancy_packets") or "").strip()
            else 0
        ),
        minimum_stop_cycles=(
            parse_int(row["minimum_stop_cycles"])
            if (row.get("minimum_stop_cycles") or "").strip()
            else 0
        ),
        stop_duration_cycles=(
            parse_int(row["stop_duration_cycles"])
            if (row.get("stop_duration_cycles") or "").strip()
            else None
        ),
        **{
            name: parse_int(row[name]) if (row.get(name) or "").strip() else None
            for name in (
                "commit_applied_cycle", "commit_cycles", "bank_cleanup_cycles",
                "install_finish_cycle", "resume_cycle", "cleanup_start_cycle",
                "cleanup_commit_cycle", "cleanup_applied_cycle", "cleanup_finish_cycle",
                "cleanup_instruction_count", "cleanup_commit_cycles", "cleanup_bank_cleanup_cycles",
            )
        },
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
    if (
        event.drain_cycle is not None
        and event.drain_cycle < event.commit_cycle
        and event.mode != "stop_the_world"
    ):
        raise ValueError(f"{path}: drain cycle precedes commit cycle")
    if event.instruction_count is not None and event.instruction_count <= 0:
        raise ValueError(f"{path}: instruction count must be positive")
    if event.dropped_packets < 0:
        raise ValueError(f"{path}: dropped packet count must be non-negative")
    if (
        event.retained_packets < 0
        or event.peak_buffer_occupancy_packets < 0
        or event.minimum_stop_cycles < 0
    ):
        raise ValueError(
            f"{path}: retained packets, peak occupancy, and minimum stop "
            "must be non-negative"
        )
    if (
        (row.get("peak_buffer_occupancy_packets") or "").strip()
        and event.peak_buffer_occupancy_packets < event.retained_packets
    ):
        raise ValueError(f"{path}: peak occupancy is below retained packets")
    if event.stop_duration_cycles is not None:
        if event.mode != "stop_the_world" or event.drain_cycle is None:
            raise ValueError(f"{path}: stop duration requires a stop_the_world drain cycle")
        if event.stop_duration_cycles != event.traffic_resume_cycle - event.drain_cycle:
            raise ValueError(f"{path}: stop duration does not match resume and drain cycles")
        if event.stop_duration_cycles < event.minimum_stop_cycles:
            raise ValueError(f"{path}: stop duration is shorter than its configured minimum")
    duration_raw = (row.get("drain_duration_cycles") or "").strip()
    if duration_raw:
        if event.drain_cycle is None:
            raise ValueError(f"{path}: drain duration is present without drain cycle")
        if parse_int(duration_raw) != event.drain_cycle - event.commit_cycle:
            raise ValueError(
                f"{path}: drain duration does not match drain and commit cycles"
            )
    if event.commit_applied_cycle is not None:
        if not event.commit_cycle <= event.commit_applied_cycle <= event.finish_cycle:
            raise ValueError(f"{path}: commit publication must follow acceptance and precede finish")
        if event.commit_cycles != event.commit_applied_cycle - event.start_cycle:
            raise ValueError(f"{path}: commit cycles do not match start and publication")
        if event.install_finish_cycle is None or event.bank_cleanup_cycles != (
            event.install_finish_cycle - event.commit_applied_cycle
        ) or event.bank_cleanup_cycles < 0:
            raise ValueError(f"{path}: invalid install double-buffer cleanup timing")
    if event.cleanup_finish_cycle is not None:
        timing = (event.cleanup_start_cycle, event.cleanup_commit_cycle,
                  event.cleanup_applied_cycle, event.cleanup_finish_cycle)
        if any(value is None for value in timing) or list(timing) != sorted(timing):
            raise ValueError(f"{path}: invalid cleanup commit ordering")
        if event.finish_cycle != event.cleanup_finish_cycle:
            raise ValueError(f"{path}: finish must mark final double-buffer cleanup")
        if event.cleanup_commit_cycles != event.cleanup_applied_cycle - event.cleanup_start_cycle:
            raise ValueError(f"{path}: invalid cleanup commit duration")
        if event.cleanup_bank_cleanup_cycles != event.finish_cycle - event.cleanup_applied_cycle:
            raise ValueError(f"{path}: invalid final double-buffer cleanup duration")
        if event.cleanup_instruction_count is None or event.cleanup_instruction_count <= 0:
            raise ValueError(f"{path}: cleanup instruction count must be positive")


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
        content = "".join(
            f'<tspan x="{x:.2f}" dy="{0 if index == 0 else size * 1.25:.2f}">{html.escape(line)}</tspan>'
            for index, line in enumerate(value.splitlines())
        ) if "\n" in value else html.escape(value)
        self.add(
            f'<text x="{x:.2f}" y="{y:.2f}" '
            'font-family="DejaVu Sans, sans-serif" '
            f'font-size="{size:.2f}" text-anchor="{anchor}" fill="{color}" '
            f'font-weight="{weight}"{transform}>{content}</text>'
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
    for start, ready, color, _ in timeline_spans(event):
        left, right = max(start, area.x_min), min(ready, area.x_max)
        if right > left:
            svg.rect(
                area.sx(left), area.y, area.sx(right) - area.sx(left), area.height, color, opacity=0.65,
            )
    for value, color, style, label in timeline_markers(event):
        if not include_finish and FINISH_LABEL in label:
            continue
        if area.x_min <= value <= area.x_max:
            svg.line(
                area.sx(value),
                area.y,
                area.sx(value),
                area.y + area.height,
                color,
                2,
                dash={"-": None, "--": "8,6", "-.": "8,4,2,4", ":": "3,5"}[style],
                opacity=0.9,
            )


def scatter_output_markers(svg: Svg, area: PlotArea, event: EventLike) -> None:
    for start, ready, color, _ in timeline_spans(event):
        bottom, top = max(start, area.y_min), min(ready, area.y_max)
        if top > bottom:
            svg.rect(area.x, area.sy(top), area.width, area.sy(bottom) - area.sy(top), color, opacity=0.35)
    for value, color, style, _ in timeline_markers(event):
        if area.y_min <= value <= area.y_max:
            svg.line(
                area.x,
                area.sy(value),
                area.x + area.width,
                area.sy(value),
                color,
                2,
                dash={"-": None, "--": "8,6", "-.": "8,4,2,4", ":": "3,5"}[style],
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
