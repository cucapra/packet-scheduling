#!/usr/bin/env python3
"""Validate packet and transition invariants for the motivating example."""

from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path
from typing import Sequence

from pifo_figures.common import PolicyEvent, read_policy_event
from pifo_motivation_plot import PacketOutcome, read_packet_outcomes


EXPECTED_MODES = {
    "r1-add": "in_place",
    "r2-stop-the-world": "stop_the_world",
    "r3-whole-tree": "full_transitive",
    "r4-confined": "confined_transitive",
}


def validate_run(case: str, run_dir: Path) -> tuple[list[PacketOutcome], PolicyEvent]:
    if case not in EXPECTED_MODES:
        raise ValueError(f"unknown motivating-example case {case!r}")
    outcomes = read_packet_outcomes(run_dir / "packet-outcomes.csv")
    event = read_policy_event(run_dir / "reconfiguration-events.csv")
    requests = _read_requests(run_dir / "requests.csv")

    if event.mode != EXPECTED_MODES[case]:
        raise ValueError(
            f"{case}: expected mode {EXPECTED_MODES[case]!r}, got {event.mode!r}"
        )
    if event.instruction_count is None:
        raise ValueError(f"{case}: event does not record its instruction count")
    if set(requests) != {outcome.request_id for outcome in outcomes}:
        raise ValueError(f"{case}: packet outcomes do not cover the input trace exactly")

    for outcome in outcomes:
        flow_id, size_bytes, arrival_cycle = requests[outcome.request_id]
        if (outcome.flow_id, outcome.size_bytes) != (flow_id, size_bytes):
            raise ValueError(f"{case}: request {outcome.request_id} metadata changed")
        if outcome.push_cycle < arrival_cycle:
            raise ValueError(f"{case}: request {outcome.request_id} was pushed before arrival")

    dropped = sum(outcome.dropped for outcome in outcomes)
    if dropped != event.dropped_packets:
        raise ValueError(
            f"{case}: raw drop count {dropped} differs from event count "
            f"{event.dropped_packets}"
        )
    if dropped:
        raise ValueError(f"{case}: unexpectedly dropped {dropped} packets")
    if case == "r2-stop-the-world":
        if event.retained_packets <= 0:
            raise ValueError("r2-stop-the-world: no queued packets were retained")
        if event.minimum_stop_cycles < 1000:
            raise ValueError(
                "r2-stop-the-world: minimum stop must be at least 1000 cycles"
            )
        if (
            event.stop_duration_cycles is None
            or event.stop_duration_cycles < event.minimum_stop_cycles
        ):
            raise ValueError(
                "r2-stop-the-world: measured stop is shorter than its minimum"
            )
        completions_during_stop = [
            outcome
            for outcome in outcomes
            if outcome.pop_cycle is not None
            and event.drain_cycle is not None
            and event.drain_cycle < outcome.pop_cycle < event.finish_cycle
        ]
        if completions_during_stop:
            raise ValueError("r2-stop-the-world: packets completed while traffic was stopped")

    _validate_per_flow_fifo(case, outcomes)
    return outcomes, event


def validate_comparison(output_root: Path) -> str:
    validated = {
        case: validate_run(case, output_root / case) for case in EXPECTED_MODES
    }
    r3_event = validated["r3-whole-tree"][1]
    r4_event = validated["r4-confined"][1]
    if r3_event.drain_cycle is None or r4_event.drain_cycle is None:
        raise ValueError("R3 and R4 must both record an old-tree drain")
    r3_drain = r3_event.drain_cycle - r3_event.commit_cycle
    r4_drain = r4_event.drain_cycle - r4_event.commit_cycle
    if r3_drain >= r4_drain:
        raise ValueError(
            f"expected R3 to drain sooner than R4, got {r3_drain} >= {r4_drain} cycles"
        )

    request_traces = {
        case: _read_requests(output_root / case / "requests.csv")
        for case in EXPECTED_MODES
    }
    first_trace = request_traces["r1-add"]
    if any(trace != first_trace for trace in request_traces.values()):
        raise ValueError("R1-R4 did not replay an identical request trace")

    r1_event = validated["r1-add"][1]
    r1_zoom_max = _maximum_delay(validated["r1-add"][0], r1_event, 1)
    r3_zoom_max = _maximum_delay(validated["r3-whole-tree"][0], r3_event, 1)
    r4_zoom_max = _maximum_delay(validated["r4-confined"][0], r4_event, 1)
    if r3_zoom_max <= 4 * r4_zoom_max:
        raise ValueError(
            "R3 does not show the expected whole-tree zoom delay spike: "
            f"R3 max={r3_zoom_max}, R4 max={r4_zoom_max}"
        )
    if r4_zoom_max > r1_zoom_max + 3:
        raise ValueError(
            "R4 perturbs zoom beyond the additive baseline: "
            f"R4 max={r4_zoom_max}, R1 max={r1_zoom_max}"
        )

    lines = ["motivating-example validation: PASS"]
    for case, (outcomes, event) in validated.items():
        lines.append(
            f"{case}: packets={len(outcomes)} dropped={event.dropped_packets} "
            f"instructions={event.instruction_count} start={event.start_cycle} "
            f"commit={event.commit_cycle} drain={event.drain_cycle} "
            f"finish={event.finish_cycle}"
        )
    lines.append(f"drain duration: R3={r3_drain} cycles, R4={r4_drain} cycles")
    r2_event = validated["r2-stop-the-world"][1]
    r2_gap = _maximum_completion_gap(validated["r2-stop-the-world"][0])
    if r2_gap < r2_event.minimum_stop_cycles:
        raise ValueError(
            f"R2 output gap {r2_gap} is shorter than its configured minimum "
            f"{r2_event.minimum_stop_cycles}"
        )
    lines.append(
        f"R2 lossless stop: retained={r2_event.retained_packets} "
        f"duration={r2_event.stop_duration_cycles} cycles "
        f"minimum={r2_event.minimum_stop_cycles} cycles output_gap={r2_gap} cycles"
    )
    lines.append(
        f"post-start zoom max delay: R1={r1_zoom_max}, "
        f"R3={r3_zoom_max}, R4={r4_zoom_max} cycles"
    )
    report = "\n".join(lines) + "\n"
    (output_root / "validation.txt").write_text(report, encoding="utf-8")
    return report


def _validate_per_flow_fifo(case: str, outcomes: Sequence[PacketOutcome]) -> None:
    by_flow: dict[int, list[PacketOutcome]] = defaultdict(list)
    for outcome in outcomes:
        if not outcome.dropped:
            by_flow[outcome.flow_id].append(outcome)
    for flow_id, completed in by_flow.items():
        push_order = sorted(completed, key=lambda item: (item.push_cycle, item.request_id))
        pop_order = sorted(
            completed,
            key=lambda item: (item.pop_cycle if item.pop_cycle is not None else -1),
        )
        if [item.request_id for item in push_order] != [
            item.request_id for item in pop_order
        ]:
            raise ValueError(f"{case}: flow {flow_id} packets were reordered")


def _maximum_delay(
    outcomes: Sequence[PacketOutcome], event: PolicyEvent, flow_id: int
) -> int:
    delays = [
        outcome.delay
        for outcome in outcomes
        if outcome.flow_id == flow_id
        and outcome.push_cycle >= event.start_cycle
        and not outcome.dropped
        and outcome.delay is not None
    ]
    if not delays:
        raise ValueError(f"flow {flow_id} has no completed post-start packets")
    return max(delays)


def _maximum_completion_gap(outcomes: Sequence[PacketOutcome]) -> int:
    cycles = sorted(
        outcome.pop_cycle
        for outcome in outcomes
        if not outcome.dropped and outcome.pop_cycle is not None
    )
    if len(cycles) < 2:
        raise ValueError("need at least two completions to calculate an output gap")
    return max(right - left for left, right in zip(cycles, cycles[1:]))


def _read_requests(path: Path) -> dict[int, tuple[int, int, int]]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        expected = {"cycle", "request_id", "global_flow_id", "size_bytes"}
        if set(reader.fieldnames or ()) != expected:
            raise ValueError(f"{path}: unexpected request-trace header")
        return {
            int(row["request_id"], 0): (
                int(row["global_flow_id"], 0),
                int(row["size_bytes"], 0),
                int(row["cycle"], 0),
            )
            for row in reader
        }


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, required=True)
    args = parser.parse_args()
    print(validate_comparison(args.output_root.resolve()), end="")


if __name__ == "__main__":
    main()
