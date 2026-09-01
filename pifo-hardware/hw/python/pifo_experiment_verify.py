"""Verify the observable phases of a full-transitive RR-to-SP change."""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping, Sequence

from pifo_experiment_config import (
    ExperimentConfig,
    PhaseVerificationConfig,
    PolicyChangeConfig,
    load_experiment_config,
)


RESULT_FIELDS = {
    "request_id",
    "global_flow_id",
    "arrival_cycle",
    "admitted_cycle",
    "completed_cycle",
}
EVENT_FIELDS = {
    "mode",
    "start_cycle",
    "commit_cycle",
    "finish_cycle",
    "drain_cycle",
}


@dataclass(frozen=True)
class CompletedPacket:
    request_id: int
    flow_id: int
    arrival_cycle: int
    admitted_cycle: int
    completed_cycle: int


@dataclass(frozen=True)
class TransactionTiming:
    mode: str
    start_cycle: int
    commit_cycle: int
    finish_cycle: int
    drain_cycle: int
    instruction_count: int | None = None

    @property
    def staging_cycles(self) -> int:
        return self.commit_cycle - self.start_cycle

    @property
    def drain_cycles(self) -> int:
        return self.drain_cycle - self.commit_cycle

    @property
    def synchronization_cycles(self) -> int:
        return self.finish_cycle - self.commit_cycle


@dataclass(frozen=True)
class VerificationCheck:
    check_id: str
    description: str
    expected: object
    observed: object
    passed: bool

    def to_dict(self) -> dict[str, object]:
        return {
            "id": self.check_id,
            "description": self.description,
            "expected": self.expected,
            "observed": self.observed,
            "passed": self.passed,
        }


def read_completed_packets(path: Path) -> list[CompletedPacket]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        missing = RESULT_FIELDS.difference(reader.fieldnames or ())
        if missing:
            raise ValueError(
                f"{path}: missing result fields: {', '.join(sorted(missing))}"
            )
        packets: list[CompletedPacket] = []
        seen_ids: set[int] = set()
        for line_number, row in enumerate(reader, start=2):
            try:
                packet = CompletedPacket(
                    request_id=_integer(row["request_id"]),
                    flow_id=_integer(row["global_flow_id"]),
                    arrival_cycle=_integer(row["arrival_cycle"]),
                    admitted_cycle=_integer(row["admitted_cycle"]),
                    completed_cycle=_integer(row["completed_cycle"]),
                )
                if packet.request_id in seen_ids:
                    raise ValueError(f"duplicate request ID {packet.request_id}")
                if packet.flow_id < 0:
                    raise ValueError("flow ID must be non-negative")
                if not (
                    0
                    <= packet.arrival_cycle
                    <= packet.admitted_cycle
                    <= packet.completed_cycle
                ):
                    raise ValueError(
                        "expected arrival_cycle <= admitted_cycle <= completed_cycle"
                    )
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error
            packets.append(packet)
            seen_ids.add(packet.request_id)
    if not packets:
        raise ValueError(f"{path}: no completed packets")
    return sorted(packets, key=lambda packet: (packet.completed_cycle, packet.request_id))


def read_transaction_timing(path: Path) -> TransactionTiming:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        missing = EVENT_FIELDS.difference(reader.fieldnames or ())
        if missing:
            raise ValueError(
                f"{path}: missing event fields: {', '.join(sorted(missing))}"
            )
        rows = list(reader)
    if len(rows) != 1:
        raise ValueError(f"{path}: expected exactly one reconfiguration event")
    row = rows[0]
    if not row["drain_cycle"].strip():
        raise ValueError(f"{path}: full-transitive event has no drain cycle")
    timing = TransactionTiming(
        mode=row["mode"].strip(),
        start_cycle=_integer(row["start_cycle"]),
        commit_cycle=_integer(row["commit_cycle"]),
        finish_cycle=_integer(row["finish_cycle"]),
        drain_cycle=_integer(row["drain_cycle"]),
        instruction_count=(
            _integer(row["instruction_count"])
            if (row.get("instruction_count") or "").strip()
            else None
        ),
    )
    if timing.mode != "full_transitive":
        raise ValueError(f"{path}: phase verification requires full_transitive mode")
    if not (
        0 <= timing.start_cycle <= timing.commit_cycle <= timing.finish_cycle
        and timing.commit_cycle <= timing.drain_cycle
    ):
        raise ValueError(f"{path}: invalid transaction timestamp order")
    if timing.instruction_count is not None and timing.instruction_count <= 0:
        raise ValueError(f"{path}: instruction count must be positive")
    return timing


def verify_rr_to_sp_phases(
    config: ExperimentConfig,
    packets: Sequence[CompletedPacket],
    timing: TransactionTiming,
) -> dict[str, object]:
    packets = sorted(
        packets, key=lambda packet: (packet.completed_cycle, packet.request_id)
    )
    thresholds = config.verification
    if thresholds is None:
        raise ValueError("config has no verification section")
    reconfiguration = config.reconfiguration
    if not isinstance(reconfiguration, PolicyChangeConfig):
        raise ValueError("phase verification requires policy_change")

    flow_ids = tuple(config.traffic.flow_ids)
    if len(flow_ids) != 2:
        raise ValueError("RR-to-SP phase verification currently requires two flows")
    result_flows = {packet.flow_id for packet in packets}
    if result_flows != set(flow_ids):
        raise ValueError("result flows do not match traffic.flows")
    root_name = config.initial_tree.root
    root_change = reconfiguration.changes[root_name]
    priorities = dict(config.initial_tree.nodes[root_name].flow_state)
    priorities.update(root_change.flow_state)
    if set(priorities) != set(flow_ids):
        raise ValueError("SP root priorities must cover exactly the traffic flows")
    if len(set(priorities.values())) != len(priorities):
        raise ValueError("SP phase verification requires distinct priorities")

    # Mapper publication occurs on the commit edge. The mesh contract assigns a
    # request accepted on that edge to the old bank; only later admissions use
    # the newly active mappings.
    old_packets = [
        packet for packet in packets if packet.admitted_cycle <= timing.commit_cycle
    ]
    new_packets = [
        packet for packet in packets if packet.admitted_cycle > timing.commit_cycle
    ]
    commit_edge_packets = [
        packet for packet in packets if packet.admitted_cycle == timing.commit_cycle
    ]
    before_commit = [
        packet for packet in packets if packet.completed_cycle < timing.commit_cycle
    ]
    during_drain = [
        packet
        for packet in packets
        if timing.commit_cycle <= packet.completed_cycle < timing.drain_cycle
    ]
    after_drain = [
        packet for packet in packets if packet.completed_cycle >= timing.drain_cycle
    ]

    old_ids = {packet.request_id for packet in old_packets}
    new_ids = {packet.request_id for packet in new_packets}
    old_before = [packet for packet in before_commit if packet.request_id in old_ids]
    old_during = [packet for packet in during_drain if packet.request_id in old_ids]
    old_after = [packet for packet in after_drain if packet.request_id in old_ids]
    new_before = [packet for packet in before_commit if packet.request_id in new_ids]
    new_during = [packet for packet in during_drain if packet.request_id in new_ids]
    new_after = [packet for packet in after_drain if packet.request_id in new_ids]
    old_backlog = [
        packet
        for packet in old_packets
        if packet.completed_cycle >= timing.commit_cycle
    ]

    checks_by_fact: list[tuple[str, str, list[VerificationCheck]]] = [
        (
            "longer_queue_to_drain",
            "A large transaction and old-tree backlog make staging and drain visible.",
            [
                *(
                    [
                        _at_least(
                            "serialized_configuration_rate",
                            "cycle span from first through final acceptance at no more than one instruction per cycle",
                            max(0, timing.instruction_count - 1),
                            timing.staging_cycles,
                        )
                    ]
                    if timing.instruction_count is not None
                    else []
                ),
                _at_least(
                    "staging_duration",
                    "cycles from package start to CommitMapper acceptance",
                    thresholds.minimum_staging_cycles,
                    timing.staging_cycles,
                ),
                _at_least(
                    "old_backlog_at_commit",
                    "old-tree packets still pending when commit is accepted",
                    thresholds.minimum_old_backlog_packets,
                    len(old_backlog),
                ),
                _at_least(
                    "drain_duration",
                    "cycles from commit acceptance until the old root drains",
                    thresholds.minimum_drain_cycles,
                    timing.drain_cycles,
                ),
            ],
        ),
        (
            "before_commit_old_policy",
            "Before commit, completed packets come from the old RR tree.",
            [
                _at_least(
                    "precommit_packet_count",
                    "packets completed before commit",
                    thresholds.minimum_packets_per_phase,
                    len(before_commit),
                ),
                _equals(
                    "new_packets_before_commit",
                    "new-tree packets completed before commit",
                    0,
                    len(new_before),
                ),
                _equals(
                    "rr_repetitions_before_commit",
                    "adjacent same-flow completions under balanced RR traffic",
                    0,
                    _adjacent_same_flow(old_before),
                ),
            ],
        ),
        (
            "after_commit_drain_old_first",
            "After commit, the old RR tree drains before any new-tree output.",
            [
                _at_least(
                    "drain_phase_packet_count",
                    "packets completed from commit until drain",
                    thresholds.minimum_packets_per_phase,
                    len(during_drain),
                ),
                _equals(
                    "new_packets_during_drain",
                    "new-tree packets completed before the old root drained",
                    0,
                    len(new_during),
                ),
                _equals(
                    "rr_repetitions_during_drain",
                    "adjacent same-flow old completions while draining",
                    0,
                    _adjacent_same_flow(old_during),
                ),
                _equals(
                    "rr_repetitions_all_old",
                    "adjacent same-flow completions across the complete old epoch",
                    0,
                    _adjacent_same_flow(old_packets),
                ),
            ],
        ),
        (
            "after_drain_new_policy",
            "After drain, only the new SP tree runs and lower priorities finish first.",
            [
                _at_least(
                    "postdrain_packet_count",
                    "packets completed after drain",
                    thresholds.minimum_packets_per_phase,
                    len(after_drain),
                ),
                _equals(
                    "old_packets_after_drain",
                    "old-tree packets completed after drain",
                    0,
                    len(old_after),
                ),
                _equals(
                    "sp_priority_reversals",
                    "higher numeric SP priority followed by a lower one",
                    0,
                    _priority_reversals(new_after, priorities),
                ),
            ],
        ),
    ]

    facts = []
    all_checks: list[VerificationCheck] = []
    for fact_id, description, checks in checks_by_fact:
        all_checks.extend(checks)
        facts.append(
            {
                "id": fact_id,
                "description": description,
                "passed": all(check.passed for check in checks),
                "checks": [check.to_dict() for check in checks],
            }
        )

    return {
        "schema_version": 1,
        "passed": all(check.passed for check in all_checks),
        "event": {
            "start_cycle": timing.start_cycle,
            "commit_cycle": timing.commit_cycle,
            "drain_cycle": timing.drain_cycle,
            "finish_cycle": timing.finish_cycle,
            "staging_cycles": timing.staging_cycles,
            "drain_cycles": timing.drain_cycles,
            "synchronization_cycles": timing.synchronization_cycles,
            "instruction_count": timing.instruction_count,
        },
        "packet_counts": {
            "total": len(packets),
            "old_epoch": len(old_packets),
            "new_epoch": len(new_packets),
            "old_backlog_at_commit": len(old_backlog),
            "admitted_on_commit_edge_as_old": len(commit_edge_packets),
            "before_commit": len(before_commit),
            "during_drain": len(during_drain),
            "after_drain": len(after_drain),
        },
        "sp_priorities": {
            str(flow_id): priorities[flow_id] for flow_id in sorted(priorities)
        },
        "facts": facts,
    }


def write_verification_report(
    json_path: Path, markdown_path: Path, report: Mapping[str, object]
) -> None:
    json_path.parent.mkdir(parents=True, exist_ok=True)
    markdown_path.parent.mkdir(parents=True, exist_ok=True)
    json_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    markdown_path.write_text(_report_markdown(report), encoding="utf-8")


def run_verification(
    config: ExperimentConfig,
    results_path: Path,
    event_path: Path,
    json_path: Path,
    markdown_path: Path,
) -> dict[str, object]:
    report = verify_rr_to_sp_phases(
        config,
        read_completed_packets(results_path),
        read_transaction_timing(event_path),
    )
    write_verification_report(json_path, markdown_path, report)
    return report


def _at_least(
    check_id: str, description: str, expected: int, observed: int
) -> VerificationCheck:
    return VerificationCheck(
        check_id=check_id,
        description=description,
        expected=f">= {expected}",
        observed=observed,
        passed=observed >= expected,
    )


def _equals(
    check_id: str, description: str, expected: int, observed: int
) -> VerificationCheck:
    return VerificationCheck(
        check_id=check_id,
        description=description,
        expected=expected,
        observed=observed,
        passed=observed == expected,
    )


def _adjacent_same_flow(packets: Sequence[CompletedPacket]) -> int:
    return sum(
        left.flow_id == right.flow_id
        for left, right in zip(packets, packets[1:])
    )


def _priority_reversals(
    packets: Sequence[CompletedPacket], priorities: Mapping[int, int]
) -> int:
    return sum(
        priorities[left.flow_id] > priorities[right.flow_id]
        for left, right in zip(packets, packets[1:])
    )


def _integer(value: str) -> int:
    return int(value.strip(), 0)


def _report_markdown(report: Mapping[str, object]) -> str:
    status = "PASS" if report["passed"] else "FAIL"
    event = report["event"]
    counts = report["packet_counts"]
    assert isinstance(event, Mapping)
    assert isinstance(counts, Mapping)
    instruction_text = (
        f" across **{event['instruction_count']} accepted configuration instructions**"
        if event.get("instruction_count") is not None
        else ""
    )
    lines = [
        f"# Full-transitive phase verification: {status}",
        "",
        (
            f"Start **{event['start_cycle']}**, commit **{event['commit_cycle']}**, "
            f"drain **{event['drain_cycle']}**, finish **{event['finish_cycle']}**. "
            f"Staging took **{event['staging_cycles']} cycles**{instruction_text} and old-tree drain "
            f"took **{event['drain_cycles']} cycles**."
        ),
        "",
        (
            f"Packets: **{counts['before_commit']} before commit**, "
            f"**{counts['during_drain']} during drain**, and "
            f"**{counts['after_drain']} after drain**; "
            f"**{counts['old_backlog_at_commit']} old packets** were pending at commit."
        ),
        "",
        "| Fact / check | Expected | Observed | Result |",
        "| --- | ---: | ---: | :---: |",
    ]
    facts = report["facts"]
    assert isinstance(facts, Sequence)
    for fact in facts:
        assert isinstance(fact, Mapping)
        checks = fact["checks"]
        assert isinstance(checks, Sequence)
        for check in checks:
            assert isinstance(check, Mapping)
            result = "PASS" if check["passed"] else "FAIL"
            lines.append(
                f"| {fact['id']} / {check['id']} | {check['expected']} | "
                f"{check['observed']} | {result} |"
            )
    lines.append("")
    return "\n".join(lines)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--results", type=Path, required=True)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    config = load_experiment_config(args.config)
    report = run_verification(
        config,
        args.results,
        args.events,
        args.output_dir / "phase-verification.json",
        args.output_dir / "phase-verification.md",
    )
    print("PASS" if report["passed"] else "FAIL")
    if not report["passed"]:
        raise SystemExit(1)


if __name__ == "__main__":
    main()
