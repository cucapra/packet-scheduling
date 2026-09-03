#!/usr/bin/env python3
"""Compile one declarative full-tree move into direct timed transactions."""

from __future__ import annotations

import argparse
import json
from dataclasses import dataclass
from pathlib import Path

from pifo_experiment_config import (
    ExperimentConfig,
    InitialTreeConfig,
    PolicyChangeConfig,
    parse_policy_change_config,
    parse_tree_config,
    reconfiguration_to_dict,
    tree_to_dict,
    validate_tree_move,
)
from pifo_tree_compiler_core import build_transaction_plan
from pifo_transaction_program import (
    HardwareShape,
    TimedTransaction,
    TransactionProgram,
    write_transaction_program,
)


SCHEMA = "pifo-tree-move-v1"
HARDWARE_FIELDS = {
    "num_engines",
    "num_vpifos",
    "max_packet_priority",
    "fifo_depth",
    "prefetch_buffer_depth",
}


@dataclass(frozen=True)
class TreeMoveProgram:
    hardware: HardwareShape
    old_tree: InitialTreeConfig
    move: PolicyChangeConfig

    def __post_init__(self) -> None:
        validate_tree_move(
            self.old_tree,
            self.move,
            self.hardware.num_engines,
            self.hardware.num_vpifos,
            self.hardware.max_packet_priority,
        )


def write_tree_move_program(path: Path, config: ExperimentConfig) -> None:
    simulation = config.simulation
    payload = {
        "schema": SCHEMA,
        "hardware": {
            "num_engines": simulation.num_engines,
            "num_vpifos": simulation.num_vpifos,
            "max_packet_priority": simulation.max_packet_priority,
            "fifo_depth": simulation.fifo_depth,
            "prefetch_buffer_depth": simulation.prefetch_buffer_depth,
        },
        "old_tree": tree_to_dict(config.initial_tree),
        "move": reconfiguration_to_dict(config.reconfiguration),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def load_tree_move_program(path: Path) -> TreeMoveProgram:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        raise ValueError(f"{path}:{error.lineno}:{error.colno}: {error.msg}") from error
    root = _object(raw, "tree move")
    _exact_fields(root, {"schema", "hardware", "old_tree", "move"}, "tree move")
    if root["schema"] != SCHEMA:
        raise ValueError(f"tree move.schema must be {SCHEMA!r}")
    hardware_raw = _object(root["hardware"], "tree move.hardware")
    _exact_fields(hardware_raw, HARDWARE_FIELDS, "tree move.hardware")
    hardware = HardwareShape(
        num_engines=_integer(
            hardware_raw["num_engines"], "tree move.hardware.num_engines"
        ),
        num_vpifos=_integer(
            hardware_raw["num_vpifos"], "tree move.hardware.num_vpifos"
        ),
        max_packet_priority=_integer(
            hardware_raw["max_packet_priority"],
            "tree move.hardware.max_packet_priority",
        ),
        fifo_depth=_integer(
            hardware_raw["fifo_depth"], "tree move.hardware.fifo_depth"
        ),
        prefetch_buffer_depth=_integer(
            hardware_raw["prefetch_buffer_depth"],
            "tree move.hardware.prefetch_buffer_depth",
        ),
    )
    old_tree = parse_tree_config(root["old_tree"], "tree move.old_tree")
    change = parse_policy_change_config(
        root["move"],
        old_tree,
        hardware.max_packet_priority,
        "tree move.move",
    )
    return TreeMoveProgram(hardware=hardware, old_tree=old_tree, move=change)


def compile_tree_move(program: TreeMoveProgram) -> TransactionProgram:
    plan = build_transaction_plan(
        program.old_tree, program.move, program.hardware.num_vpifos
    )
    drain_root = (
        (plan.drain_engine_id, plan.drain_vpifo_id)
        if plan.drain_engine_id is not None and plan.drain_vpifo_id is not None
        else None
    )
    return TransactionProgram(
        hardware=program.hardware,
        root_engine_id=plan.root_engine_id,
        root_vpifo_id=plan.root_vpifo_id,
        initial=TimedTransaction(
            at_cycle=None,
            name="initial-tree",
            mode="direct",
            commands=plan.initial_commands,
        ),
        transactions=(
            TimedTransaction(
                at_cycle=plan.cycle,
                name=plan.name,
                mode=plan.mode,
                before_label=plan.before_label,
                after_label=plan.after_label,
                drain_root=drain_root,
                gated_flow_ids=plan.gated_flow_ids,
                minimum_stop_cycles=plan.minimum_stop_cycles,
                commands=plan.transaction_commands,
            ),
        ),
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True, help="Tree-move JSON.")
    parser.add_argument(
        "--output", type=Path, required=True, help="Direct transaction timeline."
    )
    return parser


def main() -> None:
    args = build_parser().parse_args()
    try:
        compiled = compile_tree_move(load_tree_move_program(args.input))
        write_transaction_program(args.output, compiled)
    except (OSError, ValueError) as error:
        raise SystemExit(f"error: {error}") from error
    transaction = compiled.transactions[0]
    initial_count = len(compiled.initial.commands) if compiled.initial else 0
    print(
        f"Compiled {transaction.name}: initial={initial_count} "
        f"timed={len(transaction.commands)} at={transaction.at_cycle} -> {args.output}"
    )


def _object(value: object, location: str) -> dict[str, object]:
    if not isinstance(value, dict) or not all(
        isinstance(key, str) for key in value
    ):
        raise ValueError(f"{location} must be an object")
    return value


def _exact_fields(
    value: dict[str, object], expected: set[str], location: str
) -> None:
    missing = expected.difference(value)
    unknown = set(value).difference(expected)
    if missing:
        raise ValueError(f"{location} missing fields: {', '.join(sorted(missing))}")
    if unknown:
        raise ValueError(f"{location} unknown fields: {', '.join(sorted(unknown))}")


def _integer(value: object, location: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError(f"{location} must be an integer")
    return value


if __name__ == "__main__":
    main()
