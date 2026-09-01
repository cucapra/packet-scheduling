#!/usr/bin/env python3
"""Compile, simulate, render, and verify a configured PIFO experiment."""

from __future__ import annotations

import argparse
import subprocess
import sys
from dataclasses import replace
from pathlib import Path
from typing import Mapping

from pifo_experiment_config import (
    ExperimentConfig,
    generate_distributed_requests,
    load_experiment_config,
    write_effective_config,
)
from pifo_experiment_verify import run_verification
from pifo_traffic_program import (
    TrafficPattern,
    TrafficProgram,
    write_traffic_program,
)
from pifo_tree_compiler import write_tree_move_program
from pifo_tree_compiler_core import build_transaction_plan


HARDWARE_ROOT = Path(__file__).resolve().parents[2]


def run_experiment(args: argparse.Namespace) -> None:
    config = _load_run_config(args)
    output_dir = config.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    effective_config = output_dir / "experiment-config.json"
    tree_move = output_dir / "tree-move.json"
    traffic = output_dir / "traffic.json"
    transactions = output_dir / "transactions.txt"
    requests = output_dir / "requests.csv"
    results = output_dir / "request-results.csv"
    events = output_dir / "reconfiguration-events.csv"
    write_effective_config(effective_config, config)
    write_tree_move_program(tree_move, config)
    write_traffic_program(
        traffic,
        TrafficProgram(
            seed=config.seed,
            patterns=(TrafficPattern(name="default", traffic=config.traffic),),
        ),
    )

    _run(
        "pifo_tree_compiler.py",
        "--input",
        tree_move,
        "--output",
        transactions,
    )
    _run(
        "pifo_simulator.py",
        "--transactions",
        transactions,
        "--traffic",
        traffic,
        "--output-dir",
        output_dir,
        "--queue-depth",
        config.simulation.queue_depth,
        "--link-bytes-per-cycle",
        config.simulation.link_bytes_per_cycle,
        "--max-cycles",
        config.simulation.max_cycles,
        "--sbt",
        args.sbt,
    )

    labels = _flow_labels(config.plot.flow_labels or {})
    figure_root = output_dir / "figures"
    bandwidth_dir = figure_root / "bandwidth"
    scatter_dir = figure_root / "packet-scatter"
    common_figure_args = (
        "--results",
        results,
        "--events",
        events,
        "--flow-labels",
        labels,
        "--dpi",
        config.plot.dpi,
    )
    _run(
        "pifo_bandwidth_figure.py",
        *common_figure_args,
        "--output-dir",
        bandwidth_dir,
        "--link-bytes-per-cycle",
        config.simulation.link_bytes_per_cycle,
        "--window-cycles",
        config.plot.bandwidth_window_cycles,
        "--sample-cycles",
        config.plot.bandwidth_sample_cycles,
    )
    _run(
        "pifo_packet_scatter_figure.py",
        *common_figure_args,
        "--output-dir",
        scatter_dir,
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
            results,
            events,
            verification_paths[0],
            verification_paths[1],
        )

    figure_paths = tuple(
        directory / filename
        for directory in (bandwidth_dir, scatter_dir)
        for filename in ("data.csv", "figure.svg", "figure.png")
    )
    print("Generated:")
    for path in (
        effective_config,
        tree_move,
        traffic,
        transactions,
        requests,
        results,
        events,
        *figure_paths,
        *verification_paths,
    ):
        print(f"  {path}")
    if verification_report is not None and not verification_report["passed"]:
        raise RuntimeError(
            f"phase verification failed; see {verification_paths[1]}"
        )
    if verification_report is not None:
        print("Phase verification: PASS")


def validate_experiment_config(args: argparse.Namespace) -> None:
    config = load_experiment_config(args.config)
    requests = generate_distributed_requests(config.traffic, config.seed)
    transaction = build_transaction_plan(
        config.initial_tree,
        config.reconfiguration,
        config.simulation.num_vpifos,
    )
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
        f"labels={transaction.before_label}->{transaction.after_label} "
        f"scheduled_cycle={transaction.cycle} "
        f"commands={len(transaction.transaction_commands)}"
    )
    print(
        f"  tree_nodes={len(config.initial_tree.nodes)} "
        f"engines={config.simulation.num_engines} "
        "path_depths="
        + ",".join(
            str(len(path)) for path in config.initial_tree.flow_paths.values()
        )
    )
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


def _load_run_config(args: argparse.Namespace) -> ExperimentConfig:
    config = load_experiment_config(args.config, args.output_dir)
    return replace(config, output_dir=config.output_dir.resolve())


def _flow_labels(labels: Mapping[int, str]) -> str:
    return ",".join(f"{flow_id}:{label}" for flow_id, label in sorted(labels.items()))


def _run(script_name: str, *arguments: object) -> None:
    script = HARDWARE_ROOT / "hw/python" / script_name
    subprocess.run(
        [sys.executable, str(script), *(str(argument) for argument in arguments)],
        cwd=HARDWARE_ROOT,
        check=True,
    )


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)

    run = commands.add_parser(
        "run", help="Compile, simulate, and invoke both per-figure scripts."
    )
    run.add_argument("--config", type=Path, required=True)
    run.add_argument("--output-dir", type=Path)
    run.add_argument("--sbt", default="sbt")
    run.set_defaults(handler=run_experiment)

    validate = commands.add_parser(
        "validate", help="Validate an experiment without running RTL."
    )
    validate.add_argument("config", type=Path)
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
