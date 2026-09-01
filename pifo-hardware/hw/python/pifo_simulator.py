#!/usr/bin/env python3
"""Run RTL from a direct transaction timeline and a traffic-pattern file."""

from __future__ import annotations

import argparse
import math
import shutil
import subprocess
from pathlib import Path

from pifo_traffic_program import generate_traffic, load_traffic_program
from pifo_transaction_program import load_transaction_program
from request_trace import write_trace


HARDWARE_ROOT = Path(__file__).resolve().parents[2]


def run_simulator(args: argparse.Namespace) -> tuple[Path, Path, Path]:
    if args.queue_depth <= 0:
        raise ValueError("--queue-depth must be positive")
    if not math.isfinite(args.link_bytes_per_cycle) or args.link_bytes_per_cycle <= 0:
        raise ValueError("--link-bytes-per-cycle must be finite and positive")
    if args.max_cycles <= 0:
        raise ValueError("--max-cycles must be positive")
    if args.warmup_cycles < 0:
        raise ValueError("--warmup-cycles must be non-negative")
    transactions = load_transaction_program(args.transactions)
    traffic = load_traffic_program(args.traffic)
    if transactions.initial is None:
        raise ValueError("transaction timeline must contain one at=init package")
    if not transactions.transactions:
        raise ValueError("transaction timeline must contain at least one timed package")
    requests = generate_traffic(traffic)
    max_flow_id = transactions.hardware.num_vpifos - 1
    invalid_flows = sorted(
        {
            request.global_flow_id
            for request in requests
            if request.global_flow_id >= max_flow_id
        }
    )
    if invalid_flows:
        raise ValueError(
            "traffic flow IDs must be below the reserved empty-PIFO ID "
            f"{max_flow_id}: {','.join(map(str, invalid_flows))}"
        )
    late = [
        transaction.name
        for transaction in transactions.transactions
        if transaction.at_cycle is not None and transaction.at_cycle >= args.max_cycles
    ]
    if late:
        raise ValueError(
            "transactions scheduled at or after --max-cycles: " + ", ".join(late)
        )

    output_dir = args.output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    trace_path = output_dir / "requests.csv"
    results_path = output_dir / "request-results.csv"
    events_path = output_dir / "reconfiguration-events.csv"
    with trace_path.open("w", newline="", encoding="utf-8") as destination:
        write_trace(requests, destination)

    sbt_path = shutil.which(args.sbt)
    if sbt_path is None:
        raise RuntimeError(f"could not find sbt executable {args.sbt!r}")
    simulator_args = [
        "--trace",
        str(trace_path),
        "--transactions",
        str(args.transactions.resolve()),
        "--output",
        str(results_path),
        "--transaction-event-output",
        str(events_path),
        "--queue-depth",
        str(args.queue_depth),
        "--link-bytes-per-cycle",
        str(args.link_bytes_per_cycle),
        "--max-cycles",
        str(args.max_cycles),
        "--warmup-cycles",
        str(args.warmup_cycles),
        "--no-control-socket",
        "--no-flat-fifo",
    ]
    if not args.wave:
        simulator_args.append("--no-wave")
    if not args.verbose:
        simulator_args.append("--quiet")
    sbt_command = "runMain rio.sim.RequestSimulatorCli " + " ".join(
        _quote_sbt(value) for value in simulator_args
    )
    print(
        f"Simulating {len(requests)} packets with "
        f"{len(transactions.transactions)} timed transaction(s)...",
        flush=True,
    )
    subprocess.run([sbt_path, sbt_command], cwd=HARDWARE_ROOT, check=True)
    return trace_path, results_path, events_path


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--transactions",
        type=Path,
        required=True,
        help="pifo-transactions-v1 direct command timeline.",
    )
    parser.add_argument(
        "--traffic",
        type=Path,
        required=True,
        help="pifo-traffic-v1 pattern timeline.",
    )
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--queue-depth", type=int, default=32)
    parser.add_argument("--link-bytes-per-cycle", type=float, default=64.0)
    parser.add_argument("--max-cycles", type=int, default=100_000)
    parser.add_argument("--warmup-cycles", type=int, default=4)
    parser.add_argument("--sbt", default="sbt")
    parser.add_argument("--wave", action="store_true")
    parser.add_argument("--verbose", action="store_true")
    return parser


def main() -> None:
    args = build_parser().parse_args()
    try:
        generated = run_simulator(args)
    except (OSError, RuntimeError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"error: {error}") from error
    print("Generated:")
    for path in generated:
        print(f"  {path}")


def _quote_sbt(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


if __name__ == "__main__":
    main()
