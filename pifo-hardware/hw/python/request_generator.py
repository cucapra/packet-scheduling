#!/usr/bin/env python3
"""Generate canonical request traces from patterns, formal CSV output, or PCAP."""

from __future__ import annotations

import argparse
import json
import sys
from decimal import Decimal, InvalidOperation
from pathlib import Path

from request_trace import (
    FlowResolver,
    convert_formal_csv,
    convert_pcap,
    generate_pattern,
    load_flow_mapping,
    parse_int,
    write_trace,
)


PATTERNS = ("round-robin", "uniform", "poisson", "burst")


def decimal_value(value: str) -> Decimal:
    try:
        return Decimal(value)
    except InvalidOperation as error:
        raise argparse.ArgumentTypeError(f"invalid decimal {value!r}") from error


def int_value(value: str) -> int:
    try:
        return parse_int(value)
    except ValueError as error:
        raise argparse.ArgumentTypeError(str(error)) from error


def flow_list(value: str) -> list[int]:
    try:
        result = [parse_int(part) for part in value.split(",") if part.strip()]
    except ValueError as error:
        raise argparse.ArgumentTypeError(str(error)) from error
    if not result:
        raise argparse.ArgumentTypeError("expected at least one comma-separated flow ID")
    return result


def add_output(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--output",
        "-o",
        default="-",
        help="Output trace path, or - for stdout (default -).",
    )


def add_conversion_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("input", type=Path)
    parser.add_argument(
        "--flow-map",
        type=Path,
        help=(
            "JSON name-to-ID object, or pifo_node_mapping.json containing a flows object. "
            "Unmapped names receive stable IDs."
        ),
    )
    parser.add_argument(
        "--write-flow-map",
        type=Path,
        help="Write the complete resolved name-to-ID mapping as JSON.",
    )
    parser.add_argument(
        "--cycles-per-second",
        type=decimal_value,
        default=Decimal(1000),
        help="Timestamp conversion rate (default 1000 cycles/second).",
    )
    add_output(parser)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="source", required=True)

    pattern = subparsers.add_parser("pattern", help="Generate a synthetic pattern.")
    pattern.add_argument("--pattern", choices=PATTERNS, default="round-robin")
    pattern.add_argument("--count", type=int_value, default=100)
    pattern.add_argument("--flows", type=flow_list, default=[1, 2])
    pattern.add_argument("--start-cycle", type=int_value, default=0)
    pattern.add_argument(
        "--interval-cycles",
        type=float,
        default=1.0,
        help="Fixed spacing, or mean spacing for poisson (default 1).",
    )
    pattern.add_argument("--burst-size", type=int_value, default=8)
    pattern.add_argument("--burst-gap-cycles", type=int_value, default=32)
    pattern.add_argument("--size-bytes", type=int_value, default=64)
    pattern.add_argument("--min-size-bytes", type=int_value)
    pattern.add_argument("--max-size-bytes", type=int_value)
    pattern.add_argument("--first-request-id", type=int_value, default=1)
    pattern.add_argument("--seed", type=int_value, default=1)
    add_output(pattern)

    formal = subparsers.add_parser(
        "formal-csv",
        help="Convert the OCaml formal simulator's packet result CSV.",
    )
    add_conversion_options(formal)
    formal.add_argument(
        "--time-field",
        choices=("arrived", "pushed"),
        default="arrived",
        help="Formal timestamp column to use (default arrived).",
    )

    pcap = subparsers.add_parser(
        "pcap", help="Convert a classic Ethernet PCAP used by the formal simulator."
    )
    add_conversion_options(pcap)

    return parser.parse_args()


def write_output(output: str, requests) -> None:
    if output == "-":
        write_trace(requests, sys.stdout)
        return
    path = Path(output)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as destination:
        write_trace(requests, destination)


def write_mapping(path: Path | None, resolver: FlowResolver) -> None:
    if path is None:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as destination:
        json.dump({"flows": resolver.mapping}, destination, indent=2, sort_keys=True)
        destination.write("\n")


def main() -> None:
    args = parse_args()
    try:
        resolver = None
        if args.source == "pattern":
            requests = generate_pattern(
                pattern=args.pattern,
                count=args.count,
                flows=args.flows,
                start_cycle=args.start_cycle,
                interval_cycles=args.interval_cycles,
                burst_size=args.burst_size,
                burst_gap_cycles=args.burst_gap_cycles,
                size_bytes=args.size_bytes,
                min_size_bytes=args.min_size_bytes,
                max_size_bytes=args.max_size_bytes,
                first_request_id=args.first_request_id,
                seed=args.seed,
            )
        else:
            resolver = FlowResolver(load_flow_mapping(args.flow_map))
            if args.source == "formal-csv":
                requests = convert_formal_csv(
                    args.input,
                    resolver,
                    args.cycles_per_second,
                    time_field=args.time_field,
                )
            else:
                requests = convert_pcap(
                    args.input, resolver, args.cycles_per_second
                )

        write_output(args.output, requests)
        if resolver is not None:
            write_mapping(args.write_flow_map, resolver)
        if args.output != "-":
            print(f"wrote {len(requests)} requests to {args.output}")
    except (OSError, ValueError) as error:
        raise SystemExit(f"error: {error}") from error


if __name__ == "__main__":
    main()
