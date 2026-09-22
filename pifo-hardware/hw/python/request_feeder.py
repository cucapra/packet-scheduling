#!/usr/bin/env python3
"""Feed a canonical request trace to a running Scala request simulator."""

from __future__ import annotations

import argparse
import socket
import sys
import time
from pathlib import Path

from request_trace import Request, read_trace


DEFAULT_SOCKET = Path("/tmp/rio-request.sock")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Send cycle, request ID, global flow ID, and size from a canonical "
            "CSV trace to rio.sim.RequestSimulatorCli."
        )
    )
    parser.add_argument("trace", type=Path, help="Canonical request trace CSV.")
    parser.add_argument(
        "--socket",
        type=Path,
        default=DEFAULT_SOCKET,
        help=f"Unix request socket (default {DEFAULT_SOCKET}).",
    )
    parser.add_argument(
        "--connect-timeout",
        type=float,
        default=30.0,
        help="Seconds to wait for the simulation socket (default 30).",
    )
    parser.add_argument(
        "--absolute-cycles",
        action="store_true",
        help=(
            "Treat trace cycles as absolute simulator cycles. By default cycle 0 is "
            "anchored when this feeder connects."
        ),
    )
    parser.add_argument(
        "--no-end",
        action="store_true",
        help="Leave live input open instead of sending command=end.",
    )
    parser.add_argument(
        "--try-run",
        "--dry-run",
        action="store_true",
        dest="dry_run",
        help="Validate and print socket messages without connecting.",
    )
    return parser.parse_args()


def request_line(request: Request) -> str:
    return (
        "command=request "
        f"cycle={request.cycle} "
        f"requestId={request.request_id} "
        f"globalFlowId={request.global_flow_id} "
        f"sizeBytes={request.size_bytes}"
    )


def wire_lines(
    requests: list[Request], *, relative_cycles: bool, close_input: bool
) -> list[str]:
    lines: list[str] = []
    if relative_cycles:
        lines.append("command=begin")
    lines.extend(request_line(request) for request in requests)
    if close_input:
        lines.append("command=end")
    return lines


def connect_with_retry(socket_path: Path, timeout: float) -> socket.socket:
    if timeout < 0:
        raise ValueError("--connect-timeout must be non-negative")
    deadline = time.monotonic() + timeout
    last_error: OSError | None = None
    while True:
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        try:
            client.connect(str(socket_path))
            return client
        except OSError as error:
            client.close()
            last_error = error
            if time.monotonic() >= deadline:
                raise TimeoutError(
                    f"could not connect to {socket_path} within {timeout:g}s: {last_error}"
                ) from error
            time.sleep(0.05)


def main() -> None:
    args = parse_args()
    try:
        requests = read_trace(args.trace)
        lines = wire_lines(
            requests,
            relative_cycles=not args.absolute_cycles,
            close_input=not args.no_end,
        )
        payload = ("\n".join(lines) + "\n").encode("utf-8")
        if args.dry_run:
            sys.stdout.buffer.write(payload)
            return

        with connect_with_retry(args.socket, args.connect_timeout) as client:
            client.sendall(payload)
        print(f"sent {len(requests)} requests to {args.socket}")
    except (OSError, TimeoutError, ValueError) as error:
        raise SystemExit(f"error: {error}") from error


if __name__ == "__main__":
    main()
