#!/usr/bin/env python3
"""Run the designated-survivor experiment using separate compiler/simulator CLIs."""
import argparse
import subprocess
import sys
from pathlib import Path

from pifo_survivor_common import ROOT, RESOURCES, RESULTS, settings, make_traffic, measure
from pifo_survivor_compiler import MECHANISMS


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pre-cycles", type=int, nargs="+", help="Override the selected sweep points.")
    parser.add_argument("--runs", choices=MECHANISMS, nargs="+", default=list(MECHANISMS))
    parser.add_argument("--render-only", action="store_true")
    parser.add_argument("--results", type=Path, default=RESULTS)
    args = parser.parse_args()
    cfg = settings()
    points = args.pre_cycles if args.pre_cycles is not None else sorted(set(cfg["sweep_pre_cycles"] + [cfg["figure_a_pre_cycles"]]))
    def cli(script, *arguments):
        subprocess.run([sys.executable, str(ROOT / "hw/python" / script), *map(str, arguments)], check=True)
    if not args.render_only:
        for pre in points:
            case = args.results / f"pre-{pre}"
            traffic, end = make_traffic(case, pre, cfg)
            for run in args.runs:
                path = case / run
                cli("pifo_survivor_compiler.py", "--input", RESOURCES / "tree-move.json", "--mechanism", run,
                    "--cycle", pre, "--output", path / "transactions.txt")
                cli("pifo_simulator.py", "--transactions", path / "transactions.txt", "--traffic", traffic,
                    "--output-dir", path, "--queue-depth", cfg["queue_depth"],
                    "--link-bytes-per-cycle", cfg["link_bytes_per_cycle"], "--max-cycles", end + 50000, "--verilator")
    measure(args.results)
    for script in ("pifo_survivor_stop_figure.py", "pifo_survivor_zoom_figure.py"):
        cli(script, "--results", args.results)


if __name__ == "__main__":
    main()
