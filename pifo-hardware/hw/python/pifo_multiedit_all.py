#!/usr/bin/env python3
"""Replay the large-tree request via the separate compiler and simulator CLIs."""
import argparse
import subprocess
import sys

from pifo_multiedit_common import ROOT, RESOURCES, RESULTS, settings, measure
from pifo_multiedit_compiler import MECHANISMS


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", nargs="+", choices=MECHANISMS, default=list(MECHANISMS))
    parser.add_argument("--render-only", action="store_true")
    args = parser.parse_args()
    cfg = settings()
    def cli(name, *arguments):
        subprocess.run([sys.executable, str(ROOT / "hw" / "python" / name), *map(str, arguments)], check=True)
    if not args.render_only:
        for run in args.runs:
            path = RESULTS / run
            cli("pifo_multiedit_compiler.py", RESOURCES / "request.json", "--mechanism", run,
                "--output", path / "transactions.txt")
            extra = ["--unadmitted-flows", "11,12,13,14"] if run == "control" else []
            cli("pifo_simulator.py", "--transactions", path / "transactions.txt",
                "--traffic", RESOURCES / "traffic.json", "--output-dir", path,
                "--queue-depth", cfg["queue_depth"], "--link-bytes-per-cycle", cfg["link_bytes_per_cycle"],
                "--max-cycles", cfg["max_cycles"], "--evaluation-hardware", "--verilator", *extra)
    if all((RESULTS / run / "packet-outcomes.csv").exists() for run in MECHANISMS):
        measure(RESULTS)
        for script in ("pifo_multiedit_first_service.py", "pifo_multiedit_untouched_delay.py"):
            cli(script)
            cli(script, "--copy-comparison")


if __name__ == "__main__":
    main()
