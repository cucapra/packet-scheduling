#!/usr/bin/env python3
"""Replay the large-tree request via the separate compiler and simulator CLIs."""
import argparse
import sys
from pathlib import Path

# Shared experiment helpers and hardware tools.
sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
import _paths

from pifo_multiedit_common import RESOURCES, RESULTS, settings, measure
from pifo_multiedit_compiler import MECHANISMS


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runs", nargs="+", choices=MECHANISMS, default=list(MECHANISMS))
    parser.add_argument("--render-only", action="store_true")
    args = parser.parse_args()
    cfg = settings()
    if not args.render_only:
        for run in args.runs:
            path = RESULTS / run
            _paths.run_script(Path(__file__).with_name("pifo_multiedit_compiler.py"), RESOURCES / "request.json", "--mechanism", run,
                "--output", path / "transactions.txt")
            unadmitted = {"control": "11,12,13,14", "control-p2": "3,4"}.get(run)
            extra = ["--unadmitted-flows", unadmitted] if unadmitted else []
            _paths.run_script(_paths.CORE_PYTHON / "pifo_simulator.py", "--transactions", path / "transactions.txt",
                "--traffic", RESOURCES / "traffic.json", "--output-dir", path,
                "--queue-depth", cfg["queue_depth"], "--link-bytes-per-cycle", cfg["link_bytes_per_cycle"],
                "--max-cycles", cfg["max_cycles"], "--evaluation-hardware", "--verilator", *extra)
    if all((RESULTS / run / "packet-outcomes.csv").exists() for run in MECHANISMS):
        measure(RESULTS)
        for script in ("pifo_multiedit_first_service.py", "pifo_multiedit_untouched_delay.py"):
            _paths.run_script(Path(__file__).with_name(script))
            _paths.run_script(Path(__file__).with_name(script), "--copy-comparison")


if __name__ == "__main__":
    main()
