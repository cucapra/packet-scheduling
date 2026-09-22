#!/usr/bin/env python3
"""Regenerate all experiment results and figures from the checked-in sources."""

from __future__ import annotations

import argparse
import shlex
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parent
SUITES = {
    "rr-to-sp": ("pifo_experiment_figures.py", "run", "--config", "experiments/rr-to-sp.json"),
    "large-tree-rr-to-sp": (
        "pifo_experiment_figures.py", "run", "--config", "experiments/large-tree-rr-to-sp.json",
    ),
    "rr-to-sp-stop-the-world-pop": (
        "pifo_experiment_figures.py", "run", "--config", "experiments/rr-to-sp-stop-the-world-pop.json",
    ),
    "motivating-example": ("pifo_motivation_all.py",),
    "designated-survivor": ("pifo_survivor_all.py",),
    "multi-edit": ("pifo_multiedit_all.py",),
    "scalability": ("pifo_scalability.py", "batch"),
}


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--experiments", nargs="+", choices=SUITES, default=list(SUITES),
                        help="Run only these suites (default: all seven, 68 RTL runs).")
    parser.add_argument("--dry-run", action="store_true", help="Print commands without running them.")
    args = parser.parse_args(argv)
    # The RTL simulators share a build workspace, so suites must run serially.
    for suite in dict.fromkeys(args.experiments):
        script, *arguments = SUITES[suite]
        command = [sys.executable, str(ROOT / "hw/python" / script), *arguments]
        print(f"[{suite}] {shlex.join(command)}", flush=True)
        if args.dry_run:
            continue
        try:
            subprocess.run(command, cwd=ROOT, check=True)
        except (OSError, subprocess.CalledProcessError) as error:
            print(f"error: {suite} failed: {error}", file=sys.stderr)
            return 1
    if not args.dry_run:
        print(f"Completed {len(set(args.experiments))} experiment suites -> {ROOT / 'experiment-results'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
