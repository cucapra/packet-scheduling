#!/usr/bin/env python3
"""Run all four motivating-example cases and render comparison figures."""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path

from pifo_motivation_common import (
    CASE_TITLES,
    DEFAULT_OUTPUT_ROOT,
    run_case,
    run_comparisons,
)
from pifo_motivation_verify import validate_comparison


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, default=DEFAULT_OUTPUT_ROOT)
    parser.add_argument("--sbt", default="sbt")
    parser.add_argument("--render-only", action="store_true")
    parser.add_argument("--wave", action="store_true")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    output_root = args.output_root.resolve()
    try:
        for case in CASE_TITLES:
            run_case(
                case,
                output_root / case,
                args.sbt,
                args.render_only,
                args.wave,
                args.verbose,
            )
        print(validate_comparison(output_root), end="")
        run_comparisons(output_root)
    except (OSError, RuntimeError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"error: {error}") from error
    print(f"Completed all motivating-example runs -> {output_root}")


if __name__ == "__main__":
    main()
