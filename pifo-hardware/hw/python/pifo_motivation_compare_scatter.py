#!/usr/bin/env python3
"""Render R2/R3/R4 packet delays as three panels with shared axes."""

from __future__ import annotations

import argparse
from pathlib import Path

from pifo_figures.common import figure_paths, parse_flow_mapping, read_policy_event
from pifo_motivation_plot import read_packet_outcomes, render_delay_comparison


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    for run in ("r2", "r3", "r4"):
        parser.add_argument(f"--{run}-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--flow-labels", required=True)
    parser.add_argument("--dpi", type=int, default=180)
    args = parser.parse_args()
    runs = []
    for label, directory in (
        ("R2: stop the world", args.r2_dir),
        ("R3: whole-tree replace", args.r3_dir),
        ("R4: confined replace", args.r4_dir),
    ):
        runs.append(
            (
                label,
                read_packet_outcomes(directory / "packet-outcomes.csv"),
                read_policy_event(directory / "reconfiguration-events.csv"),
            )
        )
    paths = figure_paths(args.output_dir.resolve())
    render_delay_comparison(
        paths, runs, parse_flow_mapping(args.flow_labels), args.dpi
    )
    print(f"Generated {paths.svg} and {paths.png}")


if __name__ == "__main__":
    main()
