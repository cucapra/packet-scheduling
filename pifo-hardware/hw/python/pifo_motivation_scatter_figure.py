#!/usr/bin/env python3
"""Render one motivating-example generation-cycle versus packet-delay scatter."""

from __future__ import annotations

import argparse
from pathlib import Path

from pifo_figures.common import figure_paths, parse_flow_mapping, read_policy_event
from pifo_motivation_plot import read_packet_outcomes, render_delay_scatter


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--outcomes", type=Path, required=True)
    parser.add_argument("--events", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--flow-labels", required=True)
    parser.add_argument("--dpi", type=int, default=180)
    parser.add_argument("--title", default="Packet delay")
    args = parser.parse_args()
    paths = figure_paths(args.output_dir.resolve())
    render_delay_scatter(
        paths,
        read_packet_outcomes(args.outcomes),
        read_policy_event(args.events),
        parse_flow_mapping(args.flow_labels),
        args.dpi,
        args.title,
    )
    print(f"Generated {paths.svg} and {paths.png}")


if __name__ == "__main__":
    main()
