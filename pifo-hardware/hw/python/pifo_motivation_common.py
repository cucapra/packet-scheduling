"""Shared runner for the four motivating-example experiment cases."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path
from typing import Mapping

from pifo_motivation_verify import validate_run


HARDWARE_ROOT = Path(__file__).resolve().parents[2]
RESOURCE_ROOT = HARDWARE_ROOT / "experiments" / "motivating-example"
DEFAULT_OUTPUT_ROOT = HARDWARE_ROOT / "experiment-results" / "motivating-example"
CASE_TITLES = {
    "r1-add": "R1: add Spotify arm",
    "r2-stop-the-world": "R2: stop the world",
    "r3-whole-tree": "R3: whole-tree replace",
    "r4-confined": "R4: confined replace",
}


def run_case(
    case: str,
    output_dir: Path,
    sbt: str,
    render_only: bool,
    wave: bool,
    verbose: bool,
) -> Path:
    if case not in CASE_TITLES:
        raise ValueError(f"unknown motivating-example case {case!r}")
    settings = load_settings()
    output_dir = output_dir.resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    tree_move = RESOURCE_ROOT / case / "tree-move.json"
    transactions = output_dir / "transactions.txt"
    outcomes = output_dir / "packet-outcomes.csv"
    events = output_dir / "reconfiguration-events.csv"
    if not render_only:
        _run_script(
            "pifo_tree_compiler.py",
            "--input",
            tree_move,
            "--output",
            transactions,
        )
        simulator_args: list[object] = [
            "--transactions",
            transactions,
            "--traffic",
            RESOURCE_ROOT / "traffic.json",
            "--output-dir",
            output_dir,
            "--queue-depth",
            settings["queue_depth"],
            "--link-bytes-per-cycle",
            settings["link_bytes_per_cycle"],
            "--max-cycles",
            settings["max_cycles"],
            "--warmup-cycles",
            settings["warmup_cycles"],
            "--sbt",
            sbt,
        ]
        if wave:
            simulator_args.append("--wave")
        if verbose:
            simulator_args.append("--verbose")
        _run_script("pifo_simulator.py", *simulator_args)

    # Refuse to render a plausible-looking figure from incomplete, dropped, or
    # reordered packet data. All four runs are lossless, including R2's stop.
    validate_run(case, output_dir)

    labels = ",".join(
        f"{flow_id}:{name}"
        for flow_id, name in settings["flow_labels"].items()
    )
    common_figure_args: tuple[object, ...] = (
        "--outcomes",
        outcomes,
        "--events",
        events,
        "--flow-labels",
        labels,
        "--dpi",
        settings["dpi"],
        "--title",
        CASE_TITLES[case],
    )
    _run_script(
        "pifo_motivation_throughput_figure.py",
        *common_figure_args,
        "--output-dir",
        output_dir / "figures" / "throughput",
        "--link-bytes-per-cycle",
        settings["link_bytes_per_cycle"],
        "--window-cycles",
        settings["bandwidth_window_cycles"],
        "--sample-cycles",
        settings["bandwidth_sample_cycles"],
    )
    _run_script(
        "pifo_motivation_scatter_figure.py",
        *common_figure_args,
        "--output-dir",
        output_dir / "figures" / "delay-scatter",
    )
    return output_dir


def run_comparisons(output_root: Path) -> None:
    settings = load_settings()
    labels = ",".join(
        f"{flow_id}:{name}"
        for flow_id, name in settings["flow_labels"].items()
    )
    comparison_root = output_root / "comparisons"
    _run_script(
        "pifo_motivation_compare_scatter.py",
        "--r2-dir",
        output_root / "r2-stop-the-world",
        "--r3-dir",
        output_root / "r3-whole-tree",
        "--r4-dir",
        output_root / "r4-confined",
        "--output-dir",
        comparison_root / "r2-r4-delay-scatter",
        "--flow-labels",
        labels,
        "--dpi",
        settings["dpi"],
    )
    _run_script(
        "pifo_motivation_compare_throughput.py",
        "--r3-dir",
        output_root / "r3-whole-tree",
        "--r4-dir",
        output_root / "r4-confined",
        "--output-dir",
        comparison_root / "r3-r4-throughput",
        "--flow-labels",
        labels,
        "--link-bytes-per-cycle",
        settings["link_bytes_per_cycle"],
        "--window-cycles",
        settings["bandwidth_window_cycles"],
        "--sample-cycles",
        settings["bandwidth_sample_cycles"],
        "--dpi",
        settings["dpi"],
    )


def load_settings() -> Mapping[str, object]:
    raw = json.loads((RESOURCE_ROOT / "settings.json").read_text(encoding="utf-8"))
    required = {
        "link_bytes_per_cycle",
        "queue_depth",
        "max_cycles",
        "warmup_cycles",
        "bandwidth_window_cycles",
        "bandwidth_sample_cycles",
        "dpi",
        "flow_labels",
    }
    if set(raw) != required:
        raise ValueError("motivating-example settings fields do not match the schema")
    raw["flow_labels"] = {
        int(flow_id): str(label) for flow_id, label in raw["flow_labels"].items()
    }
    return raw


def case_main(case: str) -> None:
    parser = argparse.ArgumentParser(
        description=f"Run and plot {CASE_TITLES[case]}."
    )
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--sbt", default="sbt")
    parser.add_argument("--render-only", action="store_true")
    parser.add_argument("--wave", action="store_true")
    parser.add_argument("--verbose", action="store_true")
    args = parser.parse_args()
    output = args.output_dir or DEFAULT_OUTPUT_ROOT / case
    try:
        run_case(
            case,
            output,
            args.sbt,
            args.render_only,
            args.wave,
            args.verbose,
        )
    except (OSError, RuntimeError, ValueError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"error: {error}") from error
    print(f"Completed {CASE_TITLES[case]} -> {output.resolve()}")


def _run_script(name: str, *arguments: object) -> None:
    subprocess.run(
        [
            sys.executable,
            str(HARDWARE_ROOT / "hw" / "python" / name),
            *(str(argument) for argument in arguments),
        ],
        cwd=HARDWARE_ROOT,
        check=True,
    )
