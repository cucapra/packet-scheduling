from __future__ import annotations

import ast
import csv
import runpy
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_figures.bandwidth import generate as bandwidth, build_samples, render_matplotlib as bandwidth_plot, render_svg as bandwidth_svg
from pifo_figures.common import COMMIT_BACKGROUNDS, FigureInputs, PacketTiming, PolicyEvent, figure_paths, outcomes_from_timings
from pifo_figures.packet_scatter import generate as scatter, render_matplotlib as scatter_plot, render_svg as scatter_svg
from pifo_motivation_plot import render_delay_scatter, render_delay_comparison, render_throughput_comparison


EVENT = PolicyEvent(
    "RR", "SP", 10, 10, 12, 46, drain_cycle=40,
    instruction_count=7, commit_applied_cycle=14, commit_cycles=4,
    install_finish_cycle=17, bank_cleanup_cycles=3,
    cleanup_start_cycle=17, cleanup_commit_cycle=19, cleanup_applied_cycle=44,
    cleanup_finish_cycle=46, cleanup_instruction_count=6,
    cleanup_commit_cycles=27, cleanup_bank_cleanup_cycles=2,
)
EXPECTED = {
    "C1 start": 0,
    "C1 commit accepted": 2,
    "C1 ready_for_next_commit": 7,
    "C1 old-tree-drained": 30,
    "C2 start": 7,
    "C2 commit accepted": 9,
    "C2 ready_for_next_commit": 36,
    "C2 old-tree-drained": 30,
}


class CommitTimelineTest(unittest.TestCase):
    def test_saved_figure_markers_match_each_runs_recorded_commits(self):
        root = Path(__file__).resolve().parents[3] / "experiment-results"
        scripts = sorted(script for script in root.rglob("plot.py") if "PANELS =" in script.read_text())
        self.assertGreaterEqual(len(scripts), 14)
        cases = {"R2": "r2-stop-the-world", "R3": "r3-whole-tree", "R4": "r4-confined"}
        for script in scripts:
            with self.subTest(script=script):
                panels = ast.literal_eval(next(node.value for node in ast.parse(script.read_text()).body
                    if isinstance(node, ast.Assign) and isinstance(node.targets[0], ast.Name)
                    and node.targets[0].id == "PANELS"))
                for panel in panels:
                    run = script.parents[2]
                    if script.parent.parent.name == "comparisons":
                        run = root / "motivating-example" / cases[panel["title"].split(":")[0]]
                    with (run / "reconfiguration-events.csv").open() as source:
                        event = next(row for row in csv.DictReader(source) if row["event"] == "reconfiguration")
                    for name, fields in (
                        ("C1", ("start_cycle", "commit_cycle", "install_finish_cycle", "drain_cycle")),
                        ("C2", ("cleanup_start_cycle", "cleanup_commit_cycle", "cleanup_finish_cycle", "drain_cycle")),
                    ):
                        cycles = [cycle + panel["start"] for cycle, _, _, label in panel["markers"]
                                  if label.startswith(name + " ")]
                        self.assertEqual(cycles, [int(event[field]) for field in fields])

    def test_all_repo_renderers_use_both_commits_without_horizontal_delay_markers(self):
        packets = (PacketTiming(1, 1, 48, 0, 20), PacketTiming(2, 2, 48, 30, 60))
        outcomes = outcomes_from_timings(packets)
        flows, samples = build_samples(packets, EVENT, 8, 2, 16)
        runs = [("R3", outcomes, EVENT), ("R4", outcomes, EVENT)]
        labels = {1: "zoom", 2: "gmail"}
        with tempfile.TemporaryDirectory() as directory:
            paths = figure_paths(Path(directory))
            renderers = (
                ("bandwidth", lambda: bandwidth_plot(paths, flows, samples, EVENT, labels, 60)),
                ("scatter", lambda: scatter_plot(paths, packets, EVENT, labels, 60)),
                ("delay", lambda: render_delay_scatter(paths, outcomes, EVENT, labels, 60, "R3")),
                ("delay comparison", lambda: render_delay_comparison(paths, runs, labels, 60)),
                ("throughput comparison", lambda: render_throughput_comparison(paths, runs, labels, 60, 8, 2, 16)),
            )
            for kind, render in renderers:
                with self.subTest(kind=kind), patch("matplotlib.figure.Figure.savefig", autospec=True) as save:
                    render()
                    for axis in save.call_args.args[0].axes:
                        self.assertEqual({line.get_label(): line.get_xdata()[0] for line in axis.lines
                                          if line.get_label() in EXPECTED}, EXPECTED)
                        horizontal = [line for line in axis.lines if line.get_gid() == "commit-time-y"]
                        self.assertEqual(len(horizontal), 8 if kind == "scatter" else 0)
                        self.assertEqual(len(axis.patches), 4 if kind == "scatter" else 2)

    def test_svg_fallback_has_both_commits_and_background_colors(self):
        packets = (PacketTiming(1, 1, 48, 0, 20), PacketTiming(2, 2, 48, 30, 60))
        flows, samples = build_samples(packets, EVENT, 8, 2, 16)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "figure.svg"
            for render in (lambda: bandwidth_svg(path, flows, samples, EVENT, {}, 60),
                           lambda: scatter_svg(path, packets, EVENT, {}, 60)):
                render()
                svg = path.read_text()
                for label in EXPECTED:
                    self.assertIn(label, svg)
                for color in COMMIT_BACKGROUNDS:
                    self.assertIn(color, svg)

    def test_standalone_plots_show_both_commits_and_distinct_backgrounds(self):
        with tempfile.TemporaryDirectory() as directory:
            for kind, generate in (("bandwidth", lambda inputs: bandwidth(inputs, 8, 2, 16)),
                                   ("scatter", scatter)):
                with self.subTest(kind=kind):
                    output = Path(directory) / kind
                    generate(FigureInputs(
                        (PacketTiming(1, 1, 48, 0, 20), PacketTiming(2, 2, 48, 30, 60)),
                        EVENT, {1: "zoom", 2: "gmail"}, 60, output,
                    ))
                    namespace = runpy.run_path(str(output / "plot.py"))
                    panel = namespace["PANELS"][0]
                    self.assertEqual({label: cycle for cycle, _, _, label in panel["markers"]}, EXPECTED)
                    spans = panel["spans"]
                    self.assertEqual([(start, end) for start, end, _, _ in spans], [(0, 7), (7, 36)])
                    self.assertNotEqual(spans[0][2], spans[1][2])
                    axis = namespace["axis"] if kind == "scatter" else namespace["axes"][0]
                    vertical = {line.get_label(): line.get_xdata()[0] for line in axis.lines
                                if line.get_label() in EXPECTED}
                    self.assertEqual(vertical, EXPECTED)
                    if kind == "scatter":
                        self.assertEqual(axis.get_xlim(), axis.get_ylim())
                        self.assertEqual(axis.get_aspect(), 1.0)
                        horizontal = [line for line in axis.lines if line.get_gid() == "commit-time-y"]
                        self.assertEqual(sorted(line.get_ydata()[0] for line in horizontal), sorted(EXPECTED.values()))


if __name__ == "__main__":
    unittest.main()
