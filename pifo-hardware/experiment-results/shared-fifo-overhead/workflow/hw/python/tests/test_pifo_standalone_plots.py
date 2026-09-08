from __future__ import annotations

import ast
import csv
import runpy
import shutil
import subprocess
import sys
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_export_plot_scripts import export_saved_figures
from pifo_figures.bandwidth import generate as bandwidth
from pifo_figures.common import (
    PACKET_TRACE_FIELDS, FigureInputs, PacketTiming, PolicyEvent, figure_paths, read_packet_outcomes,
)
from pifo_figures.packet_scatter import generate as scatter
from pifo_motivation_plot import (
    PacketOutcome, render_delay_comparison, render_delay_scatter,
    render_throughput, render_throughput_comparison,
)

ROOT = Path(__file__).resolve().parents[3]


class StandalonePlotTest(unittest.TestCase):
    def test_saved_result_scripts_run_in_isolated_folders(self):
        scripts = sorted((ROOT / "experiment-results").rglob("*plot.py"))
        self.assertGreaterEqual(len(scripts), 16)
        with tempfile.TemporaryDirectory() as directory:
            for index, script in enumerate(scripts):
                with self.subTest(script=script.relative_to(ROOT)):
                    target = Path(directory) / str(index)
                    target.mkdir()
                    shutil.copy2(script, target / script.name)
                    for csv_path in script.parent.glob("*.csv"):
                        shutil.copy2(csv_path, target / csv_path.name)
                    subprocess.run([sys.executable, "-I", str(target / script.name)], cwd=directory,
                                   check=True, capture_output=True, text=True, timeout=30)
                    self.assertEqual(len(list(target.glob("*.png"))), 1)
                    self.assertEqual(len(list(target.glob("*.svg"))), 1)

    def test_every_figure_exports_a_relocatable_csv_plotter(self):
        event = PolicyEvent("RR", "SP", 10, 10, 20, 140, drain_cycle=55,
                            instruction_count=7, commit_applied_cycle=22, commit_cycles=12,
                            bank_cleanup_cycles=33, install_finish_cycle=55,
                            cleanup_start_cycle=55, cleanup_commit_cycle=62, cleanup_applied_cycle=107,
                            cleanup_finish_cycle=140, cleanup_instruction_count=6,
                            cleanup_commit_cycles=52, cleanup_bank_cleanup_cycles=33)
        packets = (PacketTiming(1, 1, 48, 0, 50), PacketTiming(2, 2, 48, 20, 65))
        outcomes = [PacketOutcome(p.request_id, p.flow_id, p.size_bytes, p.input_cycle, p.output_cycle, False)
                    for p in packets]
        outcomes.append(PacketOutcome(3, 2, 96, 25, None, True))
        # Exercise per-panel relative time offsets as well as local packet data.
        runs = [(f"Run {index}", outcomes, replace(event, start_cycle=10 * index)) for index in (1, 2, 3)]
        labels = {1: "zoom", 2: "gmail"}

        with tempfile.TemporaryDirectory() as original, tempfile.TemporaryDirectory() as moved:
            directories = [Path(original) / name for name in (
                "bandwidth", "scatter", "throughput", "delay", "delay-comparison", "throughput-comparison",
            )]
            bandwidth(FigureInputs(packets, event, labels, 60, directories[0], tuple(outcomes)), 8, 2, 16)
            scatter(FigureInputs(packets, event, labels, 60, directories[1], tuple(outcomes)))
            render_throughput(figure_paths(directories[2]), outcomes, event, labels, 60, 8, 2, 16, "Throughput")
            render_delay_scatter(figure_paths(directories[3]), outcomes, event, labels, 60, "Delay")
            render_delay_comparison(figure_paths(directories[4]), runs, labels, 60)
            render_throughput_comparison(figure_paths(directories[5]), runs[:2], labels, 60, 8, 2, 16)
            for source in directories:
                with (source / "packets.csv").open() as stream:
                    reader = csv.DictReader(stream)
                    self.assertTrue(set(PACKET_TRACE_FIELDS).issubset(reader.fieldnames))
                    trace = list(reader)
                groups = {row.get("run", "") for row in trace}
                expected_groups = 3 if source.name == "delay-comparison" else 2 if source.name == "throughput-comparison" else 1
                self.assertEqual(len(groups), expected_groups)
                self.assertEqual(len(trace), 3 * expected_groups)
                for row in trace:
                    self.assertIn(row["flow_name"], {"zoom", "gmail"})
                    if row["dropped"] == "true":
                        self.assertEqual((row["request_id"], row["push_cycle"], row["pop_cycle"], row["delay_cycles"]),
                                         ("3", "25", "", ""))
                    else:
                        self.assertEqual(int(row["delay_cycles"]), int(row["pop_cycle"]) - int(row["push_cycle"]))
                target = Path(moved) / source.name
                target.mkdir()
                # Deliberately copy neither repo helpers nor images/config/style files.
                for path in [source / "plot.py", *source.glob("*.csv")]:
                    shutil.copy2(path, target / path.name)
                script = target / "plot.py"
                tree = ast.parse(script.read_text())
                imports = set()
                for node in ast.walk(tree):
                    if isinstance(node, ast.Import):
                        imports.update(alias.name.split(".")[0] for alias in node.names)
                    elif isinstance(node, ast.ImportFrom):
                        imports.add(node.module.split(".")[0])
                self.assertEqual(imports, {"csv", "pathlib", "matplotlib"})
                self.assertNotIn(str(ROOT), script.read_text())
            shutil.rmtree(original)  # Only the temporary test input; prove it is no longer needed.
            for directory in directories:
                target = Path(moved) / directory.name
                with self.subTest(kind=directory.name):
                    subprocess.run([sys.executable, "-I", str(target / "plot.py")],
                                   cwd=moved, check=True, capture_output=True, text=True, timeout=30)
                    svg = (target / "figure.svg").read_text()
                    self.assertIn("zoom", svg)
                    self.assertIn("finish: double-buffer cleanup done", svg)
                    self.assertIn("cleanup=6 inst / 52 cycles", svg)
                    self.assertGreater((target / "figure.png").stat().st_size, 0)

            namespace = runpy.run_path(str(Path(moved) / "bandwidth/plot.py"))
            with (Path(moved) / "bandwidth/data.csv").open() as stream:
                rows = list(csv.DictReader(stream))
            self.assertEqual(list(namespace["axes"][0].lines[0].get_ydata()),
                             [float(row["total_link_fraction"]) for row in rows])
            namespace = runpy.run_path(str(Path(moved) / "scatter/plot.py"))
            self.assertEqual(namespace["axis"].get_xlim(), namespace["axis"].get_ylim())
            self.assertEqual(namespace["axis"].get_aspect(), 1.0)
            namespace = runpy.run_path(str(Path(moved) / "delay-comparison/plot.py"))
            axes = namespace["axes"][0]
            self.assertTrue(axes[0].get_shared_y_axes().joined(axes[0], axes[2]))
            self.assertTrue(axes[0].get_shared_x_axes().joined(axes[0], axes[2]))
            self.assertEqual(list(axes[2].collections[0].get_offsets()[0]), [-30, 50])

    def test_svg_fallback_also_emits_the_standalone_script(self):
        with tempfile.TemporaryDirectory() as directory:
            inputs = FigureInputs((PacketTiming(1, 1, 48, 0, 4),),
                                  PolicyEvent("RR", "SP", 1, 1, 2, 8), {}, 60, Path(directory))
            with patch("pifo_figures.bandwidth.select_renderer", return_value="svg"), \
                 patch("pifo_figures.bandwidth.rasterize_svg"):
                bandwidth(inputs, 4, 1, 16)
            self.assertTrue((Path(directory) / "plot.py").is_file())
            self.assertTrue((Path(directory) / "data.csv").is_file())
            self.assertTrue((Path(directory) / "packets.csv").is_file())

    def test_backfill_preserves_saved_artifacts_and_their_original_timestamps(self):
        archive = ROOT / "experiment-results/rr-to-sp"
        files = ("experiment-config.json", "reconfiguration-events.csv", "bandwidth.csv", "packet-times.csv",
                 "rr-to-sp-bandwidth.svg", "rr-to-sp-packet-scatter.svg")
        with tempfile.TemporaryDirectory() as directory:
            target = Path(directory) / "rr-to-sp"
            target.mkdir()
            for name in files:
                shutil.copy2(archive / name, target / name)
            before = {name: (target / name).read_bytes() for name in files}
            scripts = export_saved_figures(Path(directory))
            self.assertEqual(len(scripts), 2)
            self.assertEqual(before, {name: (target / name).read_bytes() for name in files})
            for script in scripts:
                self.assertIn("'start': 320", script.read_text())
                self.assertIn("commit accepted=330", script.read_text())
            with (target / "rr-to-sp-packets.csv").open() as stream:
                rows = list(csv.DictReader(stream))
            self.assertEqual(len(rows), 160)
            self.assertTrue(all(row["size_bytes"] == "" for row in rows))

    def test_saved_packet_traces_match_their_own_run_not_neighboring_archives(self):
        root = ROOT / "experiment-results"
        local_traces = sorted(root.glob("**/figures/*/packets.csv"))
        self.assertEqual(len(local_traces), 10)
        for trace in local_traces:
            with self.subTest(trace=trace):
                self.assertEqual(read_packet_outcomes(trace),
                                 read_packet_outcomes(trace.parents[2] / "packet-outcomes.csv"))
        for comparison in sorted(root.glob("**/comparisons/*/packets.csv")):
            with comparison.open() as stream:
                rows = list(csv.DictReader(stream))
            for label, case in (("R2: stop the world", "r2-stop-the-world"),
                                ("R3: whole-tree replace", "r3-whole-tree"),
                                ("R4: confined replace", "r4-confined")):
                selected = [row for row in rows if row["run"] == label]
                if not selected:
                    self.assertEqual((comparison.parent.name, label), ("r3-r4-throughput", "R2: stop the world"))
                    continue
                expected = read_packet_outcomes(root / "motivating-example" / case / "packet-outcomes.csv")
                self.assertEqual(len(selected), len(expected))
                for row, packet in zip(selected, expected):
                    self.assertEqual((int(row["request_id"]), int(row["flow"]), int(row["push_cycle"]), int(row["pop_cycle"])),
                                     (packet.request_id, packet.flow_id, packet.push_cycle, packet.pop_cycle))
        with (root / "rr-to-sp/rr-to-sp-packets.csv").open() as stream:
            archive = list(csv.DictReader(stream))
        self.assertEqual(len(archive), 160)
        self.assertEqual(archive[1]["pop_cycle"], "21")
        self.assertTrue(all(row["size_bytes"] == "" for row in archive))
        current = read_packet_outcomes(root / "rr-to-sp/packet-outcomes.csv")
        self.assertEqual(len(current), 480)
        self.assertEqual(current[1].pop_cycle, 19)
        large = read_packet_outcomes(root / "large-tree-rr-to-sp/rr-to-sp-packets.csv")
        self.assertEqual(len(large), 120)
        self.assertEqual({p.size_bytes for p in large}, {512})


if __name__ == "__main__":
    unittest.main()
