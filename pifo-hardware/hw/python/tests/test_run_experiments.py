from __future__ import annotations

import contextlib
import io
import json
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT))
sys.path.insert(0, str(ROOT / "hw/python"))

import run_experiments
import pifo_scalability
from pifo_transaction_program import load_transaction_program


class ExperimentRunnerTest(unittest.TestCase):
    def test_default_covers_every_source_suite_from_the_repository_directory(self):
        expected = {path.stem for path in (ROOT / "experiments").glob("*.json")}
        expected.update(path.name for path in (ROOT / "experiments").iterdir() if path.is_dir())
        with patch("run_experiments.subprocess.run") as run, contextlib.redirect_stdout(io.StringIO()):
            self.assertEqual(run_experiments.main([]), 0)
        self.assertEqual(set(run_experiments.SUITES), expected)
        self.assertEqual(run.call_count, len(expected))
        for call in run.call_args_list:
            self.assertEqual(call.kwargs, {"cwd": ROOT, "check": True})
            self.assertEqual(call.args[0][0], sys.executable)
            self.assertTrue(Path(call.args[0][1]).is_file())

    def test_dry_run_selects_suites_without_creating_outputs(self):
        output = io.StringIO()
        with patch("run_experiments.subprocess.run") as run, contextlib.redirect_stdout(output):
            self.assertEqual(run_experiments.main([
                "--dry-run", "--experiments", "scalability", "rr-to-sp", "scalability",
            ]), 0)
        run.assert_not_called()
        self.assertEqual(len(output.getvalue().splitlines()), 2)
        self.assertIn("pifo_scalability.py batch", output.getvalue())
        self.assertIn("experiments/rr-to-sp.json", output.getvalue())

    def test_failure_stops_before_the_next_suite(self):
        with patch("run_experiments.subprocess.run", side_effect=subprocess.CalledProcessError(2, "sbt")) as run, \
                contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()) as error:
            self.assertEqual(run_experiments.main([]), 1)
        self.assertEqual(run.call_count, 1)
        self.assertIn("rr-to-sp failed", error.getvalue())

    def test_scalability_compiles_checked_in_json_without_writing_sources(self):
        with tempfile.TemporaryDirectory() as directory:
            resources, results = Path(directory) / "sources", Path(directory) / "results"
            shutil.copytree(ROOT / "experiments/scalability", resources)
            cfg = json.loads((resources / "settings.json").read_text())
            inputs = {p.relative_to(resources): p.read_bytes() for p in resources.rglob("*.json")}
            with patch.object(pifo_scalability, "RESOURCES", resources):
                for kind in cfg["requests"]:
                    for tenants in cfg["tenants"]:
                        pifo_scalability.generate_inputs(cfg, kind, tenants)
            self.assertEqual(inputs, {p.relative_to(resources): p.read_bytes() for p in resources.rglob("*.json")})
            self.assertFalse(list(resources.rglob("*.csv")))
            # A local experiment edit must be consumed, not silently regenerated.
            request = resources / "add/m-2/request.json"
            spec = json.loads(request.read_text())
            spec["cycle"] = 2345
            request.write_text(json.dumps(spec))
            before = {p.relative_to(resources): p.read_bytes() for p in resources.rglob("*") if p.is_file()}
            with patch.object(pifo_scalability, "RESOURCES", resources), \
                    patch.object(pifo_scalability, "RESULTS", results):
                for kind in cfg["requests"]:
                    for tenants in cfg["tenants"]:
                        pifo_scalability.prepare(cfg, kind, tenants)
            after = {p.relative_to(resources): p.read_bytes() for p in resources.rglob("*") if p.is_file()}
            self.assertEqual(before, after)
            self.assertEqual(len(list(results.rglob("flows.csv"))), 40)
            program = load_transaction_program(results / "add/m-2/rio/transactions.txt")
            self.assertEqual(program.transactions[0].at_cycle, 2345)


if __name__ == "__main__":
    unittest.main()
