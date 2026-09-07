import ast
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from pifo_figures.common import commit_windows, read_policy_event
from pifo_experiment_verify import read_transaction_timing
from pifo_figures.evaluation import commit_rows, export, read_csv
from pifo_multiedit_compiler import MECHANISMS, compile_request

ROOT = Path(__file__).resolve().parents[3]


class EvaluationTest(unittest.TestCase):
    def test_all_multi_edit_epochs_fit_default_replay_fifo(self):
        spec = json.loads((ROOT / "experiments/multi-edit/request.json").read_text())
        for run in MECHANISMS:
            program, _ = compile_request(spec, run)
            for transaction in (program.initial, *program.transactions):
                first = next((i for i, c in enumerate(transaction.commands)
                              if c.command in {"UpdateMapperPre", "UpdateMapperPost"}), len(transaction.commands) - 1)
                self.assertLessEqual(len(transaction.commands) - first, 256, (run, transaction.name))

    def test_guarded_retirement_includes_leaf_fifos(self):
        spec = json.loads((ROOT / "experiments/multi-edit/request.json").read_text())
        program, _ = compile_request(spec, "rio")
        guard_nodes = {(c.engine_id, c.vpifo_id) for c in program.transactions[1].commands if c.command == "GuardDrain"}
        self.assertEqual(guard_nodes, {(2, 4), (3, 3), (3, 4)})

    def test_all_current_survivor_commits_keep_their_own_ready_time(self):
        run = ROOT / "experiment-results/designated-survivor/pre-2000/reserved"
        rows = commit_rows({"reserved": run})
        event = read_policy_event(run / "reconfiguration-events.csv")
        windows = commit_windows(event)
        self.assertEqual(len(windows), 3)
        source = read_csv(run / "reconfiguration-events.csv")
        self.assertEqual([int(r["ready_for_next_commit"]) for r in rows],
                         [int(r["install_finish_cycle"]) for r in source])
        self.assertEqual([w[-1] for w in windows], [int(r["ready_for_next_commit"]) for r in rows])
        self.assertEqual(len({r["old_tree_drained_cycle"] for r in rows}), 1)
        timing = read_transaction_timing(run / "reconfiguration-events.csv")
        self.assertEqual(timing.finish_cycle, windows[-1][-1])
        self.assertEqual(len(timing.commits), 3)

    def test_exported_templates_have_no_repo_or_shared_style_imports(self):
        with tempfile.TemporaryDirectory() as directory, patch("subprocess.run"):
            for kind in ("zoom", "stop", "first", "untouched"):
                path = Path(directory) / kind
                export(path, kind, [{"value": 1}], [{"flow": 1}], [], {})
                parsed = ast.parse((path / "plot.py").read_text())
                imports = {name.name.split(".")[0] for node in ast.walk(parsed)
                           if isinstance(node, ast.Import) for name in node.names}
                imports |= {node.module.split(".")[0] for node in ast.walk(parsed) if isinstance(node, ast.ImportFrom)}
                self.assertEqual(imports, {"csv", "pathlib", "matplotlib"})

    def test_every_evaluation_figure_bundles_full_packet_traces_and_commits(self):
        figures = [p for group in ("multi-edit", "designated-survivor")
                   for p in (ROOT / "experiment-results" / group / "figures").glob("*/plot.py")]
        self.assertEqual(len(figures), 6)
        for script in figures:
            with self.subTest(script=script):
                packets = read_csv(script.with_name("packets.csv"))
                self.assertTrue(packets)
                for p in packets:
                    self.assertIn("request_id", p)
                    self.assertIn("dropped", p)
                    self.assertIn("flow_name", p)
                    if p["pop_cycle"]:
                        self.assertEqual(int(p["delay_cycles"]), int(p["pop_cycle"]) - int(p["push_cycle"]))
                for c in read_csv(script.with_name("commits.csv")):
                    self.assertLessEqual(int(c["start_cycle"]), int(c["commit_cycle"]))
                    self.assertLessEqual(int(c["commit_cycle"]), int(c["ready_for_next_commit"]))
                    self.assertTrue(c["old_tree_drained_cycle"])


if __name__ == "__main__":
    unittest.main()
