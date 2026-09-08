import sys
import shutil
import subprocess
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from pifo_survivor_common import RESOURCES, make_traffic, settings, peak_buffer
from pifo_survivor_compiler import MECHANISMS, compile_survivor
from pifo_tree_compiler import load_tree_move_program
from pifo_tree_compiler_core import pack_flow_id
from pifo_transaction_program import load_transaction_program, write_transaction_program
from pifo_traffic_program import generate_traffic, load_traffic_program
from pifo_figures.evaluation import read_csv


class SurvivorTest(unittest.TestCase):
    def test_link_writes_exactly_one_pointer_and_never_stops(self):
        source = load_tree_move_program(RESOURCES / "tree-move.json")
        for pre in (0, 1500, 7500):
            program, plan = compile_survivor(source, "link", pre)
            commands = program.transactions[0].commands
            self.assertEqual(plan["creation_instruction_categories"]["designate"], 1)
            self.assertFalse(any(c.command in {"StopWorld", "PrefillPifo", "CopyPifoEngine"} for c in commands))

    def test_reserved_uses_hardware_count_and_reclaims_after_publication(self):
        source = load_tree_move_program(RESOURCES / "tree-move.json")
        program, plan = compile_survivor(source, "reserved", 2000)
        main, collapse, reclaim = program.transactions
        prefill = next(c for c in main.commands if c.command == "PrefillPifo")
        self.assertEqual(prefill.data, 0)
        self.assertNotEqual(prefill.vpifo_id, 0)
        self.assertEqual(prefill.engine_id, 4)
        self.assertFalse(any(c.command == "CopyPifoEngine" for c in main.commands))
        self.assertEqual(collapse.commands[0].command, "GuardDrain")
        self.assertEqual(collapse.cleanup_of, main.name)
        self.assertEqual(reclaim.cleanup_of, collapse.name)
        self.assertEqual(collapse.commands[-2].command, "UpdateRoot")
        self.assertEqual(reclaim.commands[0].command, "ClearPifoEngine")
        self.assertEqual(sum(plan["creation_instruction_categories"].values()), len(main.commands))
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "transactions.txt"
            write_transaction_program(path, program)
            self.assertEqual(program, load_transaction_program(path))

    def test_every_flow_pushes_and_pops_through_a_distinct_hardware_fifo(self):
        source = load_tree_move_program(RESOURCES / "tree-move.json")
        for mechanism in MECHANISMS:
            program, plan = compile_survivor(source, mechanism, 2000)
            for policy, commands in (("p1", program.initial.commands), ("p2b", program.transactions[0].commands)):
                paths = plan["compiled_flow_paths"][policy]
                leaves = []
                for flow, path in paths.items():
                    fid = int(flow)
                    self.assertEqual(path[-1][2], "FIFO")
                    self.assertEqual(path[-1][0], 4 if mechanism == "copy" and policy == "p2b" else 3)
                    leaves.append(tuple(path[-1][:2]))
                    expected_hops = 3 if policy == "p2b" and fid in (2, 3) else 2
                    self.assertEqual(len(path), expected_hops)
                    for index, (engine, port, brain) in enumerate(path):
                        self.assertTrue(any(c.command == "UpdateMapperPre" and
                            (c.engine_id, c.vpifo_id, c.data) == (engine, fid, port) for c in commands))
                        target = pack_flow_id(*path[index + 1][:2], source.hardware.num_vpifos) if index + 1 < len(path) else fid
                        token = pack_flow_id(engine, fid, source.hardware.num_vpifos)
                        self.assertTrue(any(c.command == "UpdateMapperPost" and
                            (c.engine_id, c.vpifo_id, c.flow_id, c.data) == (engine, port, token, target) for c in commands))
                        if brain == "FIFO":
                            self.assertTrue(any(c.command == "UpdateBrainEngine" and
                                (c.engine_id, c.vpifo_id, c.data) == (engine, port, 3) for c in commands))
                self.assertEqual(len(set(leaves)), len(leaves))
            self.assertTrue(set(tuple(p[-1][:2]) for p in plan["compiled_flow_paths"]["p1"].values()).isdisjoint(
                tuple(p[-1][:2]) for p in plan["compiled_flow_paths"]["p2b"].values()))

    def test_copy_moves_root_and_fifos_with_original_token_ids(self):
        source = load_tree_move_program(RESOURCES / "tree-move.json")
        program, plan = compile_survivor(source, "copy", 2000)
        reserved, _ = compile_survivor(source, "reserved", 2000)
        self.assertEqual(program.initial, reserved.initial)
        self.assertEqual(program.hardware, reserved.hardware)
        self.assertEqual(plan["copy_list"], [[3, 4], [1, 2]])
        self.assertEqual(plan["reserved_pes"], 0)
        self.assertEqual(plan["wrapper"], [1, 1])
        main, cleanup, reclaim = program.transactions
        self.assertEqual(main.drain_root, (2, 1))
        self.assertEqual([c.command for c in main.commands[:3]], ["StopWorld", "CopyPifoEngine", "CopyPifoEngine"])
        self.assertEqual((main.commands[0].engine_id, main.commands[0].vpifo_id), (1, 1))
        # Frozen tokens keep engine-1/engine-3 IDs after moving to PEs 2/4.
        for flow in (1, 2):
            for engine, port, token, target in ((2, 1, pack_flow_id(1, flow, 8), pack_flow_id(4, flow, 8)),
                                                (4, flow, pack_flow_id(3, flow, 8), flow)):
                self.assertTrue(any(c.command == "UpdateMapperPost" and
                    (c.engine_id, c.vpifo_id, c.flow_id, c.data) == (engine, port, token, target)
                    for c in main.commands))
                self.assertTrue(any(c.command == "UpdateMapperPost" and
                    (c.engine_id, c.vpifo_id, c.flow_id, c.data) == (engine, port, token, 0)
                    for c in cleanup.commands))
        self.assertEqual({(c.engine_id, c.vpifo_id) for c in cleanup.commands if c.command == "GuardDrain"},
                         {(2, 1), (4, 1), (4, 2)})
        self.assertEqual(reclaim.commands[0].command, "ClearPifoEngine")
        self.assertEqual(reclaim.commands[0].engine_id, 1)
        self.assertEqual(sum(plan["creation_instruction_categories"].values()), len(main.commands))
        self.assertEqual(plan["creation_instruction_categories"]["copy"], 2)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "transactions.txt"
            write_transaction_program(path, program)
            self.assertEqual(program, load_transaction_program(path))

    def test_wrapper_cannot_reuse_the_new_rr_engine(self):
        source = load_tree_move_program(RESOURCES / "tree-move.json")
        source = replace(source, hardware=replace(source.hardware, num_engines=3))
        with self.assertRaisesRegex(ValueError, "unused by both"):
            compile_survivor(source, "reserved", 2000)

    def test_trace_has_zero_prephase_option_and_continuous_offered_load(self):
        with tempfile.TemporaryDirectory() as directory:
            for pre in (0, 2000):
                path, _ = make_traffic(Path(directory), pre, settings())
                traffic = load_traffic_program(path)
                requests = generate_traffic(traffic)
                self.assertEqual(requests, generate_traffic(traffic))
                self.assertTrue(any(pre + 10 < r.cycle < pre + 500 and r.global_flow_id == 1 for r in requests))
                self.assertFalse(any(r.cycle < pre and r.global_flow_id == 3 for r in requests))
                self.assertEqual({r.size_bytes for r in requests}, {48})

    def test_peak_counts_packets_held_at_door(self):
        rows = [{"push_cycle": 0, "pop_cycle": 10}, {"push_cycle": 2, "pop_cycle": 11},
                {"push_cycle": 6, "pop_cycle": 13}]
        self.assertEqual(peak_buffer(rows, 3, 7), 3)

    def test_both_saved_figures_include_copy_and_its_commit_markers(self):
        results = RESOURCES.parents[1] / "experiment-results/designated-survivor"
        for figure in ("zoom-delay", "prefill-stop"):
            path = results / "figures" / figure
            self.assertEqual({r["run"] for r in read_csv(path / "data.csv")}, set(MECHANISMS))
            packets = read_csv(path / "packets.csv")
            self.assertEqual({r["run"].split("/")[-1] for r in packets}, set(MECHANISMS))
            commits = read_csv(path / "commits.csv")
            copy = [r for r in commits if r["run"].split("/")[-1] == "copy"]
            self.assertEqual({r["commit"] for r in copy}, {"C1", "C2", "C3"})
            self.assertTrue(all(r["old_tree_drained_cycle"] and r["ready_for_next_commit"] for r in copy))

    def test_saved_survivor_figures_replot_from_only_local_csvs(self):
        figures = RESOURCES.parents[1] / "experiment-results/designated-survivor/figures"
        with tempfile.TemporaryDirectory() as directory:
            for name in ("zoom-delay", "prefill-stop"):
                target = Path(directory) / name
                target.mkdir()
                for source in (figures / name).iterdir():
                    if source.suffix in {".py", ".csv"}:
                        shutil.copy2(source, target / source.name)
                subprocess.run([sys.executable, "-I", str(target / "plot.py")], cwd=directory,
                               check=True, capture_output=True, text=True, timeout=30)
                svg = (target / "figure.svg").read_text()
                self.assertIn("Copy + prefill Strict wrapper", svg)
                if name == "zoom-delay":
                    for commit in ("C1", "C2", "C3"):
                        for marker in ("start", "commit accepted", "ready_for_next_commit", "old-tree-drained"):
                            self.assertIn(f"Copy {commit} {marker}", svg)
                self.assertGreater((target / "figure.png").stat().st_size, 0)


if __name__ == "__main__":
    unittest.main()
