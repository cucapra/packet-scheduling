from __future__ import annotations

import csv
import os
import sys
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path
from argparse import Namespace

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_figures.common import FINISH_LABEL, FigurePaths, PacketTiming, PolicyEvent, commit_accounting, read_policy_event
from pifo_tree_compiler import compile_tree_move, load_tree_move_program
from pifo_transaction_program import load_transaction_program, write_transaction_program
from pifo_experiment_verify import read_transaction_timing
from pifo_experiment_config import DistributionSpec, TrafficConfig
from pifo_traffic_program import TrafficPattern, TrafficProgram, write_traffic_program
from pifo_simulator import run_simulator
from pifo_motivation_plot import PacketOutcome, read_packet_outcomes, render_delay_scatter
from pifo_figures.bandwidth import build_samples, render_matplotlib as render_bandwidth
from pifo_figures.packet_scatter import render_matplotlib as render_scatter

ROOT = Path(__file__).resolve().parents[3]


class CleanupTest(unittest.TestCase):
    @unittest.skipUnless(os.environ.get("PIFO_RTL_SMOKE") == "1", "opt-in small RTL smoke (56 packets)")
    def test_compiler_simulator_cleanup_end_to_end(self):
        source = load_tree_move_program(ROOT / "experiments/motivating-example/r3-whole-tree/tree-move.json")
        source = replace(source, hardware=replace(source.hardware, fifo_depth=4, prefetch_buffer_depth=2),
                         move=replace(source.move, cycle=40))
        rate = DistributionSpec(distribution="constant", value=1, unit="packets_per_cycle_per_flow")
        size = DistributionSpec(distribution="constant", value=160)
        traffic = TrafficProgram(7, (
            TrafficPattern("before", TrafficConfig((1, 2), 16, 0, rate, size)),
            TrafficPattern("after", TrafficConfig((1, 2, 3), 8, 80, rate, size)),
        ))
        with tempfile.TemporaryDirectory(prefix="pifo-cleanup-smoke-") as directory:
            root = Path(directory)
            write_transaction_program(root / "transactions.txt", compile_tree_move(source))
            write_traffic_program(root / "traffic.json", traffic)
            _, _, outcomes_path, event_path = run_simulator(Namespace(
                transactions=root / "transactions.txt", traffic=root / "traffic.json",
                output_dir=root, sbt="sbt", queue_depth=32, link_bytes_per_cycle=16,
                max_cycles=3000, warmup_cycles=4, wave=False, verbose=False,
            ))
            event = read_policy_event(event_path)
            packets = read_packet_outcomes(outcomes_path)
            self.assertEqual(len(packets), 56)
            self.assertTrue(all(not packet.dropped and packet.pop_cycle is not None for packet in packets))
            for flow in (1, 2, 3):
                served = sorted((p for p in packets if p.flow_id == flow), key=lambda p: p.pop_cycle)
                self.assertEqual([p.request_id for p in served], sorted(p.request_id for p in served))
            self.assertGreaterEqual(event.cleanup_applied_cycle, event.drain_cycle)
            self.assertEqual(event.finish_cycle, event.cleanup_finish_cycle)
            self.assertGreater(event.finish_cycle, event.cleanup_applied_cycle)
            self.assertEqual(event.instruction_count, 17)
            self.assertGreater(event.cleanup_instruction_count, 1)
            print(f"Cleanup smoke PASS: 56 packets, zero drops/reorders; "
                  f"start={event.start_cycle} accepted={event.commit_cycle} drain={event.drain_cycle} "
                  f"finish={event.finish_cycle}; {commit_accounting(event)}")

    def test_all_modes_have_an_ordinary_cleanup_commit(self):
        for resource in (ROOT / "experiments/motivating-example").glob("*/tree-move.json"):
            with self.subTest(resource=resource):
                program = compile_tree_move(load_tree_move_program(resource))
                install, cleanup = program.transactions
                self.assertEqual(cleanup.at_cycle, install.at_cycle)
                self.assertEqual(cleanup.cleanup_of, install.name)
                self.assertEqual(cleanup.mode, "direct")
                self.assertEqual(cleanup.commands[-1].command, "CommitMapper")
                self.assertTrue(all(command.data == 0 for command in cleanup.commands))
                guards = [c for c in cleanup.commands if c.command == "GuardDrain"]
                self.assertEqual(list(cleanup.commands[:len(guards)]), guards)
                if install.mode in {"in_place", "stop_the_world"}:
                    self.assertEqual(len(cleanup.commands), 1)
                else:
                    self.assertTrue(guards)
                with tempfile.TemporaryDirectory() as directory:
                    path = Path(directory) / "transactions.txt"
                    write_transaction_program(path, program)
                    self.assertEqual(load_transaction_program(path), program)

    def test_confined_cleanup_preserves_ancestors_and_live_rewrite(self):
        program = compile_tree_move(load_tree_move_program(
            ROOT / "experiments/motivating-example/r4-confined/tree-move.json"
        ))
        install, cleanup = program.transactions
        retired = {(c.engine_id, c.vpifo_id) for c in cleanup.commands if c.command == "GuardDrain"}
        self.assertEqual(retired, {(2, 2)})
        for command in cleanup.commands:
            if command.command != "CommitMapper":
                self.assertIn((command.engine_id, command.vpifo_id), retired)
        self.assertFalse(any(c.command == "UpdateMapperNonExist" for c in cleanup.commands))
        live_pre = {(c.engine_id, c.vpifo_id) for c in install.commands if c.command == "UpdateMapperPre"}
        self.assertFalse(any(
            c.command == "UpdateMapperPre" and (c.engine_id, c.vpifo_id) in live_pre
            for c in cleanup.commands
        ))

    def test_full_cleanup_guards_every_retired_node(self):
        source = load_tree_move_program(ROOT / "experiments/motivating-example/r3-whole-tree/tree-move.json")
        install, cleanup = compile_tree_move(source).transactions
        retired = {(n.engine_id, n.vpifo_id) for n in source.old_tree.nodes.values()}
        self.assertEqual({(c.engine_id, c.vpifo_id) for c in cleanup.commands if c.command == "GuardDrain"}, retired)
        self.assertEqual({(c.engine_id, c.vpifo_id) for c in cleanup.commands if c.command == "UpdateBrainEngine"}, retired)
        live_posts = {(c.engine_id, c.vpifo_id, c.flow_id) for c in install.commands if c.command == "UpdateMapperPost"}
        self.assertFalse(any(
            (c.engine_id, c.vpifo_id, c.flow_id) in live_posts
            for c in cleanup.commands if c.command == "UpdateMapperPost"
        ))

    def test_cleanup_metadata_cannot_reference_future_transition(self):
        program = compile_tree_move(load_tree_move_program(
            ROOT / "experiments/motivating-example/r3-whole-tree/tree-move.json"
        ))
        with self.assertRaisesRegex(ValueError, "earlier"):
            replace(program, transactions=tuple(reversed(program.transactions)))

    def test_old_inputs_removed_from_target_path_are_quiesced_at_install(self):
        source = load_tree_move_program(ROOT / "experiments/motivating-example/r3-whole-tree/tree-move.json")
        target = source.move.target_tree
        target = replace(target, nodes={"root": target.nodes["root"]},
                         flow_paths={flow: ("root",) for flow in target.flow_paths})
        install, cleanup = compile_tree_move(replace(source, move=replace(source.move, target_tree=target))).transactions
        for transaction in (install, cleanup):
            self.assertTrue(any(
                c.command == "UpdateMapperPre" and (c.engine_id, c.vpifo_id, c.data) == (2, 2, 0)
                for c in transaction.commands
            ))

    def test_finish_and_resume_are_distinct_and_cleanup_has_both_costs(self):
        row = dict(
            event="reconfiguration", name="move", mode="stop_the_world", from_policy="RR", to_policy="SP",
            instruction_count=9, scheduled_cycle=10, start_cycle=10, commit_cycle=25,
            commit_applied_cycle=27, commit_cycles=17, bank_cleanup_cycles=33, install_finish_cycle=60,
            drain_cycle=12, finish_cycle=170, retained_packets=3, peak_buffer_occupancy_packets=20,
            minimum_stop_cycles=100, stop_duration_cycles=100, resume_cycle=112,
            cleanup_start_cycle=113, cleanup_commit_cycle=115, cleanup_applied_cycle=137,
            cleanup_finish_cycle=170, cleanup_instruction_count=4, cleanup_commit_cycles=24,
            cleanup_bank_cleanup_cycles=33,
        )
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "events.csv"
            with path.open("w", newline="") as out:
                writer = csv.DictWriter(out, fieldnames=row)
                writer.writeheader()
                writer.writerow(row)
                writer.writerow(dict(row, event="cleanup_commit", name="move-cleanup"))
            event = read_policy_event(path)
            self.assertEqual(event.finish_cycle, 170)
            self.assertEqual(event.traffic_resume_cycle, 112)
            self.assertIn("config=9 inst / 17 cycles", commit_accounting(event))
            self.assertIn("cleanup=4 inst / 24 cycles", commit_accounting(event))
            self.assertIn("double-buffer cleanup", FINISH_LABEL)

    def test_rr_verifier_ignores_cleanup_event_row(self):
        row = dict(event="reconfiguration", mode="full_transitive", start_cycle=10,
                   commit_cycle=20, finish_cycle=100, drain_cycle=30)
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "events.csv"
            with path.open("w", newline="") as out:
                writer = csv.DictWriter(out, fieldnames=row)
                writer.writeheader()
                writer.writerow(row)
                writer.writerow(dict(row, event="cleanup_commit"))
            self.assertEqual(read_transaction_timing(path).finish_cycle, 100)

    def test_renderers_label_finish_and_both_commit_costs(self):
        event = PolicyEvent(
            "RR", "SP", 10, 10, 20, 140, drain_cycle=55, instruction_count=7,
            commit_applied_cycle=22, commit_cycles=12, bank_cleanup_cycles=33, install_finish_cycle=55,
            cleanup_start_cycle=55, cleanup_commit_cycle=62, cleanup_applied_cycle=107,
            cleanup_finish_cycle=140, cleanup_instruction_count=6, cleanup_commit_cycles=52,
            cleanup_bank_cleanup_cycles=33,
        )
        packets = (PacketTiming(1, 1, 48, 0, 50), PacketTiming(2, 2, 48, 20, 65))
        flows, samples = build_samples(packets, event, 8, 2, 16)
        with tempfile.TemporaryDirectory() as directory:
            for name, renderer in (
                ("bandwidth", lambda paths: render_bandwidth(paths, flows, samples, event, {}, 80)),
                ("scatter", lambda paths: render_scatter(paths, packets, event, {}, 80)),
                ("delay", lambda paths: render_delay_scatter(paths, [
                    PacketOutcome(p.request_id, p.flow_id, p.size_bytes, p.input_cycle, p.output_cycle, False)
                    for p in packets
                ], event, {}, 80, "Test transition")),
            ):
                paths = FigurePaths(*(Path(directory) / f"{name}.{extension}" for extension in ("csv", "svg", "png")))
                renderer(paths)
                svg = paths.svg.read_text()
                self.assertIn(FINISH_LABEL, svg)
                self.assertIn("config=7 inst / 12 cycles", svg)
                self.assertIn("cleanup=6 inst / 52 cycles", svg)
                self.assertGreater(paths.png.stat().st_size, 0)


if __name__ == "__main__":
    unittest.main()
