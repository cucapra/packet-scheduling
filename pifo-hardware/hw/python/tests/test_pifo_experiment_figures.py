from __future__ import annotations

import csv
import json
import re
import sys
import tempfile
import unittest
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_figures.common import (  # noqa: E402
    PacketTiming,
    PolicyEvent,
    read_packet_results,
    read_policy_event,
)
from pifo_figures.bandwidth import (  # noqa: E402
    BandwidthSample,
    build_samples as build_bandwidth_samples,
    render_svg as render_bandwidth_svg,
)
from pifo_figures.packet_scatter import (  # noqa: E402
    render_svg as render_scatter_svg,
    write_data as write_packet_times,
)
from pifo_experiment_config import (  # noqa: E402
    PACKET_RATE_UNIT,
    DistributionSpec,
    PolicyChangeConfig,
    TrafficConfig,
    generate_distributed_requests,
    load_experiment_config,
)
from pifo_tree_compiler_core import (  # noqa: E402
    build_transaction_plan,
)
from pifo_transaction_program import controller_command_line  # noqa: E402


class PifoExperimentFiguresTest(unittest.TestCase):
    def test_bandwidth_uses_a_normalized_hann_convolution(self) -> None:
        packets = [
            PacketTiming(1, 1, 64, 0, 1),
            PacketTiming(2, 2, 32, 4, 5),
        ]
        event = PolicyEvent("RR", "SP", 4, 4, 4, 5)
        flow_ids, samples = build_bandwidth_samples(
            packets=packets,
            event=event,
            window_cycles=5,
            sample_cycles=1,
            link_bytes_per_cycle=16,
        )
        self.assertEqual(flow_ids, [1, 2])
        self.assertAlmostEqual(
            sum(sample.flow_bytes_per_cycle[1] for sample in samples),
            64,
        )
        self.assertAlmostEqual(
            sum(sample.flow_bytes_per_cycle[2] for sample in samples),
            32,
        )
        nonzero_flow_one = [
            sample.flow_bytes_per_cycle[1]
            for sample in samples
            if sample.flow_bytes_per_cycle[1] > 0
        ]
        self.assertEqual(len(nonzero_flow_one), 3)
        for actual, expected in zip(nonzero_flow_one, (16, 32, 16)):
            self.assertAlmostEqual(actual, expected)
        for sample in samples:
            self.assertAlmostEqual(
                sample.total_bytes_per_cycle,
                sum(sample.flow_bytes_per_cycle.values()),
            )

    def test_reads_results_and_writes_required_packet_timing_columns(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            results = root / "results.csv"
            events = root / "events.csv"
            packet_times = root / "packet-times.csv"
            results.write_text(
                "request_id,global_flow_id,size_bytes,arrival_cycle,admitted_cycle,"
                "completed_cycle,admission_delay_cycles,sojourn_cycles\n"
                "7,2,128,10,11,20,1,10\n",
                encoding="utf-8",
            )
            events.write_text(
                "event,name,mode,from_policy,to_policy,instruction_count,scheduled_cycle,start_cycle,"
                "commit_cycle,finish_cycle,drain_cycle,drain_duration_cycles\n"
                "reconfiguration,change,full_transitive,RR,SP,10,12,13,14,20,18,4\n",
                encoding="utf-8",
            )

            packets = read_packet_results(results)
            event = read_policy_event(events)
            self.assertEqual(
                (
                    event.start_cycle,
                    event.commit_cycle,
                    event.finish_cycle,
                    event.drain_cycle,
                ),
                (13, 14, 20, 18),
            )
            self.assertEqual(event.instruction_count, 10)
            write_packet_times(packet_times, packets, event)

            with packet_times.open(newline="", encoding="utf-8") as source:
                rows = list(csv.DictReader(source))
            self.assertEqual(rows[0]["flow_id"], "2")
            self.assertEqual(rows[0]["input_cycle"], "10")
            self.assertEqual(rows[0]["output_cycle"], "20")
            self.assertEqual(rows[0]["input_relative_to_start"], "-3")
            self.assertEqual(rows[0]["output_relative_to_start"], "7")

    def test_reads_legacy_policy_event_without_commit_cycle(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            events = Path(directory) / "events.csv"
            events.write_text(
                "event,from_policy,to_policy,scheduled_cycle,request_cycle,"
                "complete_cycle\n"
                "policy_switch,RR,SP,12,13,15\n",
                encoding="utf-8",
            )
            event = read_policy_event(events)
            self.assertEqual(event.start_cycle, 13)
            self.assertEqual(event.commit_cycle, 13)
            self.assertEqual(event.finish_cycle, 15)

    def test_reads_lossless_stop_event(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            events = Path(directory) / "events.csv"
            events.write_text(
                "event,name,mode,from_policy,to_policy,instruction_count,scheduled_cycle,"
                "start_cycle,commit_cycle,finish_cycle,drain_cycle,drain_duration_cycles,"
                "dropped_packets,retained_packets,minimum_stop_cycles,stop_duration_cycles\n"
                "reconfiguration,stop,stop_the_world,p1,p2,16,2000,2000,2031,3038,"
                "2013,,0,137,1024,1025\n",
                encoding="utf-8",
            )
            event = read_policy_event(events)

        self.assertEqual(event.mode, "stop_the_world")
        self.assertEqual(event.dropped_packets, 0)
        self.assertEqual(event.retained_packets, 137)
        self.assertEqual(event.minimum_stop_cycles, 1024)
        self.assertEqual(event.stop_duration_cycles, 1025)

    def test_seeded_uniform_rate_and_normal_size_are_reproducible(self) -> None:
        traffic = TrafficConfig(
            flow_ids=(1, 2),
            packets_per_flow=20,
            start_cycle=3,
            packet_rate=DistributionSpec(
                distribution="uniform",
                minimum=0.1,
                maximum=0.2,
                unit=PACKET_RATE_UNIT,
            ),
            packet_size_bytes=DistributionSpec(
                distribution="normal",
                mean=256,
                stddev=80,
                minimum=64,
                maximum=512,
            ),
        )
        first = generate_distributed_requests(traffic, seed=19)
        second = generate_distributed_requests(traffic, seed=19)
        self.assertEqual(first, second)
        self.assertEqual(len(first), 40)
        self.assertEqual(first[0].cycle, 3)
        self.assertTrue(all(64 <= request.size_bytes <= 512 for request in first))
        round_cycles = [first[index].cycle for index in range(0, len(first), 2)]
        gaps = [right - left for left, right in zip(round_cycles, round_cycles[1:])]
        self.assertTrue(all(5 <= gap <= 10 for gap in gaps))

    def test_normal_rate_and_uniform_size_are_supported(self) -> None:
        traffic = TrafficConfig(
            flow_ids=(3,),
            packets_per_flow=20,
            start_cycle=0,
            packet_rate=DistributionSpec(
                distribution="normal",
                mean=0.125,
                stddev=0.02,
                minimum=0.08,
                maximum=0.2,
                unit=PACKET_RATE_UNIT,
            ),
            packet_size_bytes=DistributionSpec(
                distribution="uniform",
                minimum=64,
                maximum=1500,
            ),
        )
        requests = generate_distributed_requests(traffic, seed=23)
        self.assertEqual(len(requests), 20)
        self.assertTrue(all(64 <= request.size_bytes <= 1500 for request in requests))
        self.assertEqual(requests, generate_distributed_requests(traffic, seed=23))

    def test_checked_in_json_config_is_valid(self) -> None:
        config_path = (
            Path(__file__).resolve().parents[3] / "experiments" / "rr-to-sp.json"
        )
        config = load_experiment_config(config_path)
        requests = generate_distributed_requests(config.traffic, config.seed)
        self.assertEqual(len(requests), 480)
        self.assertIsInstance(config.reconfiguration, PolicyChangeConfig)
        assert isinstance(config.reconfiguration, PolicyChangeConfig)
        self.assertEqual(config.reconfiguration.before_label, "RR")
        self.assertEqual(config.reconfiguration.after_label, "SP")
        self.assertEqual(
            config.reconfiguration.changes["root"].flow_state,
            {1: 1, 2: 32769},
        )
        plan = build_transaction_plan(
            config.initial_tree,
            config.reconfiguration,
            config.simulation.num_vpifos,
        )
        self.assertEqual(plan.mode, "full_transitive")
        self.assertEqual((plan.drain_engine_id, plan.drain_vpifo_id), (1, 10))
        lines = [controller_command_line(command) for command in plan.transaction_commands]
        self.assertIn(
            "command=UpdateMapperNonExist engineId=1 vPifoId=10 flowId=0 data=11",
            lines,
        )
        self.assertEqual(
            sum(command.command == "UpdateMapperNonExist" for command in plan.transaction_commands),
            1,
        )
        self.assertEqual(plan.transaction_commands[-1].command, "CommitMapper")

    def test_full_transitive_change_copies_every_tree_node(self) -> None:
        example_path = (
            Path(__file__).resolve().parents[3] / "experiments" / "rr-to-sp.json"
        )
        raw = json.loads(example_path.read_text(encoding="utf-8"))
        raw["initial_tree"] = {
            "root": "root",
            "nodes": {
                "root": {
                    "engine_id": 1,
                    "vpifo_id": 10,
                    "policy": "RR",
                },
                "leaf": {
                    "engine_id": 2,
                    "vpifo_id": 12,
                    "policy": "FIFO",
                },
            },
            "flow_paths": {"1": ["root", "leaf"], "2": ["root", "leaf"]},
        }
        with tempfile.TemporaryDirectory() as directory:
            config_path = Path(directory) / "tree.json"
            config_path.write_text(json.dumps(raw), encoding="utf-8")
            config = load_experiment_config(config_path)
            plan = build_transaction_plan(
                config.initial_tree,
                config.reconfiguration,
                config.simulation.num_vpifos,
            )
        brain_targets = {
            (command.engine_id, command.vpifo_id)
            for command in plan.transaction_commands
            if command.command == "UpdateBrainEngine"
        }
        self.assertEqual(brain_targets, {(1, 11), (2, 10)})
        new_root_post = next(
            command
            for command in plan.transaction_commands
            if command.command == "UpdateMapperPost"
            and command.engine_id == 1
            and command.vpifo_id == 11
            and command.flow_id == 33
        )
        self.assertEqual(new_root_post.data, 74)

    def test_json_config_rejects_unknown_fields(self) -> None:
        example_path = (
            Path(__file__).resolve().parents[3] / "experiments" / "rr-to-sp.json"
        )
        raw = json.loads(example_path.read_text(encoding="utf-8"))
        raw["traffic"]["packet_raet"] = raw["traffic"]["packet_rate"]
        with tempfile.TemporaryDirectory() as directory:
            config_path = Path(directory) / "bad.json"
            config_path.write_text(json.dumps(raw), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "unknown field.*packet_raet"):
                load_experiment_config(config_path)

    def test_tree_rejects_vpifo_zero_null_sink(self) -> None:
        example_path = (
            Path(__file__).resolve().parents[3] / "experiments" / "rr-to-sp.json"
        )
        raw = json.loads(example_path.read_text(encoding="utf-8"))
        raw["initial_tree"] = {
            "root": "root",
            "nodes": {
                "root": {"engine_id": 1, "vpifo_id": 0, "policy": "RR"}
            },
            "flow_paths": {"1": ["root"], "2": ["root"]},
        }
        with tempfile.TemporaryDirectory() as directory:
            config_path = Path(directory) / "bad-null-tree.json"
            config_path.write_text(json.dumps(raw), encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "vPIFO 0 is the null sink"):
                load_experiment_config(config_path)

    def test_dependency_free_svg_renderer_writes_both_figures(self) -> None:
        event = PolicyEvent(
            "RR", "SP", 4, 4, 5, 8, drain_cycle=7, instruction_count=2
        )
        packets = [
            PacketTiming(1, 1, 64, 0, 2),
            PacketTiming(2, 2, 64, 4, 10),
        ]
        samples = [
            BandwidthSample(0, 4, -2, 16, 1, {1: 16, 2: 0}, {1: 1, 2: 0}),
            BandwidthSample(4, 8, 2, 16, 1, {1: 0, 2: 16}, {1: 0, 2: 1}),
        ]
        with tempfile.TemporaryDirectory() as directory:
            bandwidth = Path(directory) / "bandwidth.svg"
            scatter = Path(directory) / "scatter.svg"
            render_bandwidth_svg(
                bandwidth,
                [1, 2],
                samples,
                event,
                {1: "A", 2: "B"},
                100,
            )
            render_scatter_svg(scatter, packets, event, {1: "A", 2: "B"}, 100)
            bandwidth_text = bandwidth.read_text(encoding="utf-8")
            self.assertIn("commit accepted", bandwidth_text)
            self.assertIn("config=2 inst", bandwidth_text)
            scatter_text = scatter.read_text(encoding="utf-8")
            self.assertIn("Packet input", scatter_text)
            for color in ("#1f77b4", "#ff7f0e", "#9467bd"):
                # Each event color has an input-axis line, an output-axis line,
                # and a legend sample.
                self.assertGreaterEqual(
                    len(re.findall(rf'<line [^>]*stroke="{color}"', scatter_text)),
                    3,
                )
            svg_size = re.search(
                r'<svg [^>]*width="([0-9]+)" height="([0-9]+)"', scatter_text
            )
            self.assertIsNotNone(svg_size)
            assert svg_size is not None
            self.assertEqual(svg_size.group(1), svg_size.group(2))
            diagonal = re.search(
                r'<line x1="([0-9.]+)" y1="([0-9.]+)" '
                r'x2="([0-9.]+)" y2="([0-9.]+)" '
                r'stroke="#6baed6"',
                scatter_text,
            )
            self.assertIsNotNone(diagonal)
            assert diagonal is not None
            x1, y1, x2, y2 = map(float, diagonal.groups())
            self.assertAlmostEqual(
                abs(x2 - x1), abs(y2 - y1), delta=0.02
            )


if __name__ == "__main__":
    unittest.main()
