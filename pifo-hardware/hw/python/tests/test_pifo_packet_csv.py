from __future__ import annotations

import csv
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_figures.bandwidth import build_parser
from pifo_figures.common import (
    load_figure_inputs, read_packet_outcomes, read_run_packet_outcomes, write_packet_outcomes,
)


class PacketCsvTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.results = self.root / "request-results.csv"
        self.results.write_text(
            "request_id,global_flow_id,size_bytes,arrival_cycle,admitted_cycle,completed_cycle\n"
            "1,1,48,10,1010,1020\n"
        )
        self.requests = self.root / "requests.csv"
        self.requests.write_text("cycle,request_id,global_flow_id,size_bytes\n10,1,1,48\n20,2,2,64\n")
        self.outcomes = self.root / "packet-outcomes.csv"
        self.outcomes.write_text("request_id,flow,size_bytes,push_cycle,pop_cycle,dropped\n"
                                 "1,1,48,10,1020,false\n2,2,64,20,,true\n")

    def test_default_figure_input_includes_drops_and_source_wait(self):
        event = self.root / "events.csv"
        event.write_text("event,from_policy,to_policy,scheduled_cycle,start_cycle,commit_cycle,finish_cycle\n"
                         "reconfiguration,RR,SP,10,10,12,15\n")
        args = build_parser().parse_args([
            "--results", str(self.results), "--events", str(event), "--output-dir", str(self.root / "figure"),
            "--link-bytes-per-cycle", "16", "--window-cycles", "8", "--sample-cycles", "2",
        ])
        inputs = load_figure_inputs(args)
        self.assertEqual(len(inputs.packets), 1)
        self.assertEqual(len(inputs.packet_outcomes), 2)
        trace = self.root / "packets.csv"
        write_packet_outcomes(trace, inputs.packet_outcomes, {1: "zoom", 2: "gmail"})
        self.assertEqual(read_packet_outcomes(trace), list(inputs.packet_outcomes))
        with trace.open() as stream:
            rows = list(csv.DictReader(stream))
        self.assertEqual((rows[0]["flow_name"], rows[0]["push_cycle"], rows[0]["delay_cycles"]),
                         ("zoom", "10", "1010"))
        self.assertEqual((rows[1]["flow_name"], rows[1]["pop_cycle"], rows[1]["delay_cycles"], rows[1]["dropped"]),
                         ("gmail", "", "", "true"))

    def test_missing_packets_are_not_silently_inferred_as_dropped(self):
        self.outcomes.unlink()
        with self.assertRaisesRegex(ValueError, "cover every generated request"):
            read_run_packet_outcomes(self.results)

    def test_lossless_legacy_conversion_requires_complete_request_coverage(self):
        self.outcomes.unlink()
        self.requests.write_text("cycle,request_id,global_flow_id,size_bytes\n10,1,1,48\n")
        trace = read_run_packet_outcomes(self.results)
        self.assertEqual(len(trace), 1)
        self.assertEqual((trace[0].push_cycle, trace[0].pop_cycle, trace[0].dropped), (10, 1020, False))
        self.requests.unlink()
        with self.assertRaisesRegex(ValueError, "completion results alone"):
            read_run_packet_outcomes(self.results)

    def test_explicit_missing_outcomes_do_not_fall_back_to_completions(self):
        with self.assertRaises(FileNotFoundError):
            read_run_packet_outcomes(self.results, self.root / "missing.csv")

    def test_rejects_admission_time_and_mismatched_runs(self):
        self.outcomes.write_text("request_id,flow,size_bytes,push_cycle,pop_cycle,dropped\n"
                                 "1,1,48,1010,1020,false\n2,2,64,20,,true\n")
        with self.assertRaisesRegex(ValueError, "disagree"):
            read_run_packet_outcomes(self.results)

    def test_rejects_incomplete_outcomes_even_if_completions_match(self):
        self.outcomes.write_text("request_id,flow,size_bytes,push_cycle,pop_cycle,dropped\n1,1,48,10,1020,false\n")
        with self.assertRaisesRegex(ValueError, "cover every generated request"):
            read_run_packet_outcomes(self.results)


if __name__ == "__main__":
    unittest.main()
