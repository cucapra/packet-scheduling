"""Regression checks against committed native synthesis reports."""
import copy
from pathlib import Path
import unittest

from summarize_quartus import summarize, zero_memory_usage

RESULTS = Path(__file__).resolve().parents[1] / "experiment-results/hardware-overhead"
PIFO = RESULTS / "pifo-component/runs/pifo-house-1push-v32-c1024-t8-quartus/output_files/pifo.syn.rpt"
OVERFLOW = RESULTS / "r2-vflows/runs/rio-only-pe5-v1024-c1024-dynamic-quartus/output_files/pifo.syn.rpt"
REPLAY = RESULTS / "r4-replay/journal-m20k/runs/full-m20k/output_files/pifo.syn.rpt"


class MemoryReportingTest(unittest.TestCase):
    def test_ram_free_component_has_evidenced_zero(self):
        report = summarize(PIFO)
        evidence = report["zero_memory_usage"]
        self.assertEqual(evidence["block_memory_bits"], 0)
        self.assertEqual(evidence["mlab_memory_bits"], 0)
        self.assertEqual(evidence["hierarchy_entities_checked"], 4)
        self.assertEqual(evidence["ram_instances"], 0)

    def test_unexplained_missing_memory_is_not_zero(self):
        tables = copy.deepcopy(summarize(PIFO)["tables"])
        key = next(k for k in tables if "Resource Utilization by Entity" in k)
        del tables[key]
        self.assertIsNone(zero_memory_usage(tables))

    def test_overflow_still_recovers_positive_memory(self):
        report = summarize(OVERFLOW)
        self.assertGreater(report["recovered_memory_total"]["block_memory_bits"], 2**31)
        self.assertIn("mlab_memory_bits", report["unreported_resources"])
        self.assertNotIn("zero_memory_usage", report)

    def test_reported_replay_memory_is_unchanged(self):
        report = summarize(REPLAY)
        usage = next(rows for name, rows in report["tables"].items()
                     if name.startswith("Synthesis Resource Usage Summary"))
        values = {row[0]: row[1] for row in usage}
        self.assertEqual(int(values["Total block memory bits"]), 1426719512)
        self.assertNotIn("zero_memory_usage", report)


if __name__ == "__main__":
    unittest.main()
