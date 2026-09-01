from __future__ import annotations

import sys
import unittest
from dataclasses import replace
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_experiment_config import (  # noqa: E402
    PhaseVerificationConfig,
    load_experiment_config,
)
from pifo_tree_compiler_core import build_transaction_plan  # noqa: E402
from pifo_experiment_verify import (  # noqa: E402
    CompletedPacket,
    TransactionTiming,
    verify_rr_to_sp_phases,
)


HARDWARE_ROOT = Path(__file__).resolve().parents[3]
LARGE_CONFIG = HARDWARE_ROOT / "experiments" / "large-tree-rr-to-sp.json"


class PifoExperimentVerifyTest(unittest.TestCase):
    def test_large_checked_config_builds_a_28_command_full_tree_copy(self) -> None:
        config = load_experiment_config(LARGE_CONFIG)
        plan = build_transaction_plan(
            config.initial_tree,
            config.reconfiguration,
            config.simulation.num_vpifos,
        )

        self.assertEqual(len(config.initial_tree.nodes), 7)
        self.assertEqual(config.simulation.num_engines, 4)
        self.assertEqual(len(plan.transaction_commands), 28)
        self.assertEqual(plan.mode, "full_transitive")
        self.assertEqual(plan.transaction_commands[-1].command, "CommitMapper")
        self.assertIsNotNone(config.verification)

    def test_accepts_rr_before_and_during_drain_then_sp_after_drain(self) -> None:
        report = verify_rr_to_sp_phases(
            self._small_threshold_config(),
            self._passing_packets(),
            TransactionTiming(
                mode="full_transitive",
                start_cycle=7,
                commit_cycle=10,
                finish_cycle=40,
                drain_cycle=20,
            ),
        )

        self.assertTrue(report["passed"])
        self.assertEqual(
            [fact["passed"] for fact in report["facts"]],
            [True, True, True, True],
        )
        self.assertEqual(report["packet_counts"]["old_backlog_at_commit"], 2)

    def test_rejects_a_new_tree_packet_before_old_tree_drain(self) -> None:
        packets = self._passing_packets()
        packets[4] = replace(packets[4], completed_cycle=18)
        report = verify_rr_to_sp_phases(
            self._small_threshold_config(),
            packets,
            TransactionTiming(
                mode="full_transitive",
                start_cycle=7,
                commit_cycle=10,
                finish_cycle=40,
                drain_cycle=20,
            ),
        )

        self.assertFalse(report["passed"])
        drain_fact = report["facts"][2]
        checks = {check["id"]: check for check in drain_fact["checks"]}
        self.assertEqual(checks["new_packets_during_drain"]["observed"], 1)
        self.assertFalse(checks["new_packets_during_drain"]["passed"])

    def test_rejects_more_than_one_configuration_acceptance_per_cycle(self) -> None:
        report = verify_rr_to_sp_phases(
            self._small_threshold_config(),
            self._passing_packets(),
            TransactionTiming(
                mode="full_transitive",
                start_cycle=7,
                commit_cycle=10,
                finish_cycle=40,
                drain_cycle=20,
                instruction_count=4,
            ),
        )

        self.assertFalse(report["passed"])
        staging_fact = report["facts"][0]
        checks = {check["id"]: check for check in staging_fact["checks"]}
        self.assertEqual(checks["serialized_configuration_rate"]["expected"], ">= 4")
        self.assertEqual(checks["serialized_configuration_rate"]["observed"], 3)
        self.assertFalse(checks["serialized_configuration_rate"]["passed"])

    def test_admission_on_commit_edge_belongs_to_old_rr_epoch(self) -> None:
        packets = self._passing_packets()
        packets[3] = replace(packets[3], admitted_cycle=10)
        report = verify_rr_to_sp_phases(
            self._small_threshold_config(),
            packets,
            TransactionTiming(
                mode="full_transitive",
                start_cycle=7,
                commit_cycle=10,
                finish_cycle=40,
                drain_cycle=20,
            ),
        )

        self.assertTrue(report["passed"])
        self.assertEqual(
            report["packet_counts"]["admitted_on_commit_edge_as_old"], 1
        )

    def _small_threshold_config(self):
        return replace(
            load_experiment_config(LARGE_CONFIG),
            verification=PhaseVerificationConfig(
                minimum_staging_cycles=2,
                minimum_old_backlog_packets=2,
                minimum_drain_cycles=2,
                minimum_packets_per_phase=2,
            ),
        )

    @staticmethod
    def _passing_packets() -> list[CompletedPacket]:
        return [
            CompletedPacket(1, 1, 0, 1, 4),
            CompletedPacket(2, 2, 0, 2, 6),
            CompletedPacket(3, 1, 0, 3, 12),
            CompletedPacket(4, 2, 0, 4, 14),
            CompletedPacket(5, 1, 0, 11, 22),
            CompletedPacket(6, 1, 0, 12, 24),
            CompletedPacket(7, 2, 0, 13, 26),
            CompletedPacket(8, 2, 0, 14, 28),
        ]


if __name__ == "__main__":
    unittest.main()
