import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_config import (
    InitialTreeConfig,
    NodePolicyChangeConfig,
    PolicyChangeConfig,
    TreeNodeConfig,
)
from pifo_transaction_program import ControllerCommand, HardwareShape
from pifo_tree_compiler import TreeMoveProgram, compile_tree_move


class TreeCompilerTest(unittest.TestCase):
    def setUp(self):
        tree = InitialTreeConfig(
            root="root",
            nodes={
                "root": TreeNodeConfig(1, 1, "RR", {}),
                "a": TreeNodeConfig(2, 1, "FIFO", {}),
                "b": TreeNodeConfig(2, 2, "FIFO", {}),
            },
            flow_paths={1: ("root", "a"), 2: ("root", "b")},
        )
        move = PolicyChangeConfig(
            cycle=10, name="change", before_label="RR", after_label="SP",
            changes={"root": NodePolicyChangeConfig("SP", {1: 2, 2: 1})},
        )
        self.program = compile_tree_move(
            TreeMoveProgram(HardwareShape(2, 8, 256, 4, 2), tree, move)
        )

    def test_initial_tree_routes_through_fifo_leaves(self):
        commands = self.program.initial.commands
        # Packed IDs use three bits for the vPIFO/flow: engine 2, leaf 1 = 17.
        self.assertIn(ControllerCommand("UpdateMapperPost", 1, 1, 9, 17), commands)
        self.assertIn(ControllerCommand("UpdateMapperPost", 1, 1, 10, 18), commands)
        self.assertIn(ControllerCommand("UpdateMapperPost", 2, 1, 17, 1), commands)
        self.assertIn(ControllerCommand("UpdateMapperPost", 2, 2, 18, 2), commands)
        self.assertEqual(commands[-1].command, "CommitMapper")

    def test_move_copies_nodes_and_rewrites_the_old_root(self):
        install = self.program.transactions[0]
        self.assertEqual((install.at_cycle, install.drain_root), (10, (1, 1)))
        self.assertEqual(
            {(c.engine_id, c.vpifo_id, c.data) for c in install.commands
             if c.command == "UpdateBrainEngine"},
            {(1, 2, 2), (2, 3, 3), (2, 4, 3)},
        )
        self.assertIn(ControllerCommand("UpdateBrainFlowState", 1, 2, 9, 2), install.commands)
        self.assertIn(ControllerCommand("UpdateBrainFlowState", 1, 2, 10, 1), install.commands)
        self.assertIn(ControllerCommand("UpdateMapperNonExist", 1, 1, 0, 2), install.commands)
        self.assertEqual(install.commands[-1].command, "CommitMapper")

    def test_cleanup_guards_retired_nodes_before_clearing_them(self):
        install, cleanup = self.program.transactions
        self.assertEqual(cleanup.cleanup_of, install.name)
        self.assertEqual(cleanup.commands[:3], (
            ControllerCommand("GuardDrain", 1, 1, 0, 0),
            ControllerCommand("GuardDrain", 2, 1, 0, 0),
            ControllerCommand("GuardDrain", 2, 2, 0, 0),
        ))
        cleared = cleanup.commands[3:-1]
        self.assertTrue(cleared)
        self.assertTrue(all(c.data == 0 for c in cleared))
        self.assertEqual(
            {(c.engine_id, c.vpifo_id) for c in cleared
             if c.command == "UpdateBrainEngine"},
            {(1, 1), (2, 1), (2, 2)},
        )
        self.assertNotIn("UpdateMapperNonExist", [c.command for c in cleared])
        self.assertEqual(cleanup.commands[-1].command, "CommitMapper")

    def test_vpifo_zero_remains_the_null_sink(self):
        with self.assertRaisesRegex(ValueError, "null sink"):
            TreeNodeConfig(1, 0, "FIFO", {})


if __name__ == "__main__":
    unittest.main()
