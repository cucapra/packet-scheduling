import copy
import json
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from pifo_multiedit_compiler import compile_request, MECHANISMS
from pifo_multiedit_common import RESOURCES
from pifo_transaction_program import load_transaction_program, write_transaction_program
from pifo_traffic_program import generate_traffic, load_traffic_program


class MultiEditTest(unittest.TestCase):
    def setUp(self):
        self.spec = json.loads((RESOURCES / "request.json").read_text())

    def test_all_programs_round_trip(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "commands.txt"
            for mechanism in MECHANISMS:
                with self.subTest(mechanism=mechanism):
                    program, _ = compile_request(self.spec, mechanism)
                    write_transaction_program(path, program)
                    self.assertEqual(program, load_transaction_program(path))

    def test_rio_hoists_five_edits_and_only_retirement_waits(self):
        program, report = compile_request(self.spec, "rio")
        self.assertEqual(len(report["unguarded_edits"]), 5)
        self.assertEqual(len(report["guarded_edits"]), 1)
        first, cleanup = program.transactions
        self.assertFalse(any(c.command in {"StopWorld", "WaitPifoEmpty", "PrefillPifo"} for c in first.commands))
        self.assertEqual(cleanup.commands[0].command, "GuardDrain")
        self.assertEqual(cleanup.cleanup_of, first.name)
        # Untouched work/web nodes and FIFO leaves must not be configured again.
        self.assertFalse(any(c.engine_id == 2 and c.vpifo_id in {3, 5} and c.command == "UpdateBrainEngine"
                             for c in first.commands))
        self.assertFalse(any(c.engine_id == 3 and c.vpifo_id in {5, 6, 7, 8} for c in first.commands))

    def test_p2_control_starts_in_p2_without_a_transition(self):
        p1, _ = compile_request(self.spec, "control")
        p2, report = compile_request(self.spec, "control-p2")
        self.assertEqual(p2.transactions, ())
        self.assertEqual(report["unadmitted_flows"], [3, 4])
        self.assertNotEqual(p1.initial.commands, p2.initial.commands)
        configured_leaves = {(c.engine_id, c.vpifo_id) for c in p2.initial.commands
                             if c.command == "UpdateBrainEngine" and c.data == 3}
        self.assertEqual(configured_leaves, {(3, flow) for flow in range(1, 3)} |
                         {(3, flow) for flow in range(5, 15)})

    def test_copy_moves_all_three_levels_and_preserves_old_token_ids(self):
        program, report = compile_request(self.spec, "relocate")
        commands = program.transactions[0].commands
        self.assertEqual(report["copy_list"], [[3, 6], [2, 5], [1, 4]])
        self.assertEqual([c.data for c in commands if c.command == "CopyPifoEngine"], [6, 5, 4])
        prefill = next(c for c in commands if c.command == "PrefillPifo")
        self.assertEqual((prefill.engine_id, prefill.vpifo_id, prefill.data), (1, 1, 0))
        self.assertTrue(any(c.command == "UpdateMapperPost" and c.engine_id == 4 and c.flow_id == 17
                            and c.data == 81 for c in commands))

    def test_traffic_continues_during_outage_and_legacy_stops(self):
        program = load_traffic_program(RESOURCES / "traffic.json")
        data = generate_traffic(program)
        self.assertEqual(data, generate_traffic(program))
        self.assertFalse(any(r.cycle >= 2000 and r.global_flow_id in {3, 4} for r in data))
        self.assertTrue(any(2000 < r.cycle < 3025 and r.global_flow_id == 1 for r in data))
        self.assertEqual({r.size_bytes for r in data}, {48})

    def test_reject_unsupported_internal_edit_instead_of_ignoring_it(self):
        changed = copy.deepcopy(self.spec)
        changed["after"][0]["policy"] = "RR"
        with self.assertRaisesRegex(ValueError, "inside an existing"):
            compile_request(changed, "rio")


if __name__ == "__main__":
    unittest.main()
