import sys
import tempfile
import unittest
from dataclasses import replace
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_transaction_program import (
    ControllerCommand,
    HardwareShape,
    TimedTransaction,
    TransactionProgram,
    load_transaction_program,
    write_transaction_program,
)


class TransactionProgramTest(unittest.TestCase):
    def setUp(self):
        self.write = ControllerCommand("UpdateMapperPre", 1, 1, 0, 2)
        self.commit = ControllerCommand("CommitMapper", 1, 0, 0, 0)
        install = TimedTransaction(
            10, "install", (self.write, self.commit),
            mode="full_transitive", drain_root=(1, 1), gated_flow_ids=(2,),
        )
        cleanup = TimedTransaction(
            10, "cleanup", (ControllerCommand("GuardDrain", 1, 1, 0, 0), self.commit),
            cleanup_of="install",
        )
        self.program = TransactionProgram(
            HardwareShape(2, 8, 256, 4, 2), 1, 1,
            TimedTransaction(None, "initial", (self.commit,)),
            (install, cleanup),
        )

    def test_program_round_trip(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "transactions.txt"
            write_transaction_program(path, self.program)
            self.assertEqual(load_transaction_program(path), self.program)

    def test_package_requires_one_final_commit(self):
        for commands in ((), (self.write,), (self.commit, self.write),
                         (self.commit, self.commit)):
            with self.subTest(commands=commands):
                with self.assertRaisesRegex(ValueError, "exactly one CommitMapper"):
                    TimedTransaction(0, "invalid", commands)

    def test_timeline_rejects_invalid_order_and_cleanup(self):
        install, cleanup = self.program.transactions
        for transactions, message in (
            ((cleanup, install), "earlier"),
            ((install, replace(cleanup, at_cycle=5)), "ordered by cycle"),
            ((install, cleanup, replace(cleanup, name="again")), "not-yet-cleaned"),
        ):
            with self.subTest(message=message):
                with self.assertRaisesRegex(ValueError, message):
                    replace(self.program, transactions=transactions)

    def test_commands_must_fit_the_hardware(self):
        install, cleanup = self.program.transactions
        for field, value in (("engine_id", 3), ("vpifo_id", 8),
                             ("flow_id", 32), ("data", 2**32)):
            with self.subTest(field=field):
                invalid = replace(self.write, **{field: value})
                transaction = replace(install, commands=(invalid, self.commit))
                with self.assertRaisesRegex(ValueError, "out of range"):
                    replace(self.program, transactions=(transaction, cleanup))


if __name__ == "__main__":
    unittest.main()
