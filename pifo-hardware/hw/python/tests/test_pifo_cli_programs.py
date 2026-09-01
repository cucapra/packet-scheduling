from __future__ import annotations

import io
import sys
import tempfile
import unittest
from argparse import Namespace
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch


sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from pifo_experiment_config import (  # noqa: E402
    PACKET_RATE_UNIT,
    DistributionSpec,
    TrafficConfig,
    load_experiment_config,
)
from pifo_traffic_program import (  # noqa: E402
    TrafficPattern,
    TrafficProgram,
    generate_traffic,
    load_traffic_program,
    write_traffic_program,
)
from pifo_transaction_program import (  # noqa: E402
    TimedTransaction,
    TransactionProgram,
    load_transaction_program,
    write_transaction_program,
)
from pifo_tree_compiler import (  # noqa: E402
    compile_tree_move,
    load_tree_move_program,
    write_tree_move_program,
)
import pifo_experiment_figures  # noqa: E402


HARDWARE_ROOT = Path(__file__).resolve().parents[3]


class PifoCliProgramsTest(unittest.TestCase):
    def test_experiment_invokes_each_boundary_cli(self) -> None:
        calls: list[tuple[str, tuple[object, ...]]] = []
        with tempfile.TemporaryDirectory() as directory:
            output_dir = Path(directory) / "run"
            with redirect_stdout(io.StringIO()):
                with patch.object(
                    pifo_experiment_figures,
                    "_run",
                    side_effect=lambda name, *args: calls.append((name, args)),
                ):
                    pifo_experiment_figures.run_experiment(
                        Namespace(
                            config=HARDWARE_ROOT / "experiments/rr-to-sp.json",
                            output_dir=output_dir,
                            sbt="sbt",
                        )
                    )

        self.assertEqual(
            [name for name, _ in calls],
            [
                "pifo_tree_compiler.py",
                "pifo_simulator.py",
                "pifo_bandwidth_figure.py",
                "pifo_packet_scatter_figure.py",
            ],
        )
        self.assertIn(output_dir / "figures/bandwidth", calls[2][1])
        self.assertIn("--window-cycles", calls[2][1])
        self.assertIn(320, calls[2][1])
        self.assertIn("--sample-cycles", calls[2][1])
        self.assertIn(8, calls[2][1])
        self.assertIn(output_dir / "figures/packet-scatter", calls[3][1])

    def test_tree_compiler_emits_direct_timed_program(self) -> None:
        config = load_experiment_config(HARDWARE_ROOT / "experiments/rr-to-sp.json")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            tree_move = root / "tree-move.json"
            transactions = root / "transactions.txt"
            write_tree_move_program(tree_move, config)
            compiled = compile_tree_move(load_tree_move_program(tree_move))
            write_transaction_program(transactions, compiled)
            loaded = load_transaction_program(transactions)

        self.assertIsNotNone(loaded.initial)
        assert loaded.initial is not None
        self.assertEqual(len(loaded.initial.commands), 7)
        self.assertEqual(len(loaded.transactions), 1)
        self.assertEqual(loaded.transactions[0].at_cycle, 600)
        self.assertEqual(len(loaded.transactions[0].commands), 10)
        self.assertEqual(loaded.transactions[0].mode, "full_transitive")
        self.assertEqual(loaded.transactions[0].drain_root, (1, 10))

    def test_transaction_program_supports_multiple_timed_packages(self) -> None:
        config = load_experiment_config(HARDWARE_ROOT / "experiments/rr-to-sp.json")
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            tree_move = root / "tree-move.json"
            transactions = root / "transactions.txt"
            write_tree_move_program(tree_move, config)
            base = compile_tree_move(load_tree_move_program(tree_move))
            first = base.transactions[0]
            second = TimedTransaction(
                at_cycle=640,
                name="second-package",
                commands=first.commands,
            )
            program = TransactionProgram(
                hardware=base.hardware,
                root_engine_id=base.root_engine_id,
                root_vpifo_id=base.root_vpifo_id,
                initial=base.initial,
                transactions=(first, second),
            )
            write_transaction_program(transactions, program)
            loaded = load_transaction_program(transactions)

        self.assertEqual(
            [transaction.at_cycle for transaction in loaded.transactions],
            [600, 640],
        )
        self.assertEqual(
            [transaction.name for transaction in loaded.transactions],
            ["policy-change", "second-package"],
        )

    def test_traffic_program_merges_patterns_in_time_order(self) -> None:
        rate = DistributionSpec(
            distribution="constant",
            value=1.0,
            unit=PACKET_RATE_UNIT,
        )
        size = DistributionSpec(distribution="constant", value=64.0)
        program = TrafficProgram(
            seed=9,
            patterns=(
                TrafficPattern(
                    "first",
                    TrafficConfig((1,), 2, 0, rate, size),
                ),
                TrafficPattern(
                    "second",
                    TrafficConfig((2,), 2, 0, rate, size),
                ),
            ),
        )
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "traffic.json"
            write_traffic_program(path, program)
            loaded = load_traffic_program(path)
        requests = generate_traffic(loaded)

        self.assertEqual([request.request_id for request in requests], [1, 2, 3, 4])
        self.assertEqual([request.cycle for request in requests], [0, 0, 1, 1])
        self.assertEqual(
            [request.global_flow_id for request in requests], [1, 2, 1, 2]
        )


if __name__ == "__main__":
    unittest.main()
