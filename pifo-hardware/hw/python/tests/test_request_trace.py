from __future__ import annotations

import io
import struct
import sys
import tempfile
import unittest
from decimal import Decimal
from pathlib import Path


sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from request_trace import (  # noqa: E402
    FlowResolver,
    Request,
    convert_formal_csv,
    convert_pcap,
    generate_pattern,
    read_trace,
    write_trace,
)


class RequestTraceTest(unittest.TestCase):
    def test_canonical_trace_round_trip_with_comments_and_hex(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "requests.csv"
            path.write_text(
                "# rio-request-trace-v1\n"
                "cycle,request_id,global_flow_id,size_bytes\n"
                "0,1,0x2,64\n"
                "10,2,3,1500\n",
                encoding="utf-8",
            )
            requests = read_trace(path)
            self.assertEqual(
                requests,
                [
                    Request(0, 1, 2, 64),
                    Request(10, 2, 3, 1500),
                ],
            )

            rendered = io.StringIO()
            write_trace(requests, rendered)
            self.assertEqual(
                rendered.getvalue(),
                "cycle,request_id,global_flow_id,size_bytes\n"
                "0,1,2,64\n"
                "10,2,3,1500\n",
            )

    def test_burst_pattern_is_deterministic(self) -> None:
        requests = generate_pattern(
            pattern="burst",
            count=6,
            flows=[1, 2],
            start_cycle=3,
            interval_cycles=2,
            burst_size=3,
            burst_gap_cycles=20,
            size_bytes=64,
            min_size_bytes=None,
            max_size_bytes=None,
            first_request_id=10,
            seed=7,
        )
        self.assertEqual([request.cycle for request in requests], [3, 5, 7, 23, 25, 27])
        self.assertEqual([request.global_flow_id for request in requests], [1, 1, 1, 2, 2, 2])
        self.assertEqual([request.request_id for request in requests], [10, 11, 12, 13, 14, 15])

    def test_flow_resolver_prevents_implicit_id_collisions(self) -> None:
        resolver = FlowResolver()
        self.assertEqual(resolver.resolve("1"), 1)
        self.assertEqual(resolver.resolve("A"), 2)

        with self.assertRaisesRegex(ValueError, "same ID"):
            FlowResolver({"A": 1, "B": 1})

    def test_formal_csv_conversion_uses_arrival_and_mapping(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "formal.csv"
            path.write_text(
                '"flow", "dst", "arrived", "length", "pushed", "popped"\n'
                '"B","0","10.005","1500","10.006","10.020"\n'
                '"A","0","10.000","64","10.001","10.010"\n',
                encoding="utf-8",
            )
            resolver = FlowResolver({"A": 7, "B": 8})
            requests = convert_formal_csv(path, resolver, Decimal(1000))
            self.assertEqual(
                requests,
                [
                    Request(0, 2, 7, 64),
                    Request(5, 1, 8, 1500),
                ],
            )

    def test_classic_pcap_conversion_matches_formal_source_mac_convention(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "packets.pcap"
            ethernet = bytes.fromhex("001122334455") + bytes([0x20]) * 6 + bytes.fromhex("0800")
            payload = ethernet + bytes(50)
            with path.open("wb") as destination:
                destination.write(
                    struct.pack("<IHHIIII", 0xA1B2C3D4, 2, 4, 0, 0, 65535, 1)
                )
                destination.write(struct.pack("<IIII", 2, 500_000, len(payload), len(payload)))
                destination.write(payload)

            requests = convert_pcap(path, FlowResolver({"B": 4}), Decimal(1000))
            self.assertEqual(requests, [Request(0, 1, 4, len(payload))])


if __name__ == "__main__":
    unittest.main()
