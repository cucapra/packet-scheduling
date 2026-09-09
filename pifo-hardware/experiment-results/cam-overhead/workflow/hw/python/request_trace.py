#!/usr/bin/env python3
"""Shared request-trace parsing, generation, and formal-trace conversion helpers."""

from __future__ import annotations

import csv
import json
import math
import random
import struct
from dataclasses import dataclass
from decimal import Decimal, ROUND_HALF_UP
from pathlib import Path
from typing import Iterable, Iterator, Mapping, Sequence, TextIO


TRACE_HEADER = ("cycle", "request_id", "global_flow_id", "size_bytes")


@dataclass(frozen=True, order=True)
class Request:
    cycle: int
    request_id: int
    global_flow_id: int
    size_bytes: int

    def __post_init__(self) -> None:
        self.validate()

    def validate(self) -> None:
        if self.cycle < 0:
            raise ValueError(f"request cycle must be non-negative, got {self.cycle}")
        if self.request_id < 0:
            raise ValueError(f"request_id must be non-negative, got {self.request_id}")
        if self.global_flow_id < 0:
            raise ValueError(
                f"global_flow_id must be non-negative, got {self.global_flow_id}"
            )
        if self.size_bytes <= 0:
            raise ValueError(f"size_bytes must be positive, got {self.size_bytes}")


def parse_int(value: str) -> int:
    try:
        return int(value.strip(), 0)
    except ValueError as error:
        raise ValueError(f"invalid integer {value!r}") from error


def _meaningful_lines(source: TextIO) -> Iterator[str]:
    for raw in source:
        line = raw.strip()
        if line and not line.startswith("#"):
            yield raw


def read_trace(path: Path) -> list[Request]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(_meaningful_lines(source), skipinitialspace=True)
        actual_header = tuple(reader.fieldnames or ())
        if actual_header != TRACE_HEADER:
            raise ValueError(
                f"{path}: expected header {','.join(TRACE_HEADER)!r}, "
                f"got {','.join(actual_header)!r}"
            )

        requests: list[Request] = []
        seen_ids: set[int] = set()
        previous_cycle = -1
        for line_number, row in enumerate(reader, start=2):
            try:
                request = Request(
                    cycle=parse_int(row["cycle"]),
                    request_id=parse_int(row["request_id"]),
                    global_flow_id=parse_int(row["global_flow_id"]),
                    size_bytes=parse_int(row["size_bytes"]),
                )
                request.validate()
                if request.request_id in seen_ids:
                    raise ValueError(f"duplicate request_id {request.request_id}")
                if request.cycle < previous_cycle:
                    raise ValueError(
                        f"cycles must be nondecreasing ({previous_cycle} followed by "
                        f"{request.cycle})"
                    )
            except (KeyError, TypeError, ValueError) as error:
                raise ValueError(f"{path}:{line_number}: {error}") from error

            requests.append(request)
            seen_ids.add(request.request_id)
            previous_cycle = request.cycle
        return requests


def write_trace(requests: Iterable[Request], destination: TextIO) -> None:
    writer = csv.writer(destination, lineterminator="\n")
    writer.writerow(TRACE_HEADER)
    seen_ids: set[int] = set()
    previous_cycle = -1
    for request in requests:
        request.validate()
        if request.request_id in seen_ids:
            raise ValueError(f"duplicate request_id {request.request_id}")
        if request.cycle < previous_cycle:
            raise ValueError("requests must be sorted by nondecreasing cycle")
        writer.writerow(
            (
                request.cycle,
                request.request_id,
                request.global_flow_id,
                request.size_bytes,
            )
        )
        seen_ids.add(request.request_id)
        previous_cycle = request.cycle


def load_flow_mapping(path: Path | None) -> dict[str, int]:
    if path is None:
        return {}
    with path.open(encoding="utf-8") as source:
        value = json.load(source)
    if isinstance(value, dict) and isinstance(value.get("flows"), dict):
        value = value["flows"]
    if not isinstance(value, dict):
        raise ValueError(f"{path}: flow mapping must be an object or contain a flows object")
    result = {str(name): int(flow_id) for name, flow_id in value.items()}
    if any(flow_id < 0 for flow_id in result.values()):
        raise ValueError(f"{path}: flow IDs must be non-negative")
    return result


class FlowResolver:
    def __init__(self, configured: Mapping[str, int] | None = None):
        self._mapping = {
            str(name): int(flow_id) for name, flow_id in (configured or {}).items()
        }
        if any(flow_id < 0 for flow_id in self._mapping.values()):
            raise ValueError("flow IDs must be non-negative")
        if len(set(self._mapping.values())) != len(self._mapping):
            raise ValueError("flow mapping assigns the same ID to multiple names")
        self._used_ids = set(self._mapping.values())

    @property
    def mapping(self) -> dict[str, int]:
        return dict(self._mapping)

    def resolve(self, name: str) -> int:
        normalized = name.strip()
        if not normalized:
            raise ValueError("flow name must not be empty")
        try:
            numeric = int(normalized, 0)
        except ValueError:
            numeric = None
        if numeric is not None:
            if numeric < 0:
                raise ValueError(f"flow ID must be non-negative, got {numeric}")
            self._used_ids.add(numeric)
            return numeric

        if normalized in self._mapping:
            return self._mapping[normalized]

        # Preserve the formal simulator's conventional A -> 1, B -> 2, ... mapping
        # where it does not collide with an explicit mapping.
        if len(normalized) == 1 and normalized.isalpha():
            candidate = ord(normalized.upper()) - ord("A") + 1
            if candidate > 0 and candidate not in self._used_ids:
                self._mapping[normalized] = candidate
                self._used_ids.add(candidate)
                return candidate

        candidate = 1
        while candidate in self._used_ids:
            candidate += 1
        self._mapping[normalized] = candidate
        self._used_ids.add(candidate)
        return candidate


def _normalized_csv_rows(path: Path) -> Iterator[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(_meaningful_lines(source), skipinitialspace=True)
        if not reader.fieldnames:
            raise ValueError(f"{path}: missing CSV header")
        normalized_header = [field.strip().strip('"').lower() for field in reader.fieldnames]
        for row in reader:
            yield {
                normalized_header[index]: value.strip()
                for index, value in enumerate(row.values())
                if index < len(normalized_header) and value is not None
            }


def convert_formal_csv(
    path: Path,
    resolver: FlowResolver,
    cycles_per_second: Decimal,
    time_field: str = "arrived",
) -> list[Request]:
    if not cycles_per_second.is_finite() or cycles_per_second <= 0:
        raise ValueError("cycles_per_second must be positive")
    rows = list(_normalized_csv_rows(path))
    if not rows:
        return []
    required = {"flow", "length", time_field}
    missing = required.difference(rows[0])
    if missing:
        raise ValueError(f"{path}: formal CSV is missing fields: {', '.join(sorted(missing))}")

    origin = min(Decimal(row[time_field]) for row in rows)
    requests: list[Request] = []
    for request_id, row in enumerate(rows, start=1):
        relative_time = Decimal(row[time_field]) - origin
        cycle = int((relative_time * cycles_per_second).to_integral_value(ROUND_HALF_UP))
        requests.append(
            Request(
                cycle=cycle,
                request_id=request_id,
                global_flow_id=resolver.resolve(row["flow"]),
                size_bytes=parse_int(row["length"]),
            )
        )
    return sorted(requests, key=lambda request: (request.cycle, request.request_id))


@dataclass(frozen=True)
class PcapRecord:
    timestamp: Decimal
    included_length: int
    source_mac: bytes


def read_classic_pcap(path: Path) -> list[PcapRecord]:
    magic_formats = {
        b"\xd4\xc3\xb2\xa1": ("<", Decimal(1_000_000)),
        b"\xa1\xb2\xc3\xd4": (">", Decimal(1_000_000)),
        b"\x4d\x3c\xb2\xa1": ("<", Decimal(1_000_000_000)),
        b"\xa1\xb2\x3c\x4d": (">", Decimal(1_000_000_000)),
    }
    with path.open("rb") as source:
        global_header = source.read(24)
        if len(global_header) != 24 or global_header[:4] not in magic_formats:
            raise ValueError(f"{path}: unsupported or malformed classic PCAP header")
        endian, fractional_scale = magic_formats[global_header[:4]]
        link_type = struct.unpack(f"{endian}I", global_header[20:24])[0]
        if link_type != 1:
            raise ValueError(f"{path}: only Ethernet PCAP (link type 1) is supported")

        records: list[PcapRecord] = []
        record_number = 0
        while True:
            record_header = source.read(16)
            if not record_header:
                break
            record_number += 1
            if len(record_header) != 16:
                raise ValueError(f"{path}: truncated record header {record_number}")
            seconds, fraction, included_length, _original_length = struct.unpack(
                f"{endian}IIII", record_header
            )
            payload = source.read(included_length)
            if len(payload) != included_length:
                raise ValueError(f"{path}: truncated packet {record_number}")
            if len(payload) < 14:
                raise ValueError(f"{path}: packet {record_number} is shorter than Ethernet")
            records.append(
                PcapRecord(
                    timestamp=Decimal(seconds) + (Decimal(fraction) / fractional_scale),
                    included_length=included_length,
                    source_mac=payload[6:12],
                )
            )
        return records


FORMAL_SOURCE_MAC_TO_FLOW = {
    bytes([byte]) * 6: chr(ord("A") + index)
    for index, byte in enumerate(range(0x10, 0x80, 0x10))
}


def convert_pcap(
    path: Path, resolver: FlowResolver, cycles_per_second: Decimal
) -> list[Request]:
    if not cycles_per_second.is_finite() or cycles_per_second <= 0:
        raise ValueError("cycles_per_second must be positive")
    records = read_classic_pcap(path)
    if not records:
        return []
    origin = min(record.timestamp for record in records)
    requests: list[Request] = []
    for request_id, record in enumerate(records, start=1):
        flow_name = FORMAL_SOURCE_MAC_TO_FLOW.get(
            record.source_mac, record.source_mac.hex(":")
        )
        cycle = int(
            ((record.timestamp - origin) * cycles_per_second).to_integral_value(
                ROUND_HALF_UP
            )
        )
        requests.append(
            Request(
                cycle=cycle,
                request_id=request_id,
                global_flow_id=resolver.resolve(flow_name),
                size_bytes=record.included_length,
            )
        )
    return sorted(requests, key=lambda request: (request.cycle, request.request_id))


def generate_pattern(
    *,
    pattern: str,
    count: int,
    flows: Sequence[int],
    start_cycle: int,
    interval_cycles: float,
    burst_size: int,
    burst_gap_cycles: int,
    size_bytes: int,
    min_size_bytes: int | None,
    max_size_bytes: int | None,
    first_request_id: int,
    seed: int,
) -> list[Request]:
    if count < 0:
        raise ValueError("count must be non-negative")
    if not flows or any(flow < 0 for flow in flows):
        raise ValueError("at least one non-negative flow ID is required")
    if (
        start_cycle < 0 or not math.isfinite(interval_cycles) or interval_cycles <= 0
    ):
        raise ValueError("start_cycle must be non-negative and interval_cycles positive")
    if burst_size <= 0 or burst_gap_cycles <= 0:
        raise ValueError("burst_size and burst_gap_cycles must be positive")
    if size_bytes <= 0:
        raise ValueError("size_bytes must be positive")
    if (min_size_bytes is None) != (max_size_bytes is None):
        raise ValueError("min_size_bytes and max_size_bytes must be supplied together")
    if min_size_bytes is not None and not (0 < min_size_bytes <= max_size_bytes):
        raise ValueError("size range must satisfy 0 < min <= max")

    rng = random.Random(seed)
    requests: list[Request] = []
    poisson_cycle = float(start_cycle)

    def choose_size() -> int:
        if min_size_bytes is None:
            return size_bytes
        return rng.randint(min_size_bytes, max_size_bytes)

    for index in range(count):
        if pattern == "round-robin":
            flow = flows[index % len(flows)]
            cycle = start_cycle + round(index * interval_cycles)
        elif pattern == "uniform":
            flow = rng.choice(flows)
            cycle = start_cycle + round(index * interval_cycles)
        elif pattern == "poisson":
            flow = rng.choice(flows)
            if index:
                poisson_cycle += rng.expovariate(1.0 / interval_cycles)
            cycle = round(poisson_cycle)
        elif pattern == "burst":
            burst_index, within_burst = divmod(index, burst_size)
            flow = flows[burst_index % len(flows)]
            cycle = (
                start_cycle
                + burst_index * burst_gap_cycles
                + round(within_burst * interval_cycles)
            )
        else:
            raise ValueError(f"unknown pattern {pattern!r}")

        requests.append(
            Request(
                cycle=cycle,
                request_id=first_request_id + index,
                global_flow_id=flow,
                size_bytes=choose_size(),
            )
        )
    return sorted(requests, key=lambda request: (request.cycle, request.request_id))
