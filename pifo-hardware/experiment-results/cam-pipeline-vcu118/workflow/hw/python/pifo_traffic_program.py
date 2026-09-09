"""Time-varying traffic-pattern input for the simulator CLI."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

from pifo_experiment_config import (
    PACKET_RATE_UNIT,
    TrafficConfig,
    generate_distributed_requests,
    parse_distribution_spec,
    traffic_to_dict,
)
from request_trace import Request


SCHEMA = "pifo-traffic-v1"


@dataclass(frozen=True)
class TrafficPattern:
    name: str
    traffic: TrafficConfig

    def __post_init__(self) -> None:
        if not self.name.strip():
            raise ValueError("traffic pattern name must not be empty")


@dataclass(frozen=True)
class TrafficProgram:
    seed: int
    patterns: tuple[TrafficPattern, ...]

    def __post_init__(self) -> None:
        if not self.patterns:
            raise ValueError("traffic program must contain at least one pattern")
        names = [pattern.name for pattern in self.patterns]
        if len(set(names)) != len(names):
            raise ValueError("traffic pattern names must be unique")


def write_traffic_program(path: Path, program: TrafficProgram) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "schema": SCHEMA,
        "seed": program.seed,
        "patterns": [
            {"name": pattern.name, **traffic_to_dict(pattern.traffic)}
            for pattern in program.patterns
        ],
    }
    path.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def load_traffic_program(path: Path) -> TrafficProgram:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        raise ValueError(f"{path}:{error.lineno}:{error.colno}: {error.msg}") from error
    root = _object(raw, "traffic program")
    _exact_fields(root, {"schema", "seed", "patterns"}, "traffic program")
    if root["schema"] != SCHEMA:
        raise ValueError(f"traffic program.schema must be {SCHEMA!r}")
    patterns_raw = root["patterns"]
    if not isinstance(patterns_raw, list):
        raise ValueError("traffic program.patterns must be an array")
    patterns: list[TrafficPattern] = []
    for index, raw_pattern in enumerate(patterns_raw):
        location = f"traffic program.patterns[{index}]"
        pattern = _object(raw_pattern, location)
        _exact_fields(
            pattern,
            {
                "name",
                "flows",
                "packets_per_flow",
                "start_cycle",
                "packet_rate",
                "packet_size_bytes",
            },
            location,
        )
        flows_raw = pattern["flows"]
        if not isinstance(flows_raw, list):
            raise ValueError(f"{location}.flows must be an array")
        patterns.append(
            TrafficPattern(
                name=_string(pattern["name"], f"{location}.name"),
                traffic=TrafficConfig(
                    flow_ids=tuple(
                        _integer(flow_id, f"{location}.flows[{flow_index}]")
                        for flow_index, flow_id in enumerate(flows_raw)
                    ),
                    packets_per_flow=_integer(
                        pattern["packets_per_flow"],
                        f"{location}.packets_per_flow",
                    ),
                    start_cycle=_integer(
                        pattern["start_cycle"], f"{location}.start_cycle"
                    ),
                    packet_rate=parse_distribution_spec(
                        pattern["packet_rate"],
                        f"{location}.packet_rate",
                        required_unit=PACKET_RATE_UNIT,
                    ),
                    packet_size_bytes=parse_distribution_spec(
                        pattern["packet_size_bytes"],
                        f"{location}.packet_size_bytes",
                    ),
                ),
            )
        )
    return TrafficProgram(
        seed=_integer(root["seed"], "traffic program.seed"),
        patterns=tuple(patterns),
    )


def generate_traffic(program: TrafficProgram) -> list[Request]:
    generated: list[tuple[int, int, int, Request]] = []
    for pattern_index, pattern in enumerate(program.patterns):
        pattern_seed = (
            program.seed
            if pattern_index == 0
            else program.seed ^ ((pattern_index + 1) * 0x9E37_79B9)
        )
        for local_index, request in enumerate(
            generate_distributed_requests(pattern.traffic, pattern_seed)
        ):
            generated.append(
                (request.cycle, pattern_index, local_index, request)
            )
    generated.sort(key=lambda item: item[:3])
    return [
        Request(
            cycle=request.cycle,
            request_id=request_id,
            global_flow_id=request.global_flow_id,
            size_bytes=request.size_bytes,
        )
        for request_id, (_, _, _, request) in enumerate(generated, start=1)
    ]


def _object(value: object, location: str) -> dict[str, object]:
    if not isinstance(value, dict) or not all(
        isinstance(key, str) for key in value
    ):
        raise ValueError(f"{location} must be an object")
    return value


def _exact_fields(
    value: dict[str, object], expected: set[str], location: str
) -> None:
    missing = expected.difference(value)
    unknown = set(value).difference(expected)
    if missing:
        raise ValueError(f"{location} missing fields: {', '.join(sorted(missing))}")
    if unknown:
        raise ValueError(f"{location} unknown fields: {', '.join(sorted(unknown))}")


def _integer(value: object, location: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError(f"{location} must be an integer")
    return value


def _string(value: object, location: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{location} must be a non-empty string")
    return value.strip()
