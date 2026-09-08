"""Typed JSON configuration and traffic generation for PIFO experiments."""

from __future__ import annotations

import json
import math
import random
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping

from request_trace import Request


PACKET_RATE_UNIT = "packets_per_cycle_per_flow"
SUPPORTED_POLICIES = {"RR", "WFQ", "SP", "FIFO"}
SUPPORTED_RECONFIGURATION_MODES = {
    "in_place",
    "stop_the_world",
    "full_transitive",
    "confined_transitive",
}


@dataclass(frozen=True)
class DistributionSpec:
    distribution: str
    value: float | None = None
    minimum: float | None = None
    maximum: float | None = None
    mean: float | None = None
    stddev: float | None = None
    unit: str | None = None

    def __post_init__(self) -> None:
        if self.distribution not in {"constant", "uniform", "normal"}:
            raise ValueError(
                "distribution must be one of constant, uniform, or normal"
            )
        values = (self.value, self.minimum, self.maximum, self.mean, self.stddev)
        if any(item is not None and not math.isfinite(item) for item in values):
            raise ValueError("distribution parameters must be finite")
        if self.distribution == "constant":
            if self.value is None or self.value <= 0:
                raise ValueError("constant distribution value must be positive")
        elif self.distribution == "uniform":
            if self.minimum is None or self.maximum is None:
                raise ValueError("uniform distribution requires min and max")
            if self.minimum <= 0 or self.maximum <= self.minimum:
                raise ValueError("uniform distribution requires 0 < min < max")
        else:
            if None in (self.mean, self.stddev, self.minimum, self.maximum):
                raise ValueError(
                    "normal distribution requires mean, stddev, min, and max"
                )
            assert self.mean is not None
            assert self.stddev is not None
            assert self.minimum is not None
            assert self.maximum is not None
            if self.minimum <= 0 or self.maximum <= self.minimum:
                raise ValueError("normal distribution requires 0 < min < max")
            if self.stddev <= 0:
                raise ValueError("normal distribution stddev must be positive")
            if not self.minimum <= self.mean <= self.maximum:
                raise ValueError("normal distribution mean must be within [min, max]")

    def sample(self, rng: random.Random) -> float:
        if self.distribution == "constant":
            assert self.value is not None
            return self.value
        if self.distribution == "uniform":
            assert self.minimum is not None and self.maximum is not None
            return rng.uniform(self.minimum, self.maximum)
        assert self.mean is not None and self.stddev is not None
        assert self.minimum is not None and self.maximum is not None
        return min(
            self.maximum,
            max(self.minimum, rng.gauss(self.mean, self.stddev)),
        )

    def to_dict(self) -> dict[str, object]:
        result: dict[str, object] = {"distribution": self.distribution}
        if self.unit is not None:
            result["unit"] = self.unit
        if self.distribution == "constant":
            result["value"] = self.value
        elif self.distribution == "uniform":
            result["min"] = self.minimum
            result["max"] = self.maximum
        else:
            result["mean"] = self.mean
            result["stddev"] = self.stddev
            result["min"] = self.minimum
            result["max"] = self.maximum
        return result


@dataclass(frozen=True)
class TrafficConfig:
    flow_ids: tuple[int, ...]
    packets_per_flow: int
    start_cycle: int
    packet_rate: DistributionSpec
    packet_size_bytes: DistributionSpec

    def __post_init__(self) -> None:
        if not self.flow_ids:
            raise ValueError("traffic.flows must contain at least one flow ID")
        if len(set(self.flow_ids)) != len(self.flow_ids):
            raise ValueError("traffic.flows must not contain duplicates")
        if any(flow_id < 0 for flow_id in self.flow_ids):
            raise ValueError("traffic flow IDs must be non-negative")
        if self.packets_per_flow <= 0:
            raise ValueError("traffic.packets_per_flow must be positive")
        if self.start_cycle < 0:
            raise ValueError("traffic.start_cycle must be non-negative")
        if self.packet_rate.unit != PACKET_RATE_UNIT:
            raise ValueError(
                f"traffic.packet_rate.unit must be {PACKET_RATE_UNIT!r}"
            )


@dataclass(frozen=True)
class TreeNodeConfig:
    engine_id: int
    vpifo_id: int
    policy: str
    flow_state: Mapping[int, int]

    def __post_init__(self) -> None:
        if self.engine_id <= 0:
            raise ValueError("tree node engine_id must be positive")
        if self.vpifo_id <= 0:
            raise ValueError(
                "tree node vpifo_id must be positive; vPIFO 0 is the null sink"
            )
        if self.policy not in SUPPORTED_POLICIES:
            raise ValueError(f"unsupported tree node policy {self.policy!r}")
        if any(flow_id < 0 or state < 0 for flow_id, state in self.flow_state.items()):
            raise ValueError("tree node flow_state IDs and values must be non-negative")


@dataclass(frozen=True)
class InitialTreeConfig:
    root: str
    nodes: Mapping[str, TreeNodeConfig]
    flow_paths: Mapping[int, tuple[str, ...]]

    def __post_init__(self) -> None:
        if not self.root:
            raise ValueError("initial_tree.root must not be empty")
        if self.root not in self.nodes:
            raise ValueError("initial_tree.root must name a configured node")
        if not self.nodes:
            raise ValueError("initial_tree.nodes must not be empty")
        for flow_id, path in self.flow_paths.items():
            if flow_id < 0:
                raise ValueError("initial_tree.flow_paths keys must be non-negative")
            if not path or path[0] != self.root:
                raise ValueError(
                    f"initial_tree.flow_paths.{flow_id} must start at root {self.root!r}"
                )
            unknown = [node_name for node_name in path if node_name not in self.nodes]
            if unknown:
                raise ValueError(
                    f"initial_tree.flow_paths.{flow_id} contains unknown nodes: "
                    + ", ".join(unknown)
                )
            engines = [self.nodes[node_name].engine_id for node_name in path]
            if len(set(engines)) != len(engines):
                raise ValueError(
                    f"initial_tree.flow_paths.{flow_id} uses more than one node "
                    "on the same engine"
                )


@dataclass(frozen=True)
class NodePolicyChangeConfig:
    policy: str
    flow_state: Mapping[int, int]

    def __post_init__(self) -> None:
        if self.policy not in SUPPORTED_POLICIES:
            raise ValueError(f"unsupported changed policy {self.policy!r}")
        if any(flow_id < 0 or state < 0 for flow_id, state in self.flow_state.items()):
            raise ValueError("changed flow_state IDs and values must be non-negative")


@dataclass(frozen=True)
class PolicyChangeConfig:
    cycle: int
    name: str
    before_label: str
    after_label: str
    changes: Mapping[str, NodePolicyChangeConfig]
    mode: str = "full_transitive"
    target_tree: InitialTreeConfig | None = None
    minimum_stop_cycles: int = 0

    def __post_init__(self) -> None:
        if self.cycle < 0:
            raise ValueError("reconfiguration.cycle must be non-negative")
        if not self.name:
            raise ValueError("reconfiguration.name must not be empty")
        if not self.before_label or not self.after_label:
            raise ValueError("policy-change labels must not be empty")
        if not self.changes and self.target_tree is None:
            raise ValueError("policy change requires changes or target_tree")
        if self.mode not in SUPPORTED_RECONFIGURATION_MODES:
            raise ValueError(
                "policy-change mode must be in_place, stop_the_world, "
                "full_transitive, or confined_transitive"
            )
        if self.minimum_stop_cycles < 0:
            raise ValueError("minimum_stop_cycles must be non-negative")
        if self.mode != "stop_the_world" and self.minimum_stop_cycles:
            raise ValueError(
                "minimum_stop_cycles is only valid for stop_the_world"
            )


def validate_tree_move(
    tree: InitialTreeConfig,
    change: PolicyChangeConfig,
    num_engines: int,
    num_vpifos: int,
    max_packet_priority: int,
) -> None:
    _validate_tree_shape(
        tree,
        "old tree",
        num_engines,
        num_vpifos,
        max_packet_priority,
    )
    if change.target_tree is not None:
        _validate_tree_shape(
            change.target_tree,
            "target tree",
            num_engines,
            num_vpifos,
            max_packet_priority,
        )
        removed_flows = set(tree.flow_paths).difference(change.target_tree.flow_paths)
        if removed_flows:
            raise ValueError(
                "target tree removes flows: "
                + ",".join(map(str, sorted(removed_flows)))
            )
        if change.mode == "stop_the_world":
            old_root = tree.nodes[tree.root]
            target_root = change.target_tree.nodes[change.target_tree.root]
            if (old_root.engine_id, old_root.vpifo_id) != (
                target_root.engine_id,
                target_root.vpifo_id,
            ):
                raise ValueError(
                    "stop_the_world must reuse the old physical root"
                )
    elif change.mode != "full_transitive":
        raise ValueError(f"{change.mode} requires an explicit target_tree")

    unknown_changes = set(change.changes).difference(tree.nodes)
    if unknown_changes:
        raise ValueError(
            "move.changes contains unknown nodes: "
            + ",".join(sorted(unknown_changes))
        )
    for name, node_change in change.changes.items():
        merged_state = dict(tree.nodes[name].flow_state)
        merged_state.update(node_change.flow_state)
        _validate_node_state(
            tree, name, node_change.policy, merged_state, max_packet_priority
        )


def _validate_tree_shape(
    tree: InitialTreeConfig,
    label: str,
    num_engines: int,
    num_vpifos: int,
    max_packet_priority: int,
) -> None:
    flow_ids = set(tree.flow_paths)
    if any(flow_id >= num_vpifos - 1 for flow_id in flow_ids):
        raise ValueError(
            f"{label} flow IDs must be below num_vpifos - 1; "
            "the highest ID is reserved for empty-PIFO output"
        )
    physical_nodes: set[tuple[int, int]] = set()
    for name, node in tree.nodes.items():
        if node.engine_id > num_engines:
            raise ValueError(f"{label}.nodes.{name}.engine_id is out of range")
        if node.vpifo_id >= num_vpifos - 1:
            raise ValueError(
                f"{label}.nodes.{name}.vpifo_id must be below num_vpifos - 1"
            )
        physical = (node.engine_id, node.vpifo_id)
        if physical in physical_nodes:
            raise ValueError(
                f"{label} has duplicate physical node "
                f"{node.engine_id}:{node.vpifo_id}"
            )
        physical_nodes.add(physical)
        _validate_node_state(tree, name, node.policy, node.flow_state, max_packet_priority)



def _validate_node_state(
    tree: InitialTreeConfig,
    node_name: str,
    policy: str,
    flow_state: Mapping[int, int],
    max_packet_priority: int,
) -> None:
    node_flows = {
        flow_id for flow_id, path in tree.flow_paths.items() if node_name in path
    }
    unknown = set(flow_state).difference(node_flows)
    if unknown:
        raise ValueError(
            f"flow_state for node {node_name!r} contains flows not on that node: "
            + ",".join(map(str, sorted(unknown)))
        )
    if any(state >= 2**32 for state in flow_state.values()):
        raise ValueError("flow-state values must fit in 32 bits")
    if policy == "SP":
        missing = node_flows.difference(flow_state)
        if missing:
            raise ValueError(
                f"SP node {node_name!r} is missing flow_state for flows: "
                + ",".join(map(str, sorted(missing)))
            )
        if any(
            flow_state[flow_id] <= 0
            or flow_state[flow_id] >= max_packet_priority
            for flow_id in node_flows
        ):
            raise ValueError(
                f"SP node {node_name!r} priorities must be in "
                f"[1, {max_packet_priority - 1}]"
            )


@dataclass(frozen=True)
class SimulationConfig:
    link_bytes_per_cycle: float = 64.0
    queue_depth: int = 256
    max_cycles: int = 100_000
    max_packet_priority: int = 65_536
    num_engines: int = 2
    num_vpifos: int = 32
    fifo_depth: int = 32
    prefetch_buffer_depth: int = 2

    def __post_init__(self) -> None:
        if (
            not math.isfinite(self.link_bytes_per_cycle)
            or self.link_bytes_per_cycle <= 0
        ):
            raise ValueError("simulation.link_bytes_per_cycle must be positive")
        if self.queue_depth <= 0:
            raise ValueError("simulation.queue_depth must be positive")
        if self.max_cycles <= 0:
            raise ValueError("simulation.max_cycles must be positive")
        if self.max_packet_priority <= 1:
            raise ValueError("simulation.max_packet_priority must be greater than one")
        if self.num_engines <= 0:
            raise ValueError("simulation.num_engines must be positive")
        if self.num_vpifos < 3:
            raise ValueError("simulation.num_vpifos must be at least 3")
        if self.fifo_depth <= 0:
            raise ValueError("simulation.fifo_depth must be positive")
        if self.prefetch_buffer_depth <= 0:
            raise ValueError("simulation.prefetch_buffer_depth must be positive")
        capacity = self.num_vpifos * self.fifo_depth
        if capacity & (capacity - 1):
            raise ValueError(
                "simulation.num_vpifos * simulation.fifo_depth must be a power of two"
            )


@dataclass(frozen=True)
class PlotConfig:
    bandwidth_window_cycles: int = 256
    bandwidth_sample_cycles: int = 8
    flow_labels: Mapping[int, str] | None = None
    dpi: int = 180

    def __post_init__(self) -> None:
        if self.bandwidth_window_cycles < 3:
            raise ValueError("plot.bandwidth_window_cycles must be at least 3")
        if self.bandwidth_sample_cycles <= 0:
            raise ValueError("plot.bandwidth_sample_cycles must be positive")
        if self.bandwidth_sample_cycles > self.bandwidth_window_cycles:
            raise ValueError(
                "plot.bandwidth_sample_cycles cannot exceed "
                "plot.bandwidth_window_cycles"
            )
        if self.dpi <= 0:
            raise ValueError("plot.dpi must be positive")


@dataclass(frozen=True)
class PhaseVerificationConfig:
    minimum_staging_cycles: int = 1
    minimum_old_backlog_packets: int = 1
    minimum_drain_cycles: int = 1
    minimum_packets_per_phase: int = 1

    def __post_init__(self) -> None:
        values = {
            "minimum_staging_cycles": self.minimum_staging_cycles,
            "minimum_old_backlog_packets": self.minimum_old_backlog_packets,
            "minimum_drain_cycles": self.minimum_drain_cycles,
            "minimum_packets_per_phase": self.minimum_packets_per_phase,
        }
        for name, value in values.items():
            if value <= 0:
                raise ValueError(f"verification.{name} must be positive")


@dataclass(frozen=True)
class ExperimentConfig:
    output_dir: Path
    seed: int
    traffic: TrafficConfig
    initial_tree: InitialTreeConfig
    reconfiguration: PolicyChangeConfig
    simulation: SimulationConfig
    plot: PlotConfig
    verification: PhaseVerificationConfig | None = None

    def __post_init__(self) -> None:
        if self.reconfiguration.cycle >= self.simulation.max_cycles:
            raise ValueError(
                "reconfiguration.cycle must be less than simulation.max_cycles"
            )
        traffic_flows = set(self.traffic.flow_ids)
        path_flows = set(self.initial_tree.flow_paths)
        if path_flows != traffic_flows:
            missing = sorted(traffic_flows - path_flows)
            extra = sorted(path_flows - traffic_flows)
            details = []
            if missing:
                details.append("missing " + ",".join(map(str, missing)))
            if extra:
                details.append("unknown " + ",".join(map(str, extra)))
            raise ValueError("initial_tree.flow_paths: " + "; ".join(details))
        validate_tree_move(
            self.initial_tree,
            self.reconfiguration,
            self.simulation.num_engines,
            self.simulation.num_vpifos,
            self.simulation.max_packet_priority,
        )
        if self.verification is not None:
            root_name = self.initial_tree.root
            old_root = self.initial_tree.nodes[root_name]
            new_root = self.reconfiguration.changes.get(root_name)
            if old_root.policy != "RR" or new_root is None or new_root.policy != "SP":
                raise ValueError(
                    "phase verification currently requires a root RR-to-SP change"
                )
        labels = self.plot.flow_labels or {}
        unknown_labels = set(labels).difference(traffic_flows)
        if unknown_labels:
            raise ValueError(
                "plot.flow_labels contains unknown flows: "
                + ",".join(map(str, sorted(unknown_labels)))
            )

    def to_dict(self) -> dict[str, object]:
        labels = self.plot.flow_labels or {}
        result: dict[str, object] = {
            "output_dir": str(self.output_dir),
            "seed": self.seed,
            "traffic": traffic_to_dict(self.traffic),
            "initial_tree": tree_to_dict(self.initial_tree),
            "reconfiguration": reconfiguration_to_dict(self.reconfiguration),
            "simulation": {
                "link_bytes_per_cycle": self.simulation.link_bytes_per_cycle,
                "queue_depth": self.simulation.queue_depth,
                "max_cycles": self.simulation.max_cycles,
                "max_packet_priority": self.simulation.max_packet_priority,
                "num_engines": self.simulation.num_engines,
                "num_vpifos": self.simulation.num_vpifos,
                "fifo_depth": self.simulation.fifo_depth,
                "prefetch_buffer_depth": self.simulation.prefetch_buffer_depth,
            },
            "plot": {
                "bandwidth_window_cycles": self.plot.bandwidth_window_cycles,
                "bandwidth_sample_cycles": self.plot.bandwidth_sample_cycles,
                "flow_labels": {
                    str(flow_id): label
                    for flow_id, label in sorted(labels.items())
                },
                "dpi": self.plot.dpi,
            },
        }
        if self.verification is not None:
            result["verification"] = {
                "minimum_staging_cycles": self.verification.minimum_staging_cycles,
                "minimum_old_backlog_packets": (
                    self.verification.minimum_old_backlog_packets
                ),
                "minimum_drain_cycles": self.verification.minimum_drain_cycles,
                "minimum_packets_per_phase": (
                    self.verification.minimum_packets_per_phase
                ),
            }
        return result


def tree_to_dict(tree: InitialTreeConfig) -> dict[str, object]:
    return {
        "root": tree.root,
        "nodes": {
            name: {
                "engine_id": node.engine_id,
                "vpifo_id": node.vpifo_id,
                "policy": node.policy,
                "flow_state": {
                    str(flow_id): state
                    for flow_id, state in sorted(node.flow_state.items())
                },
            }
            for name, node in tree.nodes.items()
        },
        "flow_paths": {
            str(flow_id): list(path)
            for flow_id, path in sorted(tree.flow_paths.items())
        },
    }


def traffic_to_dict(traffic: TrafficConfig) -> dict[str, object]:
    return {
        "flows": list(traffic.flow_ids),
        "packets_per_flow": traffic.packets_per_flow,
        "start_cycle": traffic.start_cycle,
        "packet_rate": traffic.packet_rate.to_dict(),
        "packet_size_bytes": traffic.packet_size_bytes.to_dict(),
    }


def reconfiguration_to_dict(
    reconfiguration: PolicyChangeConfig,
) -> dict[str, object]:
    result: dict[str, object] = {
        "type": "policy_change",
        "mode": reconfiguration.mode,
        "cycle": reconfiguration.cycle,
        "name": reconfiguration.name,
        "before_label": reconfiguration.before_label,
        "after_label": reconfiguration.after_label,
    }
    if reconfiguration.target_tree is not None:
        result["target_tree"] = tree_to_dict(reconfiguration.target_tree)
    else:
        result["changes"] = {
            name: {
                "policy": change.policy,
                "flow_state": {
                    str(flow_id): state
                    for flow_id, state in sorted(change.flow_state.items())
                },
            }
            for name, change in reconfiguration.changes.items()
        }
    if reconfiguration.minimum_stop_cycles:
        result["minimum_stop_cycles"] = reconfiguration.minimum_stop_cycles
    return result


def default_strict_priorities(
    flow_ids: tuple[int, ...], max_packet_priority: int
) -> dict[int, int]:
    count = max(1, len(flow_ids))
    step = max(1, max_packet_priority // count)
    return {
        flow_id: min(max_packet_priority - 1, 1 + index * step)
        for index, flow_id in enumerate(sorted(flow_ids))
    }


def generate_distributed_requests(
    traffic: TrafficConfig, seed: int
) -> list[Request]:
    """Generate one packet per flow per round using seeded distributions.

    A sampled per-flow packet rate controls the gap to the next round. Packet
    sizes are sampled independently for every packet. Separate PRNG streams
    keep the arrival sequence stable when only the size distribution changes.
    """

    rate_rng = random.Random(seed)
    size_rng = random.Random(seed ^ 0x5A17_2C39)
    elapsed_cycles = 0.0
    request_id = 1
    requests: list[Request] = []
    for packet_index in range(traffic.packets_per_flow):
        cycle = traffic.start_cycle + round(elapsed_cycles)
        for flow_id in traffic.flow_ids:
            size_bytes = max(1, round(traffic.packet_size_bytes.sample(size_rng)))
            requests.append(
                Request(
                    cycle=cycle,
                    request_id=request_id,
                    global_flow_id=flow_id,
                    size_bytes=size_bytes,
                )
            )
            request_id += 1
        if packet_index + 1 < traffic.packets_per_flow:
            elapsed_cycles += 1.0 / traffic.packet_rate.sample(rate_rng)
    return requests


def write_effective_config(path: Path, config: ExperimentConfig) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(config.to_dict(), indent=2, sort_keys=False) + "\n",
        encoding="utf-8",
    )


def load_experiment_config(
    path: Path, output_dir_override: Path | None = None
) -> ExperimentConfig:
    try:
        raw = json.loads(path.read_text(encoding="utf-8"))
    except json.JSONDecodeError as error:
        raise ValueError(f"{path}:{error.lineno}:{error.colno}: {error.msg}") from error
    root = _object(raw, "config")
    _only_keys(
        root,
        {
            "output_dir",
            "seed",
            "traffic",
            "initial_tree",
            "reconfiguration",
            "policy_change",
            "simulation",
            "plot",
            "verification",
        },
        "config",
    )
    if "reconfiguration" in root and "policy_change" in root:
        raise ValueError(
            "config must use reconfiguration or legacy policy_change, not both"
        )

    simulation_raw = _object(root.get("simulation", {}), "simulation")
    _only_keys(
        simulation_raw,
        {
            "link_bytes_per_cycle",
            "queue_depth",
            "max_cycles",
            "max_packet_priority",
            "num_engines",
            "num_vpifos",
            "fifo_depth",
            "prefetch_buffer_depth",
        },
        "simulation",
    )
    simulation = SimulationConfig(
        link_bytes_per_cycle=_number(
            simulation_raw.get("link_bytes_per_cycle", 64.0),
            "simulation.link_bytes_per_cycle",
        ),
        queue_depth=_integer(
            simulation_raw.get("queue_depth", 256), "simulation.queue_depth"
        ),
        max_cycles=_integer(
            simulation_raw.get("max_cycles", 100_000), "simulation.max_cycles"
        ),
        max_packet_priority=_integer(
            simulation_raw.get("max_packet_priority", 65_536),
            "simulation.max_packet_priority",
        ),
        num_engines=_integer(
            simulation_raw.get("num_engines", 2), "simulation.num_engines"
        ),
        num_vpifos=_integer(
            simulation_raw.get("num_vpifos", 32), "simulation.num_vpifos"
        ),
        fifo_depth=_integer(
            simulation_raw.get("fifo_depth", 32), "simulation.fifo_depth"
        ),
        prefetch_buffer_depth=_integer(
            simulation_raw.get("prefetch_buffer_depth", 2),
            "simulation.prefetch_buffer_depth",
        ),
    )

    traffic_raw = _required_object(root, "traffic", "config")
    _only_keys(
        traffic_raw,
        {
            "flows",
            "packets_per_flow",
            "start_cycle",
            "packet_rate",
            "packet_size_bytes",
        },
        "traffic",
    )
    flows_raw = traffic_raw.get("flows")
    if not isinstance(flows_raw, list):
        raise ValueError("traffic.flows must be an array of integers")
    flow_ids = tuple(
        _integer(flow_id, f"traffic.flows[{index}]")
        for index, flow_id in enumerate(flows_raw)
    )
    traffic = TrafficConfig(
        flow_ids=flow_ids,
        packets_per_flow=_integer(
            _required(traffic_raw, "packets_per_flow", "traffic"),
            "traffic.packets_per_flow",
        ),
        start_cycle=_integer(traffic_raw.get("start_cycle", 0), "traffic.start_cycle"),
        packet_rate=_parse_distribution(
            _required(traffic_raw, "packet_rate", "traffic"),
            "traffic.packet_rate",
            required_unit=PACKET_RATE_UNIT,
        ),
        packet_size_bytes=_parse_distribution(
            _required(traffic_raw, "packet_size_bytes", "traffic"),
            "traffic.packet_size_bytes",
        ),
    )

    legacy_change = "policy_change" in root
    if legacy_change:
        reconfiguration_raw = _required_object(root, "policy_change", "config")
        reconfiguration_location = "policy_change"
    else:
        reconfiguration_raw = _required_object(root, "reconfiguration", "config")
        reconfiguration_location = "reconfiguration"

    initial_policy = _initial_policy_hint(
        reconfiguration_raw, reconfiguration_location, legacy_change
    )
    if "initial_tree" in root:
        initial_tree = parse_tree_config(
            _required_object(root, "initial_tree", "config"), "initial_tree"
        )
    else:
        initial_state: Mapping[int, int] = {}
        if initial_policy == "SP":
            initial_state = _parse_flow_state(
                reconfiguration_raw.get("strict_priorities"),
                f"{reconfiguration_location}.strict_priorities",
                traffic.flow_ids,
                simulation.max_packet_priority,
                default_when_missing=True,
            )
        initial_tree = _default_initial_tree(
            traffic.flow_ids, initial_policy, initial_state
        )
    reconfiguration = _parse_reconfiguration(
        reconfiguration_raw,
        reconfiguration_location,
        legacy_change,
        initial_tree,
        traffic.flow_ids,
        simulation.max_packet_priority,
    )

    plot_raw = _object(root.get("plot", {}), "plot")
    _only_keys(
        plot_raw,
        {
            "bandwidth_window_cycles",
            "bandwidth_sample_cycles",
            "flow_labels",
            "dpi",
        },
        "plot",
    )
    labels_raw = _object(plot_raw.get("flow_labels", {}), "plot.flow_labels")
    labels: dict[int, str] = {}
    for raw_flow_id, raw_label in labels_raw.items():
        try:
            flow_id = int(raw_flow_id, 0)
        except (TypeError, ValueError) as error:
            raise ValueError(
                "plot.flow_labels keys must be integer flow IDs"
            ) from error
        labels[flow_id] = _string(raw_label, f"plot.flow_labels.{raw_flow_id}")
    plot = PlotConfig(
        bandwidth_window_cycles=_integer(
            plot_raw.get("bandwidth_window_cycles", 256),
            "plot.bandwidth_window_cycles",
        ),
        bandwidth_sample_cycles=_integer(
            plot_raw.get("bandwidth_sample_cycles", 8),
            "plot.bandwidth_sample_cycles",
        ),
        flow_labels=labels,
        dpi=_integer(plot_raw.get("dpi", 180), "plot.dpi"),
    )

    verification: PhaseVerificationConfig | None = None
    if "verification" in root:
        verification_raw = _required_object(root, "verification", "config")
        verification_fields = {
            "minimum_staging_cycles",
            "minimum_old_backlog_packets",
            "minimum_drain_cycles",
            "minimum_packets_per_phase",
        }
        _only_keys(verification_raw, verification_fields, "verification")
        verification = PhaseVerificationConfig(
            minimum_staging_cycles=_integer(
                verification_raw.get("minimum_staging_cycles", 1),
                "verification.minimum_staging_cycles",
            ),
            minimum_old_backlog_packets=_integer(
                verification_raw.get("minimum_old_backlog_packets", 1),
                "verification.minimum_old_backlog_packets",
            ),
            minimum_drain_cycles=_integer(
                verification_raw.get("minimum_drain_cycles", 1),
                "verification.minimum_drain_cycles",
            ),
            minimum_packets_per_phase=_integer(
                verification_raw.get("minimum_packets_per_phase", 1),
                "verification.minimum_packets_per_phase",
            ),
        )

    configured_output = Path(
        _string(root.get("output_dir", "experiment-results/rr-to-sp"), "output_dir")
    )
    return ExperimentConfig(
        output_dir=output_dir_override or configured_output,
        seed=_integer(root.get("seed", 1), "seed"),
        traffic=traffic,
        initial_tree=initial_tree,
        reconfiguration=reconfiguration,
        simulation=simulation,
        plot=plot,
        verification=verification,
    )


def _initial_policy_hint(
    value: Mapping[str, object], location: str, legacy: bool
) -> str:
    if legacy:
        return _string(value.get("before", "RR"), f"{location}.before").upper()
    kind = _string(
        _required(value, "type", location), f"{location}.type"
    ).lower()
    if kind == "policy_change" and "before" in value:
        return _string(value["before"], f"{location}.before").upper()
    label = value.get("before_label", "RR")
    if isinstance(label, str) and label.strip().upper() in SUPPORTED_POLICIES:
        return label.strip().upper()
    return "RR"


def _default_initial_tree(
    flow_ids: tuple[int, ...], policy: str, flow_state: Mapping[int, int]
) -> InitialTreeConfig:
    return InitialTreeConfig(
        root="root",
        nodes={
            "root": TreeNodeConfig(
                engine_id=1,
                vpifo_id=10,
                policy=policy,
                flow_state=dict(flow_state),
            )
        },
        flow_paths={flow_id: ("root",) for flow_id in flow_ids},
    )


def parse_tree_config(
    value: object, location: str = "tree"
) -> InitialTreeConfig:
    tree = _object(value, location)
    _only_keys(tree, {"root", "nodes", "flow_paths"}, location)
    root_name = _string(_required(tree, "root", location), f"{location}.root")
    nodes_raw = _object(
        _required(tree, "nodes", location), f"{location}.nodes"
    )
    nodes: dict[str, TreeNodeConfig] = {}
    for name, raw_node in nodes_raw.items():
        node_location = f"{location}.nodes.{name}"
        node = _object(raw_node, node_location)
        _only_keys(
            node,
            {"engine_id", "vpifo_id", "policy", "flow_state"},
            node_location,
        )
        nodes[name] = TreeNodeConfig(
            engine_id=_integer(
                _required(node, "engine_id", node_location),
                f"{node_location}.engine_id",
            ),
            vpifo_id=_integer(
                _required(node, "vpifo_id", node_location),
                f"{node_location}.vpifo_id",
            ),
            policy=_string(
                _required(node, "policy", node_location),
                f"{node_location}.policy",
            ).upper(),
            flow_state=_parse_integer_mapping(
                node.get("flow_state", {}), f"{node_location}.flow_state"
            ),
        )
    paths_raw = _object(
        _required(tree, "flow_paths", location),
        f"{location}.flow_paths",
    )
    paths: dict[int, tuple[str, ...]] = {}
    for raw_flow_id, raw_path in paths_raw.items():
        flow_id = _mapping_key_integer(
            raw_flow_id, f"{location}.flow_paths keys"
        )
        if not isinstance(raw_path, list):
            raise ValueError(
                f"{location}.flow_paths.{raw_flow_id} must be an array of node names"
            )
        paths[flow_id] = tuple(
            _string(node_name, f"{location}.flow_paths.{raw_flow_id}[{index}]")
            for index, node_name in enumerate(raw_path)
        )
    return InitialTreeConfig(root=root_name, nodes=nodes, flow_paths=paths)


def parse_policy_change_config(
    value: object,
    initial_tree: InitialTreeConfig,
    max_packet_priority: int,
    location: str = "move",
) -> PolicyChangeConfig:
    move = _object(value, location)
    return _parse_reconfiguration(
        move,
        location,
        legacy=False,
        initial_tree=initial_tree,
        traffic_flows=tuple(sorted(initial_tree.flow_paths)),
        max_packet_priority=max_packet_priority,
    )


def parse_distribution_spec(
    value: object,
    location: str,
    required_unit: str | None = None,
) -> DistributionSpec:
    return _parse_distribution(value, location, required_unit)


def _parse_reconfiguration(
    value: Mapping[str, object],
    location: str,
    legacy: bool,
    initial_tree: InitialTreeConfig,
    traffic_flows: tuple[int, ...],
    max_packet_priority: int,
) -> PolicyChangeConfig:
    if legacy:
        _only_keys(
            value,
            {"cycle", "before", "after", "strict_priorities", "settle_cycles"},
            location,
        )
        before = _policy(value.get("before", "RR"), f"{location}.before")
        after = _policy(value.get("after", "SP"), f"{location}.after")
        root_node = initial_tree.nodes[initial_tree.root]
        if root_node.policy != before:
            raise ValueError(
                f"{location}.before must match the initial root policy {root_node.policy}"
            )
        state = _parse_flow_state(
            value.get("strict_priorities"),
            f"{location}.strict_priorities",
            traffic_flows,
            max_packet_priority,
            default_when_missing=after == "SP",
        )
        return PolicyChangeConfig(
            cycle=_integer(_required(value, "cycle", location), f"{location}.cycle"),
            name="policy-change",
            before_label=before,
            after_label=after,
            changes={
                initial_tree.root: NodePolicyChangeConfig(
                    policy=after, flow_state=state
                )
            },
        )

    kind = _string(_required(value, "type", location), f"{location}.type").lower()
    if kind != "policy_change":
        raise ValueError(f"{location}.type must be policy_change")

    _only_keys(
        value,
        {
            "type",
            "mode",
            "cycle",
            "name",
            "before",
            "after",
            "strict_priorities",
            "before_label",
            "after_label",
            "changes",
            "target_tree",
            "minimum_stop_cycles",
        },
        location,
    )
    mode = _string(value.get("mode", "full_transitive"), f"{location}.mode").lower()
    if mode not in SUPPORTED_RECONFIGURATION_MODES:
        raise ValueError(
            "policy_change mode must be in_place, stop_the_world, "
            "full_transitive, or confined_transitive"
        )
    target_tree: InitialTreeConfig | None = None
    if "target_tree" in value:
        if any(
            key in value
            for key in ("changes", "before", "after", "strict_priorities")
        ):
            raise ValueError(
                f"{location}.target_tree cannot be combined with changes, before, "
                "after, or strict_priorities"
            )
        target_tree = parse_tree_config(
            value["target_tree"], f"{location}.target_tree"
        )
        changes = {}
        root_before = initial_tree.nodes[initial_tree.root].policy
        root_after = target_tree.nodes[target_tree.root].policy
        before_label = _optional_label(
            value.get("before_label", root_before), f"{location}.before_label"
        )
        after_label = _optional_label(
            value.get("after_label", root_after), f"{location}.after_label"
        )
    elif "changes" in value:
        if any(key in value for key in ("before", "after", "strict_priorities")):
            raise ValueError(
                f"{location}.changes cannot be combined with before, after, or strict_priorities"
            )
        changes = _parse_node_changes(value["changes"], f"{location}.changes")
        root_before = initial_tree.nodes[initial_tree.root].policy
        root_after = changes.get(
            initial_tree.root,
            NodePolicyChangeConfig(root_before, {}),
        ).policy
        before_label = _optional_label(
            value.get("before_label", root_before), f"{location}.before_label"
        )
        after_label = _optional_label(
            value.get("after_label", root_after), f"{location}.after_label"
        )
    else:
        before = _policy(value.get("before", "RR"), f"{location}.before")
        after = _policy(value.get("after", "SP"), f"{location}.after")
        if initial_tree.nodes[initial_tree.root].policy != before:
            raise ValueError(
                f"{location}.before must match the initial root policy "
                f"{initial_tree.nodes[initial_tree.root].policy}"
            )
        state = _parse_flow_state(
            value.get("strict_priorities"),
            f"{location}.strict_priorities",
            traffic_flows,
            max_packet_priority,
            default_when_missing=after == "SP",
        )
        changes = {
            initial_tree.root: NodePolicyChangeConfig(
                policy=after, flow_state=state
            )
        }
        before_label = _optional_label(
            value.get("before_label", before), f"{location}.before_label"
        )
        after_label = _optional_label(
            value.get("after_label", after), f"{location}.after_label"
        )
    return PolicyChangeConfig(
        cycle=_integer(_required(value, "cycle", location), f"{location}.cycle"),
        name=_string(value.get("name", "policy-change"), f"{location}.name"),
        before_label=before_label,
        after_label=after_label,
        changes=changes,
        mode=mode,
        target_tree=target_tree,
        minimum_stop_cycles=_integer(
            value.get("minimum_stop_cycles", 0),
            f"{location}.minimum_stop_cycles",
        ),
    )


def _parse_node_changes(
    raw: object, location: str
) -> dict[str, NodePolicyChangeConfig]:
    value = _object(raw, location)
    result: dict[str, NodePolicyChangeConfig] = {}
    for name, raw_change in value.items():
        change_location = f"{location}.{name}"
        change = _object(raw_change, change_location)
        _only_keys(change, {"policy", "flow_state"}, change_location)
        result[name] = NodePolicyChangeConfig(
            policy=_policy(
                _required(change, "policy", change_location),
                f"{change_location}.policy",
            ),
            flow_state=_parse_integer_mapping(
                change.get("flow_state", {}), f"{change_location}.flow_state"
            ),
        )
    return result


def _parse_flow_state(
    raw: object | None,
    location: str,
    flow_ids: tuple[int, ...],
    max_packet_priority: int,
    default_when_missing: bool,
) -> dict[int, int]:
    if raw is None:
        if default_when_missing:
            return default_strict_priorities(flow_ids, max_packet_priority)
        return {}
    return _parse_integer_mapping(raw, location)


def _parse_integer_mapping(raw: object, location: str) -> dict[int, int]:
    value = _object(raw, location)
    result: dict[int, int] = {}
    for raw_key, raw_value in value.items():
        key = _mapping_key_integer(raw_key, f"{location} keys")
        result[key] = _integer(raw_value, f"{location}.{raw_key}")
    return result


def _mapping_key_integer(raw_key: str, location: str) -> int:
    try:
        return int(raw_key, 0)
    except (TypeError, ValueError) as error:
        raise ValueError(f"{location} must be integer IDs") from error


def _policy(raw: object, location: str) -> str:
    result = _string(raw, location).upper()
    if result not in SUPPORTED_POLICIES:
        raise ValueError(f"unsupported policy {result!r} at {location}")
    return result


def _optional_label(raw: object, location: str) -> str:
    if not isinstance(raw, str):
        raise ValueError(f"{location} must be a string")
    return raw.strip()


def _parse_distribution(
    raw: object,
    location: str,
    required_unit: str | None = None,
) -> DistributionSpec:
    value = _object(raw, location)
    kind = _string(
        _required(value, "distribution", location),
        f"{location}.distribution",
    ).lower()
    common_keys = {"distribution", "unit"}
    if kind == "constant":
        _only_keys(value, common_keys | {"value"}, location)
        spec = DistributionSpec(
            distribution=kind,
            value=_number(_required(value, "value", location), f"{location}.value"),
            unit=_optional_unit(value, location, required_unit),
        )
    elif kind == "uniform":
        _only_keys(value, common_keys | {"min", "max"}, location)
        spec = DistributionSpec(
            distribution=kind,
            minimum=_number(_required(value, "min", location), f"{location}.min"),
            maximum=_number(_required(value, "max", location), f"{location}.max"),
            unit=_optional_unit(value, location, required_unit),
        )
    elif kind == "normal":
        _only_keys(value, common_keys | {"mean", "stddev", "min", "max"}, location)
        spec = DistributionSpec(
            distribution=kind,
            mean=_number(_required(value, "mean", location), f"{location}.mean"),
            stddev=_number(_required(value, "stddev", location), f"{location}.stddev"),
            minimum=_number(_required(value, "min", location), f"{location}.min"),
            maximum=_number(_required(value, "max", location), f"{location}.max"),
            unit=_optional_unit(value, location, required_unit),
        )
    else:
        raise ValueError(
            f"{location}.distribution must be constant, uniform, or normal"
        )
    return spec


def _optional_unit(
    value: Mapping[str, object], location: str, required_unit: str | None
) -> str | None:
    if required_unit is None:
        if "unit" not in value:
            return None
        return _string(value["unit"], f"{location}.unit")
    unit = _string(_required(value, "unit", location), f"{location}.unit")
    if unit != required_unit:
        raise ValueError(f"{location}.unit must be {required_unit!r}")
    return unit


def _object(value: object, location: str) -> dict[str, object]:
    if not isinstance(value, dict) or not all(isinstance(key, str) for key in value):
        raise ValueError(f"{location} must be a JSON object")
    return value


def _required_object(
    value: Mapping[str, object], key: str, location: str
) -> dict[str, object]:
    child_location = f"{location}.{key}".removeprefix("config.")
    return _object(_required(value, key, location), child_location)


def _required(value: Mapping[str, object], key: str, location: str) -> object:
    if key not in value:
        raise ValueError(f"{location}.{key} is required")
    return value[key]


def _only_keys(value: Mapping[str, object], allowed: set[str], location: str) -> None:
    unknown = sorted(set(value).difference(allowed))
    if unknown:
        raise ValueError(f"{location}: unknown field(s): {', '.join(unknown)}")


def _integer(value: object, location: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ValueError(f"{location} must be an integer")
    return value


def _number(value: object, location: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{location} must be a number")
    result = float(value)
    if not math.isfinite(result):
        raise ValueError(f"{location} must be finite")
    return result


def _string(value: object, location: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{location} must be a non-empty string")
    return value.strip()
