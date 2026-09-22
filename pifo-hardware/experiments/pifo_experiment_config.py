"""Configuration for the combined RR/SP experiments."""

from __future__ import annotations

import json
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Mapping

import _paths  # Make the shared hardware tools available to experiment scripts.
from pifo_config import (
    InitialTreeConfig,
    PACKET_RATE_UNIT,
    PolicyChangeConfig,
    SUPPORTED_POLICIES,
    TrafficConfig,
    TreeNodeConfig,
    _integer,
    _number,
    _object,
    _only_keys,
    _parse_distribution,
    _parse_flow_state,
    _parse_reconfiguration,
    _required,
    _string,
    parse_tree_config,
    reconfiguration_to_dict,
    traffic_to_dict,
    tree_to_dict,
    validate_tree_move,
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


def _required_object(
    value: Mapping[str, object], key: str, location: str
) -> dict[str, object]:
    child_location = f"{location}.{key}".removeprefix("config.")
    return _object(_required(value, key, location), child_location)
