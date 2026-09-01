"""Core compiler from a declarative tree move to controller commands."""

from __future__ import annotations

from dataclasses import dataclass

from pifo_experiment_config import (
    InitialTreeConfig,
    PolicyChangeConfig,
    TreeNodeConfig,
)
from pifo_transaction_program import ControllerCommand


POLICY_TO_BRAIN = {
    "RR": 1,
    "WFQ": 1,
    "SP": 2,
    "FIFO": 3,
}


@dataclass(frozen=True)
class TransactionPlan:
    initial_commands: tuple[ControllerCommand, ...]
    transaction_commands: tuple[ControllerCommand, ...]
    name: str
    mode: str
    cycle: int
    before_label: str
    after_label: str
    root_engine_id: int
    root_vpifo_id: int
    drain_engine_id: int | None
    drain_vpifo_id: int | None


def pack_flow_id(engine_id: int, vpifo_or_flow_id: int, num_vpifos: int) -> int:
    vpifo_width = (num_vpifos - 1).bit_length()
    return (engine_id << vpifo_width) | vpifo_or_flow_id


def build_transaction_plan(
    initial_tree: InitialTreeConfig,
    reconfiguration: PolicyChangeConfig,
    num_vpifos: int,
) -> TransactionPlan:
    initial_commands = tuple(_configure_tree(initial_tree, num_vpifos))
    root = initial_tree.nodes[initial_tree.root]
    new_tree = _copy_tree_for_policy_change(
        initial_tree,
        reconfiguration,
        num_vpifos,
    )
    transaction_commands = tuple(
        _configure_full_transitive(
            old_tree=initial_tree,
            new_tree=new_tree,
            num_vpifos=num_vpifos,
        )
    )
    return TransactionPlan(
        initial_commands=initial_commands,
        transaction_commands=transaction_commands,
        name=reconfiguration.name,
        mode=reconfiguration.mode,
        cycle=reconfiguration.cycle,
        before_label=reconfiguration.before_label,
        after_label=reconfiguration.after_label,
        root_engine_id=root.engine_id,
        root_vpifo_id=root.vpifo_id,
        drain_engine_id=root.engine_id,
        drain_vpifo_id=root.vpifo_id,
    )


def _copy_tree_for_policy_change(
    tree: InitialTreeConfig,
    change: PolicyChangeConfig,
    num_vpifos: int,
) -> InitialTreeConfig:
    allocated = _allocate_copy_ids(tree, num_vpifos)
    nodes: dict[str, TreeNodeConfig] = {}
    for name, old_node in tree.nodes.items():
        node_change = change.changes.get(name)
        flow_state = dict(old_node.flow_state)
        if node_change is not None:
            flow_state.update(node_change.flow_state)
        nodes[name] = TreeNodeConfig(
            engine_id=old_node.engine_id,
            vpifo_id=allocated[name],
            policy=node_change.policy if node_change is not None else old_node.policy,
            flow_state=flow_state,
        )
    return InitialTreeConfig(
        root=tree.root,
        nodes=nodes,
        flow_paths=tree.flow_paths,
    )


def _allocate_copy_ids(
    tree: InitialTreeConfig, num_vpifos: int
) -> dict[str, int]:
    used_by_engine: dict[int, set[int]] = {}
    for node in tree.nodes.values():
        used_by_engine.setdefault(node.engine_id, set()).add(node.vpifo_id)

    result: dict[str, int] = {}
    preferred = tuple(range(10, num_vpifos - 1)) + tuple(
        range(0, min(10, num_vpifos - 1))
    )
    for name, node in tree.nodes.items():
        used = used_by_engine.setdefault(node.engine_id, set())
        try:
            new_id = next(candidate for candidate in preferred if candidate not in used)
        except StopIteration as error:
            raise ValueError(
                f"cannot copy full tree: engine {node.engine_id} has no free vPifo ID"
            ) from error
        used.add(new_id)
        result[name] = new_id
    return result


def _configure_tree(
    tree: InitialTreeConfig, num_vpifos: int
) -> list[ControllerCommand]:
    commands = _configure_brains(tree, num_vpifos)
    commands.extend(_configure_flow_mappings(tree, num_vpifos, include_pre=True))
    commands.append(_commit_command())
    return commands


def _configure_full_transitive(
    old_tree: InitialTreeConfig,
    new_tree: InitialTreeConfig,
    num_vpifos: int,
) -> list[ControllerCommand]:
    commands = _configure_brains(new_tree, num_vpifos)
    commands.extend(_configure_flow_mappings(new_tree, num_vpifos, include_pre=True))

    new_root = new_tree.nodes[new_tree.root]
    old_root = old_tree.nodes[old_tree.root]
    if old_root.engine_id != new_root.engine_id:
        raise ValueError(
            "front underflow rewrite requires old and new roots on the same engine"
        )
    # Root dequeue requests intentionally keep targeting old_root. Once its old
    # tokens drain, the per-engine front table rewrites later requests directly
    # to new_root. The engine holds the next request for the activation cycle.
    commands.append(
        ControllerCommand(
            command="UpdateMapperNonExist",
            engine_id=old_root.engine_id,
            vpifo_id=old_root.vpifo_id,
            flow_id=0,
            data=new_root.vpifo_id,
        )
    )
    commands.append(_commit_command())
    return commands


def _configure_brains(
    tree: InitialTreeConfig, num_vpifos: int
) -> list[ControllerCommand]:
    commands: list[ControllerCommand] = []
    for name, node in tree.nodes.items():
        commands.append(
            ControllerCommand(
                command="UpdateBrainEngine",
                engine_id=node.engine_id,
                vpifo_id=node.vpifo_id,
                flow_id=0,
                data=POLICY_TO_BRAIN[node.policy],
            )
        )
        for flow_id, state in sorted(node.flow_state.items()):
            commands.append(
                ControllerCommand(
                    command="UpdateBrainFlowState",
                    engine_id=node.engine_id,
                    vpifo_id=node.vpifo_id,
                    flow_id=pack_flow_id(node.engine_id, flow_id, num_vpifos),
                    data=state,
                )
            )
    return commands


def _configure_flow_mappings(
    tree: InitialTreeConfig,
    num_vpifos: int,
    include_pre: bool,
) -> list[ControllerCommand]:
    commands: list[ControllerCommand] = []
    for flow_id, path in sorted(tree.flow_paths.items()):
        for index, node_name in enumerate(path):
            node = tree.nodes[node_name]
            if include_pre:
                commands.append(
                    ControllerCommand(
                        command="UpdateMapperPre",
                        engine_id=node.engine_id,
                        vpifo_id=flow_id,
                        flow_id=0,
                        data=node.vpifo_id,
                    )
                )
            if index + 1 < len(path):
                next_node = tree.nodes[path[index + 1]]
                output = pack_flow_id(
                    next_node.engine_id, next_node.vpifo_id, num_vpifos
                )
            else:
                output = pack_flow_id(0, flow_id, num_vpifos)
            commands.append(
                ControllerCommand(
                    command="UpdateMapperPost",
                    engine_id=node.engine_id,
                    vpifo_id=node.vpifo_id,
                    flow_id=pack_flow_id(node.engine_id, flow_id, num_vpifos),
                    data=output,
                )
            )
    return commands


def _commit_command() -> ControllerCommand:
    return ControllerCommand(
        command="CommitMapper", engine_id=1, vpifo_id=0, flow_id=0, data=0
    )
