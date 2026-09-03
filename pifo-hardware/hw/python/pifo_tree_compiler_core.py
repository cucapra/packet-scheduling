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
    gated_flow_ids: tuple[int, ...]
    minimum_stop_cycles: int


def pack_flow_id(engine_id: int, vpifo_or_flow_id: int, num_vpifos: int) -> int:
    vpifo_width = (num_vpifos - 1).bit_length()
    return (engine_id << vpifo_width) | vpifo_or_flow_id


def build_transaction_plan(
    initial_tree: InitialTreeConfig,
    reconfiguration: PolicyChangeConfig,
    num_vpifos: int,
) -> TransactionPlan:
    initial_commands = tuple(_configure_tree(initial_tree, num_vpifos))
    old_root = initial_tree.nodes[initial_tree.root]
    target_tree = reconfiguration.target_tree or _apply_policy_changes(
        initial_tree, reconfiguration
    )
    drain: tuple[int, int] | None = None

    if reconfiguration.mode == "full_transitive":
        physical_target = _allocate_fresh_tree(
            target_tree, initial_tree, num_vpifos
        )
        transaction_commands = tuple(
            _configure_full_transitive(
                old_tree=initial_tree,
                new_tree=physical_target,
                num_vpifos=num_vpifos,
            )
        )
        drain = (old_root.engine_id, old_root.vpifo_id)
    elif reconfiguration.mode == "confined_transitive":
        transaction_commands, drain = _configure_confined_transitive(
            old_tree=initial_tree,
            target_tree=target_tree,
            num_vpifos=num_vpifos,
        )
        transaction_commands = tuple(transaction_commands)
    elif reconfiguration.mode == "in_place":
        transaction_commands = tuple(
            _configure_in_place(initial_tree, target_tree, num_vpifos)
        )
    elif reconfiguration.mode == "stop_the_world":
        transaction_commands = tuple(
            _configure_stop_the_world(
                initial_tree, target_tree, num_vpifos
            )
        )
    else:  # Defensive: PolicyChangeConfig validates this first.
        raise ValueError(f"unsupported reconfiguration mode {reconfiguration.mode!r}")

    gated_flows = tuple(
        sorted(set(target_tree.flow_paths).difference(initial_tree.flow_paths))
    )
    return TransactionPlan(
        initial_commands=initial_commands,
        transaction_commands=transaction_commands,
        name=reconfiguration.name,
        mode=reconfiguration.mode,
        cycle=reconfiguration.cycle,
        before_label=reconfiguration.before_label,
        after_label=reconfiguration.after_label,
        root_engine_id=old_root.engine_id,
        root_vpifo_id=old_root.vpifo_id,
        drain_engine_id=drain[0] if drain is not None else None,
        drain_vpifo_id=drain[1] if drain is not None else None,
        gated_flow_ids=gated_flows,
        minimum_stop_cycles=reconfiguration.minimum_stop_cycles,
    )


def _apply_policy_changes(
    tree: InitialTreeConfig,
    change: PolicyChangeConfig,
) -> InitialTreeConfig:
    nodes: dict[str, TreeNodeConfig] = {}
    for name, old_node in tree.nodes.items():
        node_change = change.changes.get(name)
        flow_state = dict(old_node.flow_state)
        if node_change is not None:
            flow_state.update(node_change.flow_state)
        nodes[name] = TreeNodeConfig(
            engine_id=old_node.engine_id,
            vpifo_id=old_node.vpifo_id,
            policy=node_change.policy if node_change is not None else old_node.policy,
            flow_state=flow_state,
        )
    return InitialTreeConfig(
        root=tree.root,
        nodes=nodes,
        flow_paths=tree.flow_paths,
    )


def _allocate_fresh_tree(
    target: InitialTreeConfig,
    occupied_tree: InitialTreeConfig,
    num_vpifos: int,
) -> InitialTreeConfig:
    used_by_engine: dict[int, set[int]] = {}
    for node in occupied_tree.nodes.values():
        used_by_engine.setdefault(node.engine_id, set()).add(node.vpifo_id)

    result: dict[str, TreeNodeConfig] = {}
    # vPIFO 0 is the reset value of every enqueue mapper and must remain a
    # NOP/null sink. Allocating a real brain there would admit tokens from
    # engines that are not on a flow's path.
    preferred = tuple(range(10, num_vpifos - 1)) + tuple(
        range(1, min(10, num_vpifos - 1))
    )
    for name, node in target.nodes.items():
        used = used_by_engine.setdefault(node.engine_id, set())
        try:
            new_id = next(candidate for candidate in preferred if candidate not in used)
        except StopIteration as error:
            raise ValueError(
                f"cannot copy full tree: engine {node.engine_id} has no free vPifo ID"
            ) from error
        used.add(new_id)
        result[name] = TreeNodeConfig(
            engine_id=node.engine_id,
            vpifo_id=new_id,
            policy=node.policy,
            flow_state=dict(node.flow_state),
        )
    return InitialTreeConfig(
        root=target.root,
        nodes=result,
        flow_paths=target.flow_paths,
    )


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


def _configure_in_place(
    old_tree: InitialTreeConfig,
    target_tree: InitialTreeConfig,
    num_vpifos: int,
) -> list[ControllerCommand]:
    """Compile an additive update that needs no draining guard."""

    if set(old_tree.nodes) != set(target_tree.nodes):
        raise ValueError("in_place cannot add or remove PIFO nodes")
    for name, old_node in old_tree.nodes.items():
        target_node = target_tree.nodes[name]
        if (
            old_node.engine_id,
            old_node.vpifo_id,
            old_node.policy,
        ) != (
            target_node.engine_id,
            target_node.vpifo_id,
            target_node.policy,
        ):
            raise ValueError(
                "in_place requires every existing PIFO node to keep its "
                f"placement and policy; changed node {name!r}"
            )
    for flow_id, old_path in old_tree.flow_paths.items():
        if target_tree.flow_paths.get(flow_id) != old_path:
            raise ValueError(
                f"in_place cannot change the path of existing flow {flow_id}"
            )

    added_flows = sorted(
        set(target_tree.flow_paths).difference(old_tree.flow_paths)
    )
    commands: list[ControllerCommand] = []
    for name, target_node in target_tree.nodes.items():
        old_state = old_tree.nodes[name].flow_state
        for flow_id, state in sorted(target_node.flow_state.items()):
            if old_state.get(flow_id) != state:
                commands.append(
                    _brain_flow_state_command(
                        target_node, flow_id, state, num_vpifos
                    )
                )
    for flow_id in added_flows:
        commands.extend(
            _configure_one_flow(
                target_tree,
                flow_id,
                num_vpifos,
                include_pre=True,
            )
        )
    commands.append(_commit_command())
    return commands


def _configure_stop_the_world(
    old_tree: InitialTreeConfig,
    target_tree: InitialTreeConfig,
    num_vpifos: int,
) -> list[ControllerCommand]:
    """Compile the replacement installed after the evaluator clears hardware."""

    old_root = old_tree.nodes[old_tree.root]
    target_root = target_tree.nodes[target_tree.root]
    if (old_root.engine_id, old_root.vpifo_id) != (
        target_root.engine_id,
        target_root.vpifo_id,
    ):
        raise ValueError("stop_the_world must reuse the old physical root")
    return _configure_tree(target_tree, num_vpifos)


def _configure_confined_transitive(
    old_tree: InitialTreeConfig,
    target_tree: InitialTreeConfig,
    num_vpifos: int,
) -> tuple[list[ControllerCommand], tuple[int, int]]:
    """Replace one changed subtree while preserving its unchanged ancestors.

    New tokens enter a freshly allocated target subtree at commit. Requests
    from the unchanged parent continue to name the old subtree root. The
    existing front rewrite redirects that name only after the old root drains.
    """

    old_boundary, new_boundary, prefix, affected_flows = _confined_boundary(
        old_tree, target_tree
    )
    old_boundary_node = old_tree.nodes[old_boundary]
    target_boundary_node = target_tree.nodes[new_boundary]
    if old_boundary_node.engine_id != target_boundary_node.engine_id:
        raise ValueError(
            "confined_transitive requires old and new subtree roots on the "
            "same engine for the front rewrite"
        )

    unchanged_flows = set(old_tree.flow_paths).difference(affected_flows)
    users_of_old_boundary = {
        flow_id
        for flow_id, path in old_tree.flow_paths.items()
        if old_boundary in path
    }
    if users_of_old_boundary.intersection(unchanged_flows):
        raise ValueError(
            "confined_transitive boundary is shared by an unchanged flow"
        )

    new_subtree_names = {
        name
        for flow_id in affected_flows
        for name in target_tree.flow_paths[flow_id][len(prefix) :]
    }
    if new_boundary not in new_subtree_names:
        raise ValueError("confined_transitive target subtree is empty")

    physical_target = _allocate_selected_nodes(
        target_tree,
        old_tree,
        new_subtree_names,
        num_vpifos,
    )
    physical_boundary = physical_target.nodes[new_boundary]
    commands = _configure_brains_for_nodes(
        physical_target, new_subtree_names, num_vpifos
    )

    # Unchanged ancestors can acquire state for a newly admitted flow without
    # being copied. Existing-flow state is left untouched.
    for name in prefix:
        old_node = old_tree.nodes[name]
        target_node = target_tree.nodes[name]
        for flow_id in sorted(affected_flows):
            if (
                flow_id in target_node.flow_state
                and old_node.flow_state.get(flow_id)
                != target_node.flow_state[flow_id]
            ):
                commands.append(
                    _brain_flow_state_command(
                        old_node,
                        flow_id,
                        target_node.flow_state[flow_id],
                        num_vpifos,
                    )
                )

    old_flows = set(old_tree.flow_paths)
    for flow_id in sorted(affected_flows):
        target_path = target_tree.flow_paths[flow_id]
        if flow_id not in old_flows:
            # Install the new flow on preserved ancestors, but route the last
            # preserved hop through the old subtree name. The front rewrite
            # moves it to the new subtree only after the old tokens drain.
            for index, name in enumerate(prefix):
                node = old_tree.nodes[name]
                commands.append(_mapper_pre_command(node, flow_id))
                if index + 1 < len(prefix):
                    next_node = old_tree.nodes[prefix[index + 1]]
                    output = pack_flow_id(
                        next_node.engine_id, next_node.vpifo_id, num_vpifos
                    )
                else:
                    output = pack_flow_id(
                        old_boundary_node.engine_id,
                        old_boundary_node.vpifo_id,
                        num_vpifos,
                    )
                commands.append(
                    _mapper_post_command(node, flow_id, output, num_vpifos)
                )

        suffix_start = len(prefix)
        commands.extend(
            _configure_one_flow(
                physical_target,
                flow_id,
                num_vpifos,
                include_pre=True,
                start_index=suffix_start,
            )
        )

    commands.append(
        ControllerCommand(
            command="UpdateMapperNonExist",
            engine_id=old_boundary_node.engine_id,
            vpifo_id=old_boundary_node.vpifo_id,
            flow_id=0,
            data=physical_boundary.vpifo_id,
        )
    )
    commands.append(_commit_command())
    return commands, (old_boundary_node.engine_id, old_boundary_node.vpifo_id)


def _confined_boundary(
    old_tree: InitialTreeConfig,
    target_tree: InitialTreeConfig,
) -> tuple[str, str, tuple[str, ...], set[int]]:
    old_flows = set(old_tree.flow_paths)
    target_flows = set(target_tree.flow_paths)
    changed_existing = {
        flow_id
        for flow_id in old_flows
        if old_tree.flow_paths[flow_id] != target_tree.flow_paths.get(flow_id)
    }
    added = target_flows.difference(old_flows)
    if not changed_existing:
        raise ValueError(
            "confined_transitive requires at least one existing flow path to change"
        )

    boundaries: list[tuple[str, str, tuple[str, ...]]] = []
    for flow_id in sorted(changed_existing):
        old_path = old_tree.flow_paths[flow_id]
        target_path = target_tree.flow_paths[flow_id]
        prefix_length = 0
        while (
            prefix_length < min(len(old_path), len(target_path))
            and old_path[prefix_length] == target_path[prefix_length]
            and _same_preserved_node(
                old_tree.nodes[old_path[prefix_length]],
                target_tree.nodes[target_path[prefix_length]],
            )
        ):
            prefix_length += 1
        if prefix_length == 0:
            raise ValueError(
                "confined_transitive cannot preserve a common ancestor; "
                "use full_transitive"
            )
        if prefix_length == len(old_path) or prefix_length == len(target_path):
            raise ValueError(
                f"confined_transitive flow {flow_id} has no replaceable subtree root"
            )
        boundaries.append(
            (
                old_path[prefix_length],
                target_path[prefix_length],
                old_path[:prefix_length],
            )
        )

    first = boundaries[0]
    if any(boundary != first for boundary in boundaries[1:]):
        raise ValueError(
            "confined_transitive currently requires one shared old/new boundary"
        )
    old_boundary, new_boundary, prefix = first
    for flow_id in sorted(added):
        path = target_tree.flow_paths[flow_id]
        if path[: len(prefix)] != prefix or (
            len(path) <= len(prefix) or path[len(prefix)] != new_boundary
        ):
            raise ValueError(
                f"new flow {flow_id} does not enter the confined target boundary"
            )
    return old_boundary, new_boundary, prefix, changed_existing | added


def _same_preserved_node(old: TreeNodeConfig, target: TreeNodeConfig) -> bool:
    return (
        old.engine_id,
        old.vpifo_id,
        old.policy,
    ) == (
        target.engine_id,
        target.vpifo_id,
        target.policy,
    )


def _allocate_selected_nodes(
    target: InitialTreeConfig,
    occupied_tree: InitialTreeConfig,
    selected: set[str],
    num_vpifos: int,
) -> InitialTreeConfig:
    used_by_engine: dict[int, set[int]] = {}
    for node in occupied_tree.nodes.values():
        used_by_engine.setdefault(node.engine_id, set()).add(node.vpifo_id)
    preferred = tuple(range(10, num_vpifos - 1)) + tuple(
        range(1, min(10, num_vpifos - 1))
    )
    nodes: dict[str, TreeNodeConfig] = {}
    for name, node in target.nodes.items():
        if name not in selected:
            nodes[name] = node
            continue
        used = used_by_engine.setdefault(node.engine_id, set())
        try:
            vpifo_id = next(candidate for candidate in preferred if candidate not in used)
        except StopIteration as error:
            raise ValueError(
                f"cannot allocate confined subtree: engine {node.engine_id} "
                "has no free vPifo ID"
            ) from error
        used.add(vpifo_id)
        nodes[name] = TreeNodeConfig(
            engine_id=node.engine_id,
            vpifo_id=vpifo_id,
            policy=node.policy,
            flow_state=dict(node.flow_state),
        )
    return InitialTreeConfig(
        root=target.root,
        nodes=nodes,
        flow_paths=target.flow_paths,
    )


def _configure_brains(
    tree: InitialTreeConfig, num_vpifos: int
) -> list[ControllerCommand]:
    return _configure_brains_for_nodes(tree, set(tree.nodes), num_vpifos)


def _configure_brains_for_nodes(
    tree: InitialTreeConfig,
    node_names: set[str],
    num_vpifos: int,
) -> list[ControllerCommand]:
    commands: list[ControllerCommand] = []
    for name, node in tree.nodes.items():
        if name not in node_names:
            continue
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
                _brain_flow_state_command(node, flow_id, state, num_vpifos)
            )
    return commands


def _brain_flow_state_command(
    node: TreeNodeConfig,
    flow_id: int,
    state: int,
    num_vpifos: int,
) -> ControllerCommand:
    return ControllerCommand(
        command="UpdateBrainFlowState",
        engine_id=node.engine_id,
        vpifo_id=node.vpifo_id,
        flow_id=pack_flow_id(node.engine_id, flow_id, num_vpifos),
        data=state,
    )


def _configure_flow_mappings(
    tree: InitialTreeConfig,
    num_vpifos: int,
    include_pre: bool,
) -> list[ControllerCommand]:
    commands: list[ControllerCommand] = []
    for flow_id in sorted(tree.flow_paths):
        commands.extend(
            _configure_one_flow(
                tree, flow_id, num_vpifos, include_pre=include_pre
            )
        )
    return commands


def _configure_one_flow(
    tree: InitialTreeConfig,
    flow_id: int,
    num_vpifos: int,
    include_pre: bool,
    start_index: int = 0,
) -> list[ControllerCommand]:
    path = tree.flow_paths[flow_id]
    commands: list[ControllerCommand] = []
    for index in range(start_index, len(path)):
        node = tree.nodes[path[index]]
        if include_pre:
            commands.append(_mapper_pre_command(node, flow_id))
        if index + 1 < len(path):
            next_node = tree.nodes[path[index + 1]]
            output = pack_flow_id(
                next_node.engine_id, next_node.vpifo_id, num_vpifos
            )
        else:
            output = pack_flow_id(0, flow_id, num_vpifos)
        commands.append(
            _mapper_post_command(node, flow_id, output, num_vpifos)
        )
    return commands


def _mapper_pre_command(
    node: TreeNodeConfig, flow_id: int
) -> ControllerCommand:
    return ControllerCommand(
        command="UpdateMapperPre",
        engine_id=node.engine_id,
        vpifo_id=flow_id,
        flow_id=0,
        data=node.vpifo_id,
    )


def _mapper_post_command(
    node: TreeNodeConfig,
    flow_id: int,
    output: int,
    num_vpifos: int,
) -> ControllerCommand:
    return ControllerCommand(
        command="UpdateMapperPost",
        engine_id=node.engine_id,
        vpifo_id=node.vpifo_id,
        flow_id=pack_flow_id(node.engine_id, flow_id, num_vpifos),
        data=output,
    )


def _commit_command() -> ControllerCommand:
    return ControllerCommand(
        command="CommitMapper", engine_id=1, vpifo_id=0, flow_id=0, data=0
    )
