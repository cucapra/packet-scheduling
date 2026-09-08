#!/usr/bin/env python3
"""Compile whole-tree link, reserved-wrapper and copy/prefill experiments."""
from __future__ import annotations

import argparse
import json
from collections import Counter
from dataclasses import replace
from pathlib import Path

from pifo_tree_compiler import compile_tree_move, load_tree_move_program
from pifo_tree_compiler_core import pack_flow_id
from pifo_transaction_program import ControllerCommand, write_transaction_program

MECHANISMS = ("link", "reserved", "copy")
TOPOLOGY = "per-flow-hardware-fifo-v1"
RULES = "shared-replay-guarded-cleanup-v1"


def compiled_flow_paths(tree, commands):
    """Audit the actual enqueue addresses, including one dedicated FIFO per flow."""
    leaves = [path[-1] for path in tree.flow_paths.values()]
    if len(set(leaves)) != len(leaves) or any(tree.nodes[name].policy != "FIFO" for name in leaves):
        raise ValueError("the survivor experiment requires a distinct hardware FIFO leaf per flow")
    inputs = {(c.engine_id, c.vpifo_id): c.data for c in commands if c.command == "UpdateMapperPre"}
    return {str(flow): [[tree.nodes[name].engine_id, inputs[tree.nodes[name].engine_id, flow],
                        tree.nodes[name].policy] for name in path]
            for flow, path in tree.flow_paths.items()}


def compile_copy(source):
    """Move the frozen old tree down one PE, reusing its root PE as wrapper.

    Copy preserves token IDs/ranks, not brain state. The moved old tree is
    drain-only; the survivor is freshly configured and stays at its new depth.
    Reuse the ordinary wrapper compiler for allocation and guarded cleanup.
    """
    def descend(tree):
        return replace(tree, nodes={name: replace(node, engine_id=node.engine_id + 1)
                                    for name, node in tree.nodes.items()})

    shifted = replace(source, old_tree=descend(source.old_tree),
                      move=replace(source.move, target_tree=descend(source.move.target_tree)))
    program = compile_tree_move(shifted)
    original = compile_tree_move(source)
    main, cleanup, reclaim = program.transactions
    old_root = source.old_tree.nodes[source.old_tree.root]
    wrapper = next(c for c in main.commands if c.command == "PrefillPifo")
    if wrapper.engine_id != old_root.engine_id:
        raise ValueError("copy placement must reuse the original root PE for the wrapper")
    copies = [[engine, engine + 1] for engine in sorted(
        {node.engine_id for node in source.old_tree.nodes.values()}, reverse=True)]
    width = (source.hardware.num_vpifos - 1).bit_length()
    mask = (1 << width) - 1
    def move_address(address):
        return pack_flow_id((address >> width) + 1, address & mask, source.hardware.num_vpifos) if address >> width else address

    copied_maps = [replace(c, engine_id=c.engine_id + 1, data=move_address(c.data))
                   for c in original.initial.commands if c.command == "UpdateMapperPost"]
    # Empty source PEs may now hold a different policy. Retire their old slots
    # before installing it; preserve packed source IDs in the copied post maps.
    clear_source = [replace(c, data=0) for c in original.initial.commands
                    if c.command != "CommitMapper"]
    commands = [replace(main.commands[0], engine_id=old_root.engine_id, vpifo_id=old_root.vpifo_id)]
    commands += [ControllerCommand("CopyPifoEngine", src, 0, 0, dst) for src, dst in copies]
    commands += clear_source + copied_maps + list(main.commands[1:])
    cleanup = replace(cleanup, commands=tuple(
        replace(c, flow_id=pack_flow_id(c.engine_id - 1, c.flow_id & mask, source.hardware.num_vpifos))
        if c.command in {"UpdateMapperPost", "UpdateBrainFlowState"} else c
        for c in cleanup.commands))
    program = replace(program, root_engine_id=old_root.engine_id, root_vpifo_id=old_root.vpifo_id,
                      initial=original.initial,
                      transactions=(replace(main, commands=tuple(commands)), cleanup, reclaim))
    return program, shifted.move.target_tree, copies


def compile_survivor(source, mechanism, cycle):
    if mechanism not in MECHANISMS:
        raise ValueError("mechanism must be link, reserved or copy")
    mode = "full_transitive" if mechanism == "link" else "stop_the_world_pop"
    source = replace(source, move=replace(source.move, mode=mode, cycle=cycle, name="replace"))
    if mechanism == "copy":
        program, target, copies = compile_copy(source)
    else:
        program, target, copies = compile_tree_move(source), source.move.target_tree, []
    main = program.transactions[0]
    wrapper = next((c for c in main.commands if c.command == "PrefillPifo"), None)
    categories = Counter()
    for c in main.commands:
        if c.command == "PrefillPifo":
            category = "prefill"
        elif c.command == "CopyPifoEngine":
            category = "copy"
        elif c.command == "UpdateMapperNonExist":
            category = "designate"
        elif wrapper and c.engine_id == wrapper.engine_id and c.command in {
                "UpdateBrainEngine", "UpdateBrainFlowState", "UpdateMapperPre", "UpdateMapperPost"} and c.data:
            category = "wrapper_creation"
        else:
            category = "rest"
        categories[category] += 1
    report = {"mechanism": mechanism, "t1": cycle, "topology": TOPOLOGY, "experiment_rules": RULES,
              "compiled_flow_paths": {
                  "p1": compiled_flow_paths(source.old_tree, program.initial.commands),
                  "p2b": compiled_flow_paths(target, main.commands)},
              "copy_list": copies,
              "copy_scope": "frozen old-tree descent only; root change at teardown, no survivor ascent" if copies else None,
              "creation_instruction_categories": dict(categories),
              "instruction_counts": {t.name: len(t.commands) for t in program.transactions},
              "reserved_pes": int(mechanism == "reserved"),
              "hardware_tokens_per_pe": program.hardware.num_vpifos * program.hardware.fifo_depth,
              "wrapper": [wrapper.engine_id, wrapper.vpifo_id] if wrapper else None}
    return program, report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--mechanism", choices=MECHANISMS, required=True)
    parser.add_argument("--cycle", type=int, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    program, report = compile_survivor(load_tree_move_program(args.input), args.mechanism, args.cycle)
    write_transaction_program(args.output, program)
    args.output.with_suffix(".plan.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
