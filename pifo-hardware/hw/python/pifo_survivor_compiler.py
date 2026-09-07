#!/usr/bin/env python3
"""Compile the existing whole-tree move, with measured reserved-PE teardown."""
from __future__ import annotations

import argparse
import json
from collections import Counter
from dataclasses import replace
from pathlib import Path

from pifo_tree_compiler import compile_tree_move, load_tree_move_program
from pifo_transaction_program import ControllerCommand as Command, TimedTransaction, write_transaction_program

MECHANISMS = ("link", "reserved")
TOPOLOGY = "per-flow-hardware-fifo-v1"


def compiled_flow_paths(tree, commands):
    """Audit the actual enqueue addresses, including one dedicated FIFO per flow."""
    leaves = [path[-1] for path in tree.flow_paths.values()]
    if len(set(leaves)) != len(leaves) or any(tree.nodes[name].policy != "FIFO" for name in leaves):
        raise ValueError("the survivor experiment requires a distinct hardware FIFO leaf per flow")
    inputs = {(c.engine_id, c.vpifo_id): c.data for c in commands if c.command == "UpdateMapperPre"}
    return {str(flow): [[tree.nodes[name].engine_id, inputs[tree.nodes[name].engine_id, flow],
                        tree.nodes[name].policy] for name in path]
            for flow, path in tree.flow_paths.items()}


def compile_survivor(source, mechanism, cycle):
    if mechanism not in MECHANISMS:
        raise ValueError("mechanism must be link or reserved")
    mode = "full_transitive" if mechanism == "link" else "stop_the_world_pop"
    source = replace(source, move=replace(source.move, mode=mode, cycle=cycle, name="replace"))
    program = compile_tree_move(source)
    main = program.transactions[0]
    wrapper = next((c for c in main.commands if c.command == "PrefillPifo"), None)
    categories = Counter()
    for c in main.commands:
        if c.command == "PrefillPifo":
            category = "prefill"
        elif c.command == "UpdateMapperNonExist":
            category = "designate"
        elif wrapper and c.engine_id == wrapper.engine_id and c.command not in {"UpdateRoot", "CommitMapper"}:
            category = "wrapper_creation"
        else:
            category = "rest"
        categories[category] += 1
    if wrapper:
        new_root = next(c for c in main.commands if c.command == "UpdateBrainEngine" and c.engine_id == program.root_engine_id)
        cleanup = [Command("WaitPifoEmpty", *main.drain_root, 0, 0)]
        cleanup += [Command("UpdateMapperPre", wrapper.engine_id, f, 0, 0)
                    for f in sorted(source.move.target_tree.flow_paths)]
        cleanup += [Command("UpdateRoot", new_root.engine_id, new_root.vpifo_id, 0, 0),
                    Command("CommitMapper", 1, 0, 0, 0)]
        # Publication detaches the redundant wrapper level. Bank synchronization
        # finishes before reclamation, so old in-flight traversals can complete.
        program = replace(program, transactions=(main,
            TimedTransaction(cycle, "collapse", tuple(cleanup)),
            TimedTransaction(cycle, "reclaim", (
                Command("ClearPifoEngine", wrapper.engine_id, 0, 0, 0),
                Command("CommitMapper", 1, 0, 0, 0)))))
    report = {"mechanism": mechanism, "t1": cycle, "topology": TOPOLOGY,
              "compiled_flow_paths": {
                  "p1": compiled_flow_paths(source.old_tree, program.initial.commands),
                  "p2b": compiled_flow_paths(source.move.target_tree, main.commands)},
              "creation_instruction_categories": dict(categories),
              "instruction_counts": {t.name: len(t.commands) for t in program.transactions},
              "reserved_pes": int(wrapper is not None),
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
