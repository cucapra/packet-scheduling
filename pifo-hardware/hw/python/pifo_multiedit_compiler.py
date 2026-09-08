#!/usr/bin/env python3
"""Compile a tenant-tree operator request to timed, direct hardware packages.

Each tenant has a stable slot, root weight, SP/RR/WFQ policy, and FIFO flows.
Copy targets preserve vPIFO IDs; old packed token IDs are retained in post maps.
"""
from __future__ import annotations

import argparse
import json
from dataclasses import replace
from pathlib import Path

from pifo_transaction_program import (
    ControllerCommand as Command, HardwareShape, TimedTransaction, TransactionProgram,
    write_transaction_program,
)

MECHANISMS = ("control", "control-p2", "rio", "prefill", "relocate", "reset")


def compile_request(spec: dict, mechanism: str) -> tuple[TransactionProgram, dict]:
    if spec["schema"] != "pifo-multi-edit-v1" or mechanism not in MECHANISMS:
        raise ValueError("unsupported request schema or mechanism")
    hw = HardwareShape(**spec["hardware"])
    if hw.num_engines < 7 or hw.num_vpifos < 16:
        raise ValueError("this three-level placement requires 7 PEs and 16 vPIFOs")
    old, new = spec["before"], spec["after"]
    for tree in (old, new):
        names = [t["name"] for t in tree]
        slots = [t["slot"] for t in tree]
        flows = [f["id"] for t in tree for f in t["flows"]]
        if len(set(names)) != len(names) or len(set(slots)) != len(slots) or len(set(flows)) != len(flows):
            raise ValueError("tenant names, slots and flow IDs must be unique within a tree")
        if any(not 1 <= value < hw.num_vpifos - 1 for value in slots + flows):
            raise ValueError("slots and flow IDs must exclude the null and sentinel IDs")
        if any(t["policy"] not in {"SP", "RR", "WFQ"} or not t["flows"] for t in tree):
            raise ValueError("tenant policies must be SP/RR/WFQ with nonempty flows")
    old_by_name = {t["name"]: t for t in old}
    for tenant in new:
        if tenant["name"] in old_by_name:
            previous = old_by_name[tenant["name"]]
            if any(previous[k] != tenant[k] for k in ("slot", "policy", "flows")):
                raise ValueError("this planner supports reweight/add/remove, not edits inside an existing tenant")
    t1 = spec["cycle"]
    width = (hw.num_vpifos - 1).bit_length()
    pack = lambda engine, flow: (engine << width) | flow
    allflows = lambda tree: {f["id"] for t in tree for f in t["flows"]}
    arriving = tuple(sorted(allflows(new) - allflows(old)))
    departing = tuple(sorted(allflows(old) - allflows(new)))
    removed = [t for t in old if t["name"] not in {n["name"] for n in new}]
    if len(removed) > 1:
        raise ValueError("this evaluation planner records one departing-tenant drain")
    commit = Command("CommitMapper", 1, 0, 0, 0)

    def cmd(name, engine, port=0, flow=0, data=0):
        return Command(name, engine, port, flow, data)

    def mappings(tree, placement, *, pre=True, token_engines=None):
        root, tenants, leaves = placement
        token_engines = token_engines or placement
        commands = []
        for tenant in tree:
            slot = tenant["slot"]
            for flow in tenant["flows"]:
                fid = flow["id"]
                for i, (engine, port, target) in enumerate((
                    (root, 1, pack(tenants, slot)),
                    (tenants, slot, pack(leaves, fid)),
                    (leaves, fid, fid),
                )):
                    if pre:
                        commands.append(cmd("UpdateMapperPre", engine, fid, data=port))
                    commands.append(cmd("UpdateMapperPost", engine, port,
                                        pack(token_engines[i], fid), target))
        return commands

    def configure(tree, placement, *, root=True):
        r, t, leaf = placement
        commands = [cmd("UpdateBrainEngine", r, 1, data=4)] if root else []
        for tenant in tree:
            slot = tenant["slot"]
            weight = tenant["weight"]
            if weight <= 0 or 120 % weight:
                raise ValueError("root weights must be positive divisors of 120")
            commands += [cmd("UpdateRankQuantum", r, 1, slot, 120 // weight),
                         cmd("UpdateBrainEngine", t, slot, data=2 if tenant["policy"] == "SP" else 4)]
            for position, flow in enumerate(tenant["flows"]):
                fid = flow["id"]
                commands += [cmd("UpdateRankGroup", r, 1, fid, slot),
                             cmd("UpdateBrainEngine", leaf, fid, data=3)]
                if tenant["policy"] == "SP":
                    commands.append(cmd("UpdateBrainFlowState", t, slot, pack(t, fid), position + 1))
                else:
                    quantum = 120 // flow.get("weight", 1)
                    commands += [cmd("UpdateRankGroup", t, slot, fid, fid),
                                 cmd("UpdateRankQuantum", t, slot, fid, quantum)]
        return commands + mappings(tree, placement)

    def clear_inputs(tree, placement):
        return [cmd("UpdateMapperPre", engine, fid) for engine in placement
                for fid in sorted(allflows(tree))]

    def retirement(tree, placement):
        r, t, leaf = placement
        commands = []
        for tenant in tree:
            commands.append(cmd("GuardDrain", t, tenant["slot"]))
            commands += [cmd("GuardDrain", leaf, flow["id"]) for flow in tenant["flows"]]
            for flow in tenant["flows"]:
                fid = flow["id"]
                commands += [cmd("UpdateMapperPost", r, 1, pack(r, fid)),
                             cmd("UpdateMapperPost", t, tenant["slot"], pack(t, fid)),
                             cmd("UpdateMapperPost", leaf, fid, pack(leaf, fid)),
                             cmd("UpdateBrainEngine", leaf, fid),
                             cmd("UpdateBrainState", leaf, fid),
                             cmd("UpdateMapperPre", leaf, fid),
                             cmd("UpdateRankGroup", r, 1, fid)]
            commands.append(cmd("UpdateBrainEngine", t, tenant["slot"]))
            commands.append(cmd("UpdateRankQuantum", r, 1, tenant["slot"]))
        return commands

    def retire_tree(tree, placement, token_engines):
        root, tenants, leaves = placement
        nodes = [(root, 1)] + [(tenants, t["slot"]) for t in tree]
        nodes += [(leaves, f) for f in sorted(allflows(tree))]
        commands = [cmd("GuardDrain", engine, port) for engine, port in nodes]
        commands += [replace(c, data=0) for c in mappings(tree, placement, pre=False, token_engines=token_engines)]
        commands += [cmd(kind, engine, port) for engine, port in nodes
                     for kind in ("UpdateBrainEngine", "UpdateBrainState")]
        return commands

    initial_tree = new if mechanism == "control-p2" else old
    initial = TimedTransaction(None, "initial", tuple(configure(initial_tree, (1, 2, 3)) + [commit]))
    transactions = []
    report = {"mechanism": mechanism, "experiment_rules": "shared-replay-guarded-cleanup-v1",
              "unguarded_edits": [], "guarded_edits": [],
              "copy_list": [], "modelled_teardown_cycles": 0, "modelled_install_cycles": 0}
    if mechanism == "control":
        report["unadmitted_flows"] = list(arriving)
    elif mechanism == "control-p2":
        report["unadmitted_flows"] = list(departing)
    elif mechanism == "rio":
        commands = []
        previous = {t["name"]: t for t in old}
        for tenant in new:
            prior = previous.get(tenant["name"])
            if prior is None:
                commands += configure([tenant], (1, 2, 3), root=False)
                report["unguarded_edits"].append(f"Add {tenant['name']}")
            elif prior["weight"] != tenant["weight"]:
                commands.append(cmd("UpdateRankQuantum", 1, 1, tenant["slot"], 120 // tenant["weight"]))
                report["unguarded_edits"].append(f"ChangeMeta {tenant['name']} -> {tenant['weight']}")
        commands += clear_inputs(removed, (1, 2, 3))
        report["unguarded_edits"] += [f"Quiesce {t['name']}" for t in removed]
        drain = (2, removed[0]["slot"]) if removed else None
        transactions.append(TimedTransaction(t1, "request", tuple(commands + [commit]),
                            drain_root=drain, gated_flow_ids=arriving))
        cleanup = retirement(removed, (1, 2, 3))
        if cleanup:
            transactions.append(TimedTransaction(t1, "retire", tuple(cleanup + [commit]), cleanup_of="request"))
            report["guarded_edits"] = [f"Empty {t['name']} -> Remove" for t in removed]
    elif mechanism in {"prefill", "relocate"}:
        commands = [cmd("StopWorld", 1, 1)]
        commands += clear_inputs(old, (1, 2, 3))
        if mechanism == "prefill":
            old_place, new_place, wrapper = (1, 2, 3), (4, 5, 6), 7
        else:
            old_place, new_place, wrapper = (4, 5, 6), (2, 3, 7), 1
            report["copy_list"] = [[3, 6], [2, 5], [1, 4]]
            commands += [cmd("CopyPifoEngine", source, data=target)
                         for source, target in report["copy_list"]]
            commands += mappings(old, old_place, pre=False, token_engines=(1, 2, 3))
        commands += configure(new, new_place)
        commands.append(cmd("UpdateBrainEngine", wrapper, 1, data=2))
        for fid in sorted(allflows(new)):
            token = pack(wrapper, fid)
            commands += [cmd("UpdateBrainFlowState", wrapper, 1, token, 2),
                         cmd("UpdateMapperPre", wrapper, fid, data=1),
                         cmd("UpdateMapperPost", wrapper, 1, token, pack(new_place[0], 1))]
        sentinel = pack(wrapper, hw.num_vpifos - 1)
        commands += [cmd("UpdateMapperPost", wrapper, 1, sentinel, pack(old_place[0], 1)),
                     cmd("PrefillPifo", wrapper, 1, sentinel),
                     cmd("UpdateRoot", wrapper, 1), commit]
        transactions.append(TimedTransaction(t1, "request", tuple(commands), "stop_the_world_pop",
                            drain_root=(old_place[0], 1), gated_flow_ids=arriving))
        # Removing the wrapper requires no token edits: all remaining wrapper
        # entries represent new-tree tokens, so switching root discards that
        # redundant level. Reclaim its unreachable tokens after the publication
        # and bank synchronization have allowed outstanding traversals to finish.
        cleanup = retire_tree(old, old_place, (1, 2, 3))
        cleanup += [cmd("UpdateMapperPre", wrapper, fid) for fid in sorted(allflows(new))]
        cleanup += [cmd("UpdateRoot", new_place[0], 1), commit]
        transactions.append(TimedTransaction(t1, "collapse", tuple(cleanup), cleanup_of="request"))
        reclaim = [cmd("ClearPifoEngine", wrapper)]
        reclaim += [replace(c, data=0) for c in commands if c.engine_id == wrapper and
                    c.command in {"UpdateMapperPost", "UpdateBrainEngine", "UpdateBrainFlowState"}]
        transactions.append(TimedTransaction(t1, "reclaim", tuple(reclaim + [commit]), cleanup_of="collapse"))
    else:
        # Generous lossless reset baseline: replay all retained packets. The
        # departed tenant needs a temporary drain-only arm until its packets exit.
        replay_tree = new + removed
        commands = configure(replay_tree, (1, 2, 3)) + [commit]
        cost = spec["reset_cost"]
        report["modelled_teardown_cycles"] = cost["teardown_cycles"]
        report["modelled_install_cycles"] = cost["install_cycles"]
        transactions.append(TimedTransaction(t1, "request", tuple(commands), "stop_the_world",
                            gated_flow_ids=arriving,
                            minimum_stop_cycles=sum(cost.values())))
        transactions.append(TimedTransaction(t1, "retire", tuple(retirement(removed, (1, 2, 3)) + [commit]),
                                            cleanup_of="request"))
    # Immediate commands need no replay. Hoist them before the first banked
    # write so each epoch fits the default 256-entry shared command FIFO.
    # Stop/copy/guard order stays stable; prefill and publication stay at the end.
    def replay_order(transaction):
        banked = {"UpdateMapperPre", "UpdateMapperPost"}
        tail = {"PrefillPifo", "UpdateRoot", "CommitMapper"}
        commands = transaction.commands
        ordered = ([c for c in commands if c.command not in banked | tail] +
                   [c for c in commands if c.command in banked] +
                   [c for c in commands if c.command in tail])
        return replace(transaction, commands=tuple(ordered))
    initial = replay_order(initial)
    transactions = [replay_order(t) for t in transactions]
    report["instruction_counts"] = {t.name: len(t.commands) for t in transactions}
    report["eligible_at_t1_instructions"] = len(transactions[0].commands) if transactions else 0
    report["guarded_instructions"] = sum(len(t.commands) for t in transactions[1:])
    return TransactionProgram(hw, 1, 1, initial, tuple(transactions)), report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("request", type=Path)
    parser.add_argument("--mechanism", choices=MECHANISMS, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    program, report = compile_request(json.loads(args.request.read_text()), args.mechanism)
    write_transaction_program(args.output, program)
    args.output.with_suffix(".plan.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    main()
