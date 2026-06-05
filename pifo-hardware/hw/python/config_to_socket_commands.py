#!/usr/bin/env python3
import argparse
import json
import socket
import sys
from pathlib import Path


DEFAULT_SOCKET = "/tmp/rio-control.sock"

POLICY_TO_BRAIN = {
    "WFQ": 1,
    "RR": 1,
    "SP": 2,
    "FIFO": 3,
    "UNION": 3,
}


def parse_args():
    parser = argparse.ArgumentParser(
        description="Convert tree config JSON from stdin into PIFO control socket commands."
    )
    parser.add_argument(
        "--mapping",
        type=Path,
        default=Path("hw/python/pifo_node_mapping.json"),
        help="JSON mapping file to load if present and update with new v -> engine/vpifo assignments.",
    )
    parser.add_argument("--num-engines", type=int, default=2)
    parser.add_argument("--num-vpifos", type=int, default=32)
    parser.add_argument("--first-vpifo", type=int, default=10)
    parser.add_argument(
        "--socket",
        type=Path,
        default=Path(DEFAULT_SOCKET),
        help=f"Unix domain socket to send commands to. Defaults to {DEFAULT_SOCKET}.",
    )
    parser.add_argument(
        "--try-run",
        action="store_true",
        help="Print generated commands to stdout instead of sending them to the socket.",
    )
    return parser.parse_args()


def load_mapping(path, first_vpifo):
    if not path.exists():
        return {
            "version": 1,
            "nextVPifo": first_vpifo,
            "nodes": {},
            "flows": {},
        }

    with path.open() as f:
        data = json.load(f)
    data.setdefault("nextVPifo", first_vpifo)
    data.setdefault("nodes", {})
    data.setdefault("flows", data.pop("classes", {}))
    return data


def save_mapping(path, mapping):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w") as f:
        json.dump(mapping, f, indent=2, sort_keys=True)
        f.write("\n")


def engine_from_pe(pe, num_engines):
    if pe is None or pe <= 0:
        return 1
    return ((pe - 1) % num_engines) + 1


def allocate_node(mapping, v, pe, args):
    key = str(v)
    if key in mapping["nodes"]:
        return mapping["nodes"][key]

    vpifo = mapping["nextVPifo"]
    if vpifo >= args.num_vpifos:
        raise ValueError(f"ran out of vPifo IDs while allocating v={v}")

    node = {
        "engineId": engine_from_pe(pe, args.num_engines),
        "vPifoId": vpifo,
        "pe": pe,
    }
    mapping["nodes"][key] = node
    mapping["nextVPifo"] = vpifo + 1
    return node


def node(mapping, v):
    key = str(v)
    if key not in mapping["nodes"]:
        raise ValueError(f"node v={v} was used before spawn")
    return mapping["nodes"][key]


def raw_flow_id(mapping, name, args):
    if name not in mapping["flows"]:
        mapping["flows"][name] = len(mapping["flows"]) + 1
    raw_id = mapping["flows"][name]
    if raw_id >= args.num_vpifos:
        raise ValueError(f"flow {name} maps to raw flow ID {raw_id}, outside vpifo width")
    return raw_id


def pack_flow_id(engine_id, vpifo_or_flow_id, args):
    return (engine_id << (args.num_vpifos - 1).bit_length()) | vpifo_or_flow_id


def command(name, engine_id, vpifo_id=0, flow_id=0, data=0):
    return (
        f"command={name} "
        f"engineId={engine_id} "
        f"vPifoId={vpifo_id} "
        f"flowId={flow_id} "
        f"data={data}"
    )


def index_adoptions(transaction):
    by_step = {}
    for op in transaction:
        if op.get("op") == "adopt":
            by_step[op["step"]] = (op["parent"], op["child"])
    return by_step


def emit_transaction(transaction, index, mapping, args):
    lines = [f"# transaction {index} begin"]
    adoptions = index_adoptions(transaction)

    for op in transaction:
        kind = op.get("op")

        if kind == "spawn":
            allocated = allocate_node(mapping, op["v"], op.get("pe"), args)
            lines.append(
                f"# spawn v={op['v']} -> engineId={allocated['engineId']} vPifoId={allocated['vPifoId']}"
            )

        elif kind == "assoc":
            current = node(mapping, op["v"])
            flow = raw_flow_id(mapping, op["class"], args)
            lines.append(f"# flow {op['class']} -> rawFlowId={flow}")
            lines.append(
                command(
                    "UpdateMapperPre",
                    current["engineId"],
                    vpifo_id=flow,
                    data=current["vPifoId"],
                )
            )

        elif kind == "map":
            current = node(mapping, op["v"])
            if op["step"] not in adoptions:
                lines.append(f"# skip map without adopt step={op['step']}: {json.dumps(op, sort_keys=True)}")
                continue

            _, child_v = adoptions[op["step"]]
            child = node(mapping, child_v)
            flow = raw_flow_id(mapping, op["class"], args)
            lines.append(
                command(
                    "UpdateMapperPost",
                    current["engineId"],
                    flow_id=pack_flow_id(current["engineId"], flow, args),
                    data=pack_flow_id(child["engineId"], child["vPifoId"], args),
                )
            )

        elif kind == "change_pol":
            current = node(mapping, op["v"])
            policy = op["pol"]
            if policy not in POLICY_TO_BRAIN:
                lines.append(f"# skip unsupported policy {policy}: {json.dumps(op, sort_keys=True)}")
                continue
            lines.append(
                command(
                    "UpdateBrainEngine",
                    current["engineId"],
                    vpifo_id=current["vPifoId"],
                    data=POLICY_TO_BRAIN[policy],
                )
            )

        elif kind == "change_weight":
            lines.append(f"# skip change_weight until flow-state rule is agreed: {json.dumps(op, sort_keys=True)}")

        elif kind == "adopt":
            lines.append(f"# adopt parent={op['parent']} child={op['child']} step={op['step']}")

        else:
            lines.append(f"# skip unknown op: {json.dumps(op, sort_keys=True)}")

    lines.append(command("CommitMapper", 1, 0, 0, 0))
    lines.append(f"# transaction {index} end")
    return lines


def send_to_socket(socket_path, lines):
    payload = "\n".join(lines) + "\n"
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
        sock.connect(str(socket_path))
        sock.sendall(payload.encode())


def main():
    args = parse_args()
    transactions = json.load(sys.stdin)
    if not isinstance(transactions, list) or any(not isinstance(tx, list) for tx in transactions):
        raise SystemExit("expected stdin JSON to be an array of transaction arrays")

    mapping = load_mapping(args.mapping, args.first_vpifo)
    output = []
    for index, transaction in enumerate(transactions):
        output.extend(emit_transaction(transaction, index, mapping, args))

    if args.try_run:
        print("\n".join(output))
    else:
        send_to_socket(args.socket, output)

    save_mapping(args.mapping, mapping)


if __name__ == "__main__":
    main()
