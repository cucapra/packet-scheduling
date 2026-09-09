#!/usr/bin/env python3
"""Check bounded CAM depths and value-RAM ports in generated RTL, without synthesis."""
import argparse
import hashlib
import json
from pathlib import Path
import re


def inspect(build: Path) -> dict:
    manifest = json.loads((build / "manifest.json").read_text())
    hardware = manifest["hardware"]
    assert manifest["rtl_complete"] and hardware["lookup_backend"] == "cam"
    for filename, expected in manifest["rtl_sha256"].items():
        assert hashlib.sha256((build / "rtl" / filename).read_bytes()).hexdigest() == expected, filename
    text = (build / "rtl" / "PifoMesh.v").read_text()
    modules = dict(re.findall(r"(?ms)^module\s+(\w+)\s*\((.*?)^endmodule", text))
    definitions = {}
    capacity = hardware["cam_entries_per_bank"]
    for name, body in modules.items():
        if not name.startswith("BoundedCamMapper"):
            continue
        memories = []
        for high, memory, last in re.findall(r"reg\s*\[(\d+):0\]\s+(banks_\d+_values)\s*\[0:(\d+)\];", body):
            reads = len(re.findall(r"<=\s*" + memory + r"\[", body))
            writes = len(re.findall(memory + r"\[[^\]\n]+\]\s*<=", body))
            assert int(last) + 1 == capacity, (name, memory, last)
            assert reads == writes == 1, (name, memory, reads, writes)
            memories.append({"name": memory, "width": int(high) + 1,
                             "depth": capacity, "read_ports": reads, "write_ports": writes})
        assert memories, name
        if len(memories) == 2:
            assert '((io_writeReq_valid && activeBank) && banks_0_canWrite)' in body
            assert 'assign banks_1_writeEnabled = (! activeBank);' in body
            assert '((io_writeReq_valid && banks_1_writeEnabled) && banks_1_canWrite)' in body
        definitions[name] = memories
    instances = re.findall(r"(?m)^\s*(BoundedCamMapper\w*)\s+(\w+)\s*\(", text)
    assert sum(instance == "engineCAM" for _, instance in instances) == hardware["num_engines"]
    assert sum(instance == "deque_dequeMapper" for _, instance in instances) == hardware["num_engines"]
    value_bits = 0
    value_banks = 0
    for name, instance in instances:
        banks = definitions[name]
        expected_banks = hardware["mapper_banks"] if instance == "deque_dequeMapper" else 1
        assert len(banks) == expected_banks, (name, instance, banks)
        value_bits += sum(bank["width"] * bank["depth"] for bank in banks)
        value_banks += len(banks)
    logical = manifest["logical_storage_bits"]
    assert value_bits == logical["flow_state_tables"] + logical["post_mapper_banks"]
    # Both formerly dense pair tables must be gone, including through any
    # accidentally retained direct mapper elsewhere in the hierarchy.
    all_depths = [int(last) + 1 for last in re.findall(r"reg\s*\[\d+:0\]\s+\w+\s*\[0:(\d+)\];", text)]
    assert hardware["vpifo_token_pairs_per_pe"] not in all_depths
    return {"status": "passed", "build": str(build.resolve()), "configuration": hardware["configuration"],
            "capacity_per_bank": capacity, "cam_instances": len(instances), "cam_value_banks": value_banks,
            "cam_value_bits": value_bits, "cam_tag_valid_register_bits": logical["cam_tag_valid_registers"],
            "replay_bank_writes_mutually_exclusive": hardware["configuration"] == "replay",
            "value_ram_definitions": definitions, "maximum_declared_ram_depth": max(all_depths),
            "rtl_sha256": manifest["rtl_sha256"]["PifoMesh.v"], "synthesis_run": False,
            "physical_resource_counts_measured": False}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build", type=Path, nargs="+")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    results = [inspect(build) for build in args.build]
    content = json.dumps({"status": "passed", "builds": results}, indent=2) + "\n"
    if args.output:
        args.output.write_text(content)
    else:
        print(content, end="")
