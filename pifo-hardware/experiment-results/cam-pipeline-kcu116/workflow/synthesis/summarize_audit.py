#!/usr/bin/env python3
"""Summarize flattened generic Yosys storage without claiming FPGA utilization."""

import argparse
from collections import Counter
import json
from pathlib import Path
import re


def summarize(netlist):
    if set(netlist["modules"]) != {"PifoMesh"}:
        raise ValueError("Expected the flattened PifoMesh produced by synthesis/audit.ys")
    module = netlist["modules"]["PifoMesh"]
    sorted_bits = {
        bit for name, net in module["netnames"].items()
        if re.fullmatch(r"pifoEngines_\d+\.pifos\.pifoArray_\d+_(port|priority|data)", name)
        for bit in net["bits"] if isinstance(bit, int)
    }
    registers = Counter()
    cells = Counter()
    memories = []
    ff_bits = set()
    for name, cell in module["cells"].items():
        kind = cell["type"]
        cells[kind] += 1
        if "dff" in kind:
            width = int(cell["parameters"]["WIDTH"], 2)
            registers[kind] += width
            ff_bits.update(bit for bit in cell["connections"]["Q"] if isinstance(bit, int))
        elif "latch" in kind or kind.startswith("$dlatch"):
            raise ValueError(f"Unexpected latch: {name}")
        elif kind == "$mem_v2":
            params = cell["parameters"]
            item = {"name": name, **{
                key.lower(): int(params[key], 2) for key in ("WIDTH", "SIZE", "RD_PORTS", "WR_PORTS")
            }}
            item["bits"] = item["width"] * item["size"]
            memories.append(item)
        elif not kind.startswith("$"):
            raise ValueError(f"Unexpected unmapped module/blackbox: {kind}")
    return {
        "source": netlist["creator"],
        "scope": "Flattened optimized generic RTL; not Agilex technology mapping",
        "explicit_flip_flop_bits": sum(registers.values()),
        "sorted_entry_flip_flop_bits": len(ff_bits & sorted_bits),
        "flip_flop_bits_by_cell_type": dict(sorted(registers.items())),
        "memory_bits": sum(item["bits"] for item in memories),
        "memory_count": len(memories),
        "memories": memories,
        "generic_cell_counts": dict(sorted(cells.items())),
        "agilex_alms": None, "agilex_m20ks": None, "fmax_mhz": None,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("netlist", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    result = summarize(json.loads(args.netlist.read_text()))
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({key: value for key, value in result.items()
                      if key not in ("memories", "generic_cell_counts")}, indent=2))


if __name__ == "__main__":
    main()
