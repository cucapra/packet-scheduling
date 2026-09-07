#!/usr/bin/env python3
"""Extract resource tables from a completed PIFO Vivado synthesis build/archive."""

import argparse
import csv
import json
from pathlib import Path
import re
import sys


def number(value):
    value = value.replace(",", "")
    try:
        return int(value)
    except ValueError:
        try:
            return float(value)
        except ValueError:
            return value


def utilization(text):
    resources = {}
    header = None
    for line in text.splitlines():
        if not line.strip().startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if cells[0] == "Site Type" and "Used" in cells:
            header = cells
        elif header and len(cells) == len(header) and isinstance(number(cells[1]), (int, float)):
            resources[cells[0].rstrip("*").strip()] = {
                key: number(value) for key, value in zip(header[1:], cells[1:])
            }
    if not any(key in resources for key in ("CLB LUTs", "Slice LUTs")):
        raise ValueError("No total LUT resource row in Vivado report")
    return resources


def hierarchy(text):
    rows = []
    header = None
    stack = []
    for line in text.splitlines():
        if not line.strip().startswith("|"):
            continue
        raw = line.strip().strip("|").split("|")
        cells = [cell.strip() for cell in raw]
        if cells[0] == "Instance" and "Module" in cells:
            header = cells
        elif header and len(cells) == len(header):
            indent = len(raw[0]) - len(raw[0].lstrip())
            while stack and stack[-1][0] >= indent:
                stack.pop()
            path = "/".join([entry[1] for entry in stack] + [cells[0]])
            stack.append((indent, cells[0]))
            rows.append({"path": path, **{
                key: number(value) if index > 1 else value
                for index, (key, value) in enumerate(zip(header, cells))
            }})
    if not rows:
        raise ValueError("No hierarchy rows in Vivado report")
    return rows


def ram_utilization(text):
    rows = []
    header = None
    for line in text.splitlines():
        if not line.strip().startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if cells[0] == "Memory Name":
            header = cells if "Available Bits" in cells and "Used Bits" in cells else None
        elif header and len(cells) == len(header) and cells[1]:
            rows.append({key: number(value) for key, value in zip(header, cells)})
    return rows


def summarize(build):
    manifest = json.loads((build / "manifest.json").read_text())
    if manifest.get("tool") != "vivado" or manifest.get("status") != "synthesis_complete":
        raise ValueError(f"Not a completed Vivado synthesis: {manifest.get('status', 'status missing')}")
    marker = (build / "synthesis_complete.txt").read_text().strip()
    log = (build / "synthesis.log").read_text(errors="replace")
    if (manifest["part"] not in marker or "PifoMesh" not in marker
            or not re.search(r"^PIFO_SYNTHESIS_COMPLETE:", log, flags=re.MULTILINE)
            or re.search(r"^ERROR:", log, flags=re.MULTILINE)):
        raise ValueError("Missing or inconsistent Vivado completion evidence")
    reports = build / "reports"
    report_text = (reports / "utilization.rpt").read_text()
    if not re.search(r"Design State\s*:\s*Synthesized", report_text):
        raise ValueError("Expected a synthesized design utilization report")
    resources = utilization(report_text)
    rows = hierarchy((reports / "utilization-hierarchy.rpt").read_text())
    # Require the RAM report too: the completion marker follows its successful generation.
    memories = ram_utilization((reports / "ram-utilization.rpt").read_text())
    with (reports / "primitive-counts.tsv").open() as stream:
        primitives = {row["primitive"]: int(row["count"])
                      for row in csv.DictReader(stream, delimiter="\t")}
    if not primitives:
        raise ValueError("Missing synthesized primitive counts")
    count = lambda prefix: sum(value for key, value in primitives.items() if key.startswith(prefix))
    ram36 = count("RAMB36")
    ram18 = count("RAMB18")
    return {
        "source_build": str(build), "status": manifest["status"],
        "part": manifest["part"], "tool_version": manifest["tool_version"],
        "synthesis_directive": manifest.get("vivado_directive", "default"),
        "hardware": manifest["hardware"], "completion": marker,
        "scope": "Vivado out-of-context synthesis; no implementation or timing-closure claim",
        "resources": resources, "hierarchy": rows, "primitive_counts": primitives,
        "ram_instances": memories,
        "mapped_memory": {
            "ramb36_primitives": ram36, "ramb18_primitives": ram18,
            "bram36_tile_equivalents": ram36 + ram18 / 2,
            "bram_allocated_capacity_bits_including_parity": ram36 * 36864 + ram18 * 18432,
            "uram_primitives": count("URAM"),
            "reported_bram_uram_used_bits_including_replicas": sum(row["Used Bits"] for row in memories),
            "reported_bram_uram_allocated_bits": sum(row["Available Bits"] for row in memories),
        },
        "flip_flop_primitives": count("FD"), "latch_primitives": count("LD"),
        "dsp_primitives": count("DSP"), "unresolved_blackboxes": 0,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build", type=Path, help="Completed build or archived snapshot directory")
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    result = summarize(args.build)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(result["part"])
    for name in ("CLB LUTs", "Slice LUTs", "LUT as Logic", "LUT as Memory",
                 "CLB Registers", "Slice Registers", "Block RAM Tile", "URAM", "DSPs"):
        if name in result["resources"]:
            print(f"  {name}: {result['resources'][name]}")
    print("  Mapped memory:", result["mapped_memory"])
    print("  Latches:", result["latch_primitives"])


if __name__ == "__main__":
    try:
        main()
    except (ValueError, OSError, KeyError) as error:
        sys.exit(str(error))
