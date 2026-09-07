#!/usr/bin/env python3
"""Reconcile R4 RAM components with the archived top-level synthesis counts."""
import argparse
import csv
import json
from pathlib import Path
import re

from pifo_hardware_overhead import PROJECT, count_text, write_csv


COMPONENTS = {
    "post_mapper": "Post-mapper banks, all PEs",
    "pre_mapper": "Pre-mapper banks, all PEs",
    "engine_cam": "Unbanked engineCAM tables, all PEs",
    "replay_log": "Shared instruction log",
    "other": "Other inferred memories",
}


def component(name, configuration):
    path = name.replace("|", "/")
    if "/deque_dequeMapper/" in path:
        return "post_mapper"
    if "/enque_enqueMapper/" in path:
        return "pre_mapper"
    if "/enque_brain/engineCAM/" in path:
        return "engine_cam"
    if configuration == "replay" and re.match(r"streamFifo_\d+/logic_ram", path):
        return "replay_log"
    return "other"


def collect(output):
    statuses = json.loads((output / "run-status.json").read_text())
    with (output / "resources.csv").open() as stream:
        resources = list(csv.DictReader(stream))
    totals = {(r["platform"], int(r["vflows"]), r["configuration"], r["resource"]):
              float(r["value"]) for r in resources}
    sources = {(r["platform"], int(r["vflows"]), r["configuration"]): Path(r["source"])
               for r in resources}
    rows = []
    for status in statuses:
        if status["status"] != "synthesis_complete":
            continue
        platform, flows, variant = status["platform"], status["vflows"], status["configuration"]
        source = sources[platform, flows, variant]
        summary = json.loads((output / source).read_text())
        metrics = {key: dict.fromkeys(COMPONENTS, 0) for key in (
            ["block_memory_bits"] if platform == "quartus" else
            ["bram36_tiles", "uram288_blocks", "bram_uram_allocated_bits"])}
        if platform == "quartus":
            table = next(table for name, table in summary["tables"].items()
                         if "RAM Summary" in name)
            for cells in table[1:]:
                ram = dict(zip(table[0], cells))
                group = component(ram["Name"], variant)
                metrics["block_memory_bits"][group] += int(ram["Implementation Bits"])
        else:
            for ram in summary["ram_instances"]:
                group = component(ram["Memory Name"], variant)
                primitive = ram["Primitive"]
                if primitive.startswith("RAMB36"):
                    metrics["bram36_tiles"][group] += 1
                elif primitive.startswith("RAMB18"):
                    metrics["bram36_tiles"][group] += 0.5
                elif primitive.startswith("URAM"):
                    metrics["uram288_blocks"][group] += 1
                else:
                    raise ValueError(f"Unexpected block RAM primitive: {primitive}")
                metrics["bram_uram_allocated_bits"][group] += ram["Available Bits"]
        for metric, groups in metrics.items():
            expected = totals[platform, flows, variant, metric]
            assert sum(groups.values()) == expected, (source, metric, groups, expected)
            if variant == "replay" and metric in ("block_memory_bits", "bram_uram_allocated_bits"):
                assert groups["replay_log"] > 0, (source, "Missing journal RAM")
            for group, value in groups.items():
                rows.append(dict(platform=platform, vflows=flows, configuration=variant,
                                 resource=metric, component=group, value=value, source=str(source)))
    write_csv(output / "memory-breakdown.csv", rows,
              ["platform", "vflows", "configuration", "resource", "component", "value", "source"])
    return rows, statuses


def render(output, rows, statuses):
    lines = ["# Replay RAM breakdown", "",
             "All values come from completed synthesis reports with PIFO cores excluded. "
             "Components sum exactly to each run's reported top-level RAM count. "
             "The CSV includes every completed R4 point; the tables below show 1,024 IDs.", ""]
    for platform, metric, unit in [("quartus", "block_memory_bits", "mapped block-memory bits"),
                                   ("vivado", "bram36_tiles", "BRAM36 tile equivalents")]:
        values = {(r["configuration"], r["component"]): r["value"] for r in rows
                  if r["platform"] == platform and r["vflows"] == 1024 and r["resource"] == metric}
        if not values:
            continue
        lines += [f"## {platform.capitalize()}: {unit}", "",
                  "| Component | Ordinary | Read/copy | Replay |",
                  "|---|---:|---:|---:|"]
        for group, label in COMPONENTS.items():
            cells = [count_text(values[variant, group]) if (variant, group) in values else "Incomplete"
                     for variant in ["static", "dynamic", "replay"]]
            lines.append(f'| {label} | {" | ".join(cells)} |')
        lines += [""]
    lines += ["Quartus counts inferred implementation bits, including width pruning and RAM "
              "replicas; these are not a fitted M20K allocation. Vivado counts mapped block "
              "RAM primitives (RAMB18 counts as half a tile); allocated bits and URAM counts "
              "are also in the CSV. LUT RAM is outside this block-memory breakdown.", "",
              "The replay log is included in the totals. Rebuilt hierarchy can assign shared "
              "logic to the FIFO that drives it, so hierarchical LUT counts are not used here "
              "as standalone controller costs. Whole-design LUT differences remain the logic "
              "comparison.", ""]
    missing = [s for s in statuses if s["status"] != "synthesis_complete"]
    if missing:
        lines += ["Incomplete measurements:", ""]
        lines += [f'- {s["platform"]}, {s["vflows"]} IDs, {s["configuration"]}: `{s["status"]}`.'
                  for s in missing]
        lines += [""]
    (output / "memory-breakdown.md").write_text("\n".join(lines))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path,
                        default=PROJECT / "experiment-results/hardware-overhead/r4-replay")
    args = parser.parse_args()
    rows, statuses = collect(args.output)
    render(args.output, rows, statuses)
    print(f"Reconciled {len(rows)} component/resource rows with archived synthesis totals.")
