#!/usr/bin/env python3
"""Extract resource tables from a successful Quartus ASCII synthesis report."""

import argparse
import json
from pathlib import Path


def recover_memory_total(tables):
    """Recover an overflowing root total only when two independent sums agree.

    Quartus 25.3.1 omits the memory summary rows in the large read/copy case
    and prints INT32_MIN in the hierarchy root. Its per-RAM implementation
    sizes and direct-child hierarchy totals remain positive and consistent.
    Preserve the raw tables and never turn the missing MLAB field into zero.
    """
    usage = next((rows for name, rows in tables.items()
                  if name.startswith("Synthesis Resource Usage Summary")), [])
    values = {row[0]: row[1] for row in usage}
    if "Total block memory bits" in values:
        return None
    hierarchy = next((rows for name, rows in tables.items()
                      if "Resource Utilization by Entity" in name), [])
    if not hierarchy:
        return None
    entities = [dict(zip(hierarchy[0], row)) for row in hierarchy[1:]]
    root = next((row for row in entities if row["Full Hierarchy Name"] == "|"), None)
    if root is None or root.get("Block Memory Bits") != str(-(1 << 31)):
        return None
    ram_table = next(rows for name, rows in tables.items() if "RAM Summary" in name)
    rams = [dict(zip(ram_table[0], row)) for row in ram_table[1:]]
    children = [row for row in entities if row["Full Hierarchy Name"] != "|"
                and "|" not in row["Full Hierarchy Name"]]
    sizes = [int(row["Implementation Bits"]) for row in rams]
    child_sizes = [int(row["Block Memory Bits"]) for row in children]
    total = sum(sizes)
    if (not sizes or not child_sizes or min(sizes + child_sizes) < 0
            or total <= (1 << 31) - 1
            or total != sum(child_sizes)):
        raise ValueError("Missing Quartus memory total without consistent overflow evidence")
    return {
        "block_memory_bits": total,
        "method": "Sum of per-RAM Implementation Bits, independently matched to disjoint direct-child Block Memory Bits",
        "ram_instances": len(sizes), "direct_child_total": sum(child_sizes),
        "reported_root_total": int(root["Block Memory Bits"]),
        "note": "Quartus omits the aggregate memory rows and reports -2147483648 at the hierarchy root. The positive detailed counts reconcile exactly; raw tables are unchanged.",
    }


def summarize(report):
    tables = {}
    current = None
    for line in report.read_text().splitlines():
        line = line.strip()
        if not (line.startswith(";") and line.endswith(";")):
            continue
        cells = [cell.strip() for cell in line[1:-1].split(";")]
        if len(cells) == 1:
            current = cells[0]
            tables.setdefault(current, [])
        elif current:
            tables[current].append(cells)
    summary = tables.get("Synthesis Summary", [])
    status = next((row[1] for row in summary if row[0] == "Synthesis Status"), "")
    if not status.startswith("Successful"):
        raise ValueError(f"No successful synthesis in {report}: {status or 'status missing'}")
    selected = {name: rows for name, rows in tables.items()
                if "Resource" in name or "RAM Summary" in name or name == "Synthesis Summary"}
    if not any("Resource" in name for name in selected):
        raise ValueError(f"No resource tables in {report}")
    result = {"source_report": str(report), "status": status,
              "scope": "Quartus synthesis estimates; no placement or routing",
              "tables": selected}
    recovered = recover_memory_total(selected)
    if recovered:
        result["recovered_memory_total"] = recovered
        result["resource_notes"] = [recovered["note"]]
        usage = next(rows for name, rows in selected.items()
                     if name.startswith("Synthesis Resource Usage Summary"))
        if not any(row[0] == "Total MLAB memory bits" for row in usage):
            result["unreported_resources"] = ["mlab_memory_bits"]
            result["resource_notes"].append("MLAB memory usage is not reported for this run and is omitted, not treated as zero.")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("report", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    result = summarize(args.report)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    for name, rows in result["tables"].items():
        if "Summary" in name and "RAM" not in name:
            print(name)
            for row in rows:
                print("  " + " | ".join(row))


if __name__ == "__main__":
    main()
