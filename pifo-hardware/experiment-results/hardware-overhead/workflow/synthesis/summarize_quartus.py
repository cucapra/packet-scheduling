#!/usr/bin/env python3
"""Extract resource tables from a successful Quartus ASCII synthesis report."""

import argparse
import json
from pathlib import Path


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
    return {"source_report": str(report), "status": status,
            "scope": "Quartus synthesis estimates; no placement or routing",
            "tables": selected}


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
