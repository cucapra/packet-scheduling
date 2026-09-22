#!/usr/bin/env python3
"""Add standalone plot scripts and packet traces without rerunning or redrawing saved results."""

from __future__ import annotations

import argparse
import csv
import json
import re
from pathlib import Path

from pifo_figures.common import (
    PACKET_TRACE_FIELDS, FigurePaths, PolicyEvent, figure_paths, flow_name, read_policy_event,
    read_run_packet_outcomes, write_packet_outcomes,
)
from pifo_figures.standalone import write_plot_script
from pifo_motivation_common import CASE_TITLES, DEFAULT_OUTPUT_ROOT, load_settings
from pifo_motivation_plot import write_comparison_packets


def _rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="", encoding="utf-8-sig") as source:
        return list(csv.DictReader(source))


def _run_packets(root: Path, labels: dict[int, str]):
    packets = read_run_packet_outcomes(root / "request-results.csv")
    if not (root / "packet-outcomes.csv").exists():
        write_packet_outcomes(root / "packet-outcomes.csv", packets, labels)
    return packets


def _export_legacy_packets(root: Path, labels: dict[int, str]) -> None:
    """Never borrow packet sizes/times from a newer run stored beside archived figures."""
    rows = _rows(root / "packet-times.csv")
    sizes = {}
    if (root / "request-results.csv").exists():
        packets = _run_packets(root, labels)
        archived = {(int(r["request_id"]), int(r["flow_id"]), int(r["input_cycle"]), int(r["output_cycle"]))
                    for r in rows}
        current = {(p.request_id, p.flow_id, p.push_cycle, p.pop_cycle) for p in packets}
        if archived == current:
            sizes = {p.request_id: p.size_bytes for p in packets}
    with (root / "rr-to-sp-packets.csv").open("w", newline="", encoding="utf-8") as destination:
        writer = csv.DictWriter(destination, fieldnames=PACKET_TRACE_FIELDS, lineterminator="\n")
        writer.writeheader()
        for row in rows:
            request, flow = int(row["request_id"]), int(row["flow_id"])
            push, pop = int(row["input_cycle"]), int(row["output_cycle"])
            writer.writerow(dict(request_id=request, flow=flow, flow_name=flow_name(flow, labels),
                                 size_bytes=sizes.get(request, ""), push_cycle=push, pop_cycle=pop,
                                 delay_cycles=pop - push, dropped="false"))


def export_saved_figures(results_root: Path) -> list[Path]:
    scripts = []
    settings = load_settings()
    for data in sorted(results_root.glob("**/figures/*/data.csv")):
        run_root = data.parents[2]
        event = read_policy_event(run_root / "reconfiguration-events.csv")
        config = run_root / "experiment-config.json"
        plot = json.loads(config.read_text())["plot"] if config.exists() else settings
        labels = {int(flow): name for flow, name in plot["flow_labels"].items()}
        write_packet_outcomes(data.parent / "packets.csv", _run_packets(run_root, labels), labels)
        rows = _rows(data)
        heading = CASE_TITLES.get(run_root.name, event.label)
        if "total_link_fraction" in rows[0]:
            kind = "bandwidth"
            window = int(rows[0]["window_end_cycle"]) - int(rows[0]["window_start_cycle"])
            title = f"{heading}: Hann-smoothed output bandwidth ({event.mode}, {window}-cycle window)"
        elif "input_relative_to_start" in rows[0]:
            kind = "packet-scatter"
            title = f"Packet input–output scatter: {event.label} ({event.mode})"
        else:
            kind, title = "delay", heading
        scripts.append(write_plot_script(figure_paths(data.parent), kind, [(heading, event)],
                                         labels, plot["dpi"], title))

    motivation = results_root / "motivating-example"
    for data in sorted(motivation.glob("comparisons/*/data.csv")):
        names = list(dict.fromkeys(row["run"] for row in _rows(data)))
        cases = {title: case for case, title in CASE_TITLES.items()}
        panels = [(name, read_policy_event(motivation / cases[name] / "reconfiguration-events.csv"))
                  for name in names]
        paths = figure_paths(data.parent)
        runs = [(name, _run_packets(motivation / cases[name], settings["flow_labels"]), event)
                for name, event in panels]
        write_comparison_packets(paths.packets, runs, settings["flow_labels"])
        if "delay" in data.parent.name:
            kind, data_name = "delay", "packets.csv"
            title = "R2–R4 packet-delay comparison (shared axes)"
        else:
            kind, data_name = "throughput-comparison", "data.csv"
            title = ("R3 whole-tree vs R4 confined throughput "
                     f"({settings['bandwidth_window_cycles']}-cycle Hann window)")
        scripts.append(write_plot_script(paths, kind, panels, settings["flow_labels"],
                                         settings["dpi"], title, data_name=data_name))

    # Older results put two figures directly in the run folder. Keep those
    # filenames/data untouched and give each its own distinctly named script.
    for config in sorted(results_root.glob("*/experiment-config.json")):
        root = config.parent
        plot = json.loads(config.read_text())["plot"]
        labels = {int(flow): name for flow, name in plot["flow_labels"].items()}
        if (root / "packet-times.csv").exists():
            _export_legacy_packets(root, labels)
        for kind, data_name, stem in (
            ("bandwidth", "bandwidth.csv", "rr-to-sp-bandwidth"),
            ("packet-scatter", "packet-times.csv", "rr-to-sp-packet-scatter"),
        ):
            paths = FigurePaths(root / data_name, root / f"{stem}.svg", root / f"{stem}.png")
            if not paths.data.exists() or not paths.svg.exists():
                continue
            # Some archived figures predate the adjacent event CSV. Their own
            # printed timestamps are authoritative for that archived dataset.
            match = re.search(r"start=(\d+)\s+commit=(\d+)\s+drain=(\d+|-)\s+finish=(\d+)"
                              r"(?:\s+config=(\d+) inst)?", paths.svg.read_text())
            if match is None:
                raise ValueError(f"{paths.svg}: cannot recover archived figure timestamps")
            start, commit, drain, finish, count = match.groups()
            current = read_policy_event(root / "reconfiguration-events.csv")
            # An archive has no second-commit timestamps. Do not accidentally
            # attach cleanup metadata from the newer run beside it.
            event = PolicyEvent(current.before, current.after, int(start), int(start), int(commit), int(finish),
                                name=current.name, mode=current.mode,
                                drain_cycle=None if drain == "-" else int(drain),
                                instruction_count=int(count) if count else None)
            title = f"{event.label}: saved bandwidth samples" if kind == "bandwidth" else f"Packet input–output scatter: {event.label}"
            scripts.append(write_plot_script(paths, kind, [(event.label, event)], labels, plot["dpi"], title,
                                             packet_trace_name="rr-to-sp-packets.csv"))
    return scripts


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results-root", type=Path, default=DEFAULT_OUTPUT_ROOT.parent)
    args = parser.parse_args()
    for path in export_saved_figures(args.results_root.resolve()):
        print(path)


if __name__ == "__main__":
    main()
