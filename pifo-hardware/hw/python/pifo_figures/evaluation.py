"""Package evaluation figures as local CSVs plus one self-contained plot.py.

The generated plotters import only csv, pathlib and matplotlib. They render the
same files in the harness and when copied out of the repository.
"""
from __future__ import annotations

import csv
import subprocess
import sys
from pathlib import Path
from pprint import pformat


def read_csv(path):
    with Path(path).open(newline="") as source:
        return list(csv.DictReader(source))


def packet_rows(runs, labels):
    rows = []
    for run, path in runs.items():
        for p in read_csv(path / "packet-outcomes.csv"):
            rows.append({"run": run, **p, "flow_name": labels[str(p["flow"])],
                         "delay_cycles": int(p["pop_cycle"]) - int(p["push_cycle"]) if p["pop_cycle"] else "",
                         "status": "dropped" if p["dropped"] == "true" else "completed" if p["pop_cycle"] else "unadmitted"})
    return rows


def commit_rows(runs):
    rows = []
    for run, path in runs.items():
        source = path / "reconfiguration-events.csv"
        if not source.exists():
            continue
        events = read_csv(source)
        by_name = {e["name"]: e for e in events}
        for index, e in enumerate(events, 1):
            original = e
            while original.get("cleanup_of"):
                original = by_name[original["cleanup_of"]]
            rows.append({"run": run, "commit": f"C{index}", "name": e["name"],
                         "start_cycle": e["start_cycle"], "commit_cycle": e["commit_cycle"],
                         "ready_for_next_commit": e["install_finish_cycle"],
                         "old_tree_drained_cycle": original["drain_cycle"],
                         "drain_label": "old-tree captured (not drained)" if original["mode"] == "stop_the_world" else "old-tree-drained",
                         "commit_applied_cycle": e["commit_applied_cycle"],
                         "instruction_count": e["instruction_count"], "commit_cycles": e["commit_cycles"],
                         "bank_replay_cycles": e["bank_cleanup_cycles"]})
    return rows


def export(directory, kind, data, packets, commits, settings):
    directory.mkdir(parents=True, exist_ok=True)
    for name, rows in (("data", data), ("packets", packets), ("commits", commits)):
        with (directory / f"{name}.csv").open("w", newline="") as destination:
            fields = list(rows[0]) if rows else ["run"]
            writer = csv.DictWriter(destination, fieldnames=fields, lineterminator="\n")
            writer.writeheader()
            writer.writerows(rows)
    source = _HEADER + "\nSETTINGS = " + pformat(settings, sort_dicts=False) + "\n"
    if kind in {"zoom", "untouched"}:
        source += _TIMELINE
    source += _BODIES[kind] + _SAVE
    script = directory / "plot.py"
    script.write_text(source)
    subprocess.run([sys.executable, "-I", str(script.resolve())], check=True)


_HEADER = '''#!/usr/bin/env python3
"""Replot this folder: python plot.py. Needs only local CSV files and matplotlib.

packets.csv has every generated packet, including source-side waiting; blank pop
with status=unadmitted is a control flow without a policy arm, not a drop.
commits.csv records each commit's own replay-ready time, not a later drain wait.
"""
import csv
from pathlib import Path
import matplotlib
matplotlib.use("Agg")
from matplotlib import pyplot as plt
plt.rcdefaults()
HERE = Path(__file__).resolve().parent
def read(name):
    with (HERE / name).open(newline="") as stream:
        return list(csv.DictReader(stream))
rows = read("data.csv")
commits = read("commits.csv")
COLORS = ("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")
'''

_TIMELINE = '''
def timeline(axis, runs):
    backgrounds = ("#dbeafe", "#ffedd5", "#dcfce7")
    styles = (("start_cycle", "start", "#1f77b4", "--"),
              ("commit_cycle", "commit accepted", "#ff7f0e", "-."),
              ("ready_for_next_commit", "ready_for_next_commit", "#2ca02c", ":"),
              ("old_tree_drained_cycle", "old-tree-drained", "#9467bd", "-"))
    handles, labels, costs = [], [], []
    for row in commits:
        if row["run"] not in runs:
            continue
        name = row["commit"]
        prefix = ("Strict*" if row["run"] == "link" else "Reserved") + " " + name if len(runs) > 1 else name
        color = backgrounds[(int(name[1:]) - 1) % len(backgrounds)]
        shade = axis.axvspan(int(row["start_cycle"]), int(row["ready_for_next_commit"]),
                            color=color, alpha=.5, linewidth=0, zorder=0)
        handles.append(shade)
        labels.append(prefix + ": " + row["name"])
        for field, label, linecolor, style in styles:
            if not row[field]:
                continue
            cycle = int(row[field])
            if field == "old_tree_drained_cycle":
                label = row["drain_label"]
            line = axis.axvline(cycle, color=linecolor, linestyle=style,
                               linewidth=2 if field == "ready_for_next_commit" else 1, alpha=.8)
            handles.append(line)
            labels.append(f"{name} {label} = {cycle}")
        costs.append(f"{prefix}: {row['instruction_count']} inst / {row['commit_cycles']} cycles; "
                     f"bank replay {row['bank_replay_cycles']} cycles")
    series = axis.legend(loc="upper right", fontsize=8)
    axis.add_artist(series)
    if handles:
        key = axis.legend(handles, labels, loc="upper center", bbox_to_anchor=(.5, -.16),
                          ncol=max(1, len(costs)), fontsize=6.5)
        axis.annotate("\\n".join(costs), xy=(.5, 0), xycoords=key, xytext=(0, -6),
                      textcoords="offset points", ha="center", va="top", fontsize=7,
                      annotation_clip=False)
'''

_ZOOM = '''
fig, ax = plt.subplots(figsize=(13, 5.8), constrained_layout=True)
for index, run in enumerate(SETTINGS["runs"]):
    data = [r for r in rows if r["run"] == run]
    ax.scatter([int(r["push_cycle"]) for r in data], [int(r["delay_cycles"]) for r in data],
               s=10, alpha=.7, color=COLORS[index], label="zoom — " + SETTINGS["titles"][run])
timeline(ax, SETTINGS["runs"])
ax.set(xlabel="Packet generation / push cycle", ylabel="Per-packet delay (cycles)",
       title=SETTINGS["title"])
ax.grid(alpha=.2)
ax.margins(x=.01, y=.06)
'''

_STOP = '''
fig, ax = plt.subplots(figsize=(7.2, 4.7), constrained_layout=True)
for index, run in enumerate(SETTINGS["runs"]):
    data = [r for r in rows if r["run"] == run]
    x = [int(r["t1_backlog_packets"]) for r in data]
    y = [int(r["global_stop_cycles"]) for r in data]
    ax.plot(x, y, "o-", linewidth=1.8, color=COLORS[index], label=SETTINGS["titles"][run])
    if run == "reserved" and len(set(x)) > 1:
        mx, my = sum(x) / len(x), sum(y) / len(y)
        slope = sum((a - mx) * (b - my) for a, b in zip(x, y)) / sum((a - mx)**2 for a in x)
        ax.text(.03, .95, f"Measured fit: {slope:.3f} cycles/packet + {my - slope * mx:.1f} cycles",
                transform=ax.transAxes, va="top", fontsize=9)
ax.set(xlabel="Measured backlog at t₁ (packets)", ylabel="Global pop stop (cycles)",
       title="Wrapper birth: one link versus N token writes")
ax.set_ylim(-15, max(int(r["global_stop_cycles"]) for r in rows) * 1.15 + 10)
ax.legend(loc="center left", fontsize=9)
ax.grid(alpha=.2)
'''

_FIRST = '''
fig, ax = plt.subplots(figsize=(9, 4.8), constrained_layout=True)
names, runs = SETTINGS["flows"], SETTINGS["runs"]
width = .8 / len(runs)
for index, run in enumerate(runs):
    values = {r["flow"]: int(r["first_service_cycles_from_t1"]) for r in rows if r["run"] == run}
    bars = ax.bar([x - .4 + (index + .5) * width for x in range(len(names))],
                  [values[name] for name in names], width, label=SETTINGS["titles"][run])
    ax.bar_label(bars, fontsize=8, padding=3)
ax.set_xticks(range(len(names)), names)
ax.set(ylabel="First service − request time (cycles)", title="One request: admitting media and gaming")
ax.set_ylim(0, ax.get_ylim()[1] * 1.15)
ax.grid(axis="y", alpha=.2)
ax.set_axisbelow(True)
ax.legend(fontsize=8, loc="upper left")
'''

_UNTOUCHED = '''
runs = SETTINGS["runs"]
fig, axes = plt.subplots(1, len(runs), figsize=(5.6 * len(runs), 5.5),
                         sharex=True, sharey=True, constrained_layout=True)
maximum = 0
for axis, run in zip(axes, runs):
    for index, flow in enumerate(SETTINGS["flows"]):
        for source in ("control", run):
            data = [r for r in rows if r["run"] == run and r["source"] == source and r["flow_name"] == flow]
            x = [int(r["push_cycle"]) for r in data]
            y = [int(r["delay_cycles"]) for r in data]
            maximum = max(maximum, max(y, default=0))
            if source == "control":
                axis.plot(x, y, color=COLORS[index], alpha=.65, linestyle="--", linewidth=.8,
                          label=flow + " (p1 control)")
            else:
                axis.scatter(x, y, color=COLORS[index], alpha=.65, s=7, linewidths=0, label=flow)
    timeline(axis, [run])
    axis.set(title=SETTINGS["titles"][run], xlabel="Packet generation cycle", xlim=SETTINGS["xlim"])
    axis.grid(alpha=.15)
axes[0].set(ylabel="Packet delay: pop − generation (cycles)", ylim=(-maximum * .025, maximum * 1.06))
'''

_BODIES = {"zoom": _ZOOM, "stop": _STOP, "first": _FIRST, "untouched": _UNTOUCHED}
_SAVE = '''
fig.savefig(HERE / "figure.png", dpi=180, bbox_inches="tight")
fig.savefig(HERE / "figure.svg", metadata={"Date": None}, bbox_inches="tight")
plt.close(fig)
'''
