#!/usr/bin/env python3
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

SETTINGS = {'runs': ['link', 'reserved', 'copy'],
 'titles': {'link': 'Strict* link',
            'reserved': 'Reserved-PE Strict wrapper',
            'copy': 'Copy + prefill Strict wrapper'},
 'title': 'zoom: same whole-tree transition, 207 packets at t₁'}

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
        prefix = {"link": "Strict*", "reserved": "Reserved", "copy": "Copy"}.get(row["run"], row["run"]) + " " + name if len(runs) > 1 else name
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
            labels.append(f"{prefix} {label} = {cycle}")
        costs.append(f"{prefix}: {row['instruction_count']} inst / {row['commit_cycles']} cycles; "
                     f"bank replay {row['bank_replay_cycles']} cycles")
    series = axis.legend(loc="upper right", fontsize=8)
    axis.add_artist(series)
    if handles:
        key = axis.legend(handles, labels, loc="upper center", bbox_to_anchor=(.5, -.16),
                          ncol=max(1, min(4, len(costs))), fontsize=6.5)
        axis.annotate("\n".join(costs), xy=(.5, 0), xycoords=key, xytext=(0, -6),
                      textcoords="offset points", ha="center", va="top", fontsize=7,
                      annotation_clip=False)

fig, ax = plt.subplots(figsize=(13, 7.6 if len(SETTINGS["runs"]) > 2 else 5.8), constrained_layout=True)
for index, run in enumerate(SETTINGS["runs"]):
    data = [r for r in rows if r["run"] == run]
    ax.scatter([int(r["push_cycle"]) for r in data], [int(r["delay_cycles"]) for r in data],
               s=10, alpha=.7, color=COLORS[index], label="zoom — " + SETTINGS["titles"][run])
timeline(ax, SETTINGS["runs"])
ax.set(xlabel="Packet generation / push cycle", ylabel="Per-packet delay (cycles)",
       title=SETTINGS["title"])
ax.grid(alpha=.2)
ax.margins(x=.01, y=.06)

fig.savefig(HERE / "figure.png", dpi=180, bbox_inches="tight")
fig.savefig(HERE / "figure.svg", metadata={"Date": None}, bbox_inches="tight")
plt.close(fig)
