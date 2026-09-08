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

SETTINGS = {'runs': ('prefill', 'relocate'),
 'flows': ['Gmail', 'Ssh', 'Http', 'Https'],
 'controls': ('control', 'control-p2'),
 'titles': {'rio': 'Rio: localized edits',
            'prefill': 'Whole-tree: prefill SP',
            'relocate': 'Whole-tree: copy + prefill',
            'reset': 'Stop-the-world reset',
            'control': 'Control: p1',
            'control-p2': 'Control: p2'},
 'xlim': (0, 18000)}

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

runs = SETTINGS["runs"]
fig, axes = plt.subplots(1, len(runs), figsize=(5.6 * len(runs), 5.5),
                         sharex=True, sharey=True, constrained_layout=True)
maximum = 0
for axis, run in zip(axes, runs):
    for index, flow in enumerate(SETTINGS["flows"]):
        for source in (*SETTINGS["controls"], run):
            data = [r for r in rows if r["run"] == run and r["source"] == source and r["flow_name"] == flow]
            x = [int(r["push_cycle"]) for r in data]
            y = [int(r["delay_cycles"]) for r in data]
            maximum = max(maximum, max(y, default=0))
            if source in SETTINGS["controls"]:
                style = "--" if source == SETTINGS["controls"][0] else ":"
                axis.plot(x, y, color=COLORS[index], alpha=.65, linestyle=style, linewidth=.9,
                          label=flow + " (" + SETTINGS["titles"][source] + ")")
            else:
                axis.scatter(x, y, color=COLORS[index], alpha=.65, s=7, linewidths=0, label=flow)
    timeline(axis, [run])
    axis.set(title=SETTINGS["titles"][run], xlabel="Packet generation cycle", xlim=SETTINGS["xlim"])
    axis.grid(alpha=.15)
axes[0].set(ylabel="Packet delay: pop − generation (cycles)", ylim=(-maximum * .025, maximum * 1.06))

fig.savefig(HERE / "figure.png", dpi=180, bbox_inches="tight")
fig.savefig(HERE / "figure.svg", metadata={"Date": None}, bbox_inches="tight")
plt.close(fig)
