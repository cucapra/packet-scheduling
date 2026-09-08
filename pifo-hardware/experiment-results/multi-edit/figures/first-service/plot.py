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

SETTINGS = {'runs': ('control-p2', 'rio', 'prefill', 'reset'),
 'flows': ['Video', 'Chat', 'Game', 'Vr'],
 'titles': {'rio': 'Rio: localized edits',
            'prefill': 'Whole-tree: prefill SP',
            'relocate': 'Whole-tree: copy + prefill',
            'reset': 'Stop-the-world reset',
            'control': 'Control: p1',
            'control-p2': 'Control: p2'}}

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

fig.savefig(HERE / "figure.png", dpi=180, bbox_inches="tight")
fig.savefig(HERE / "figure.svg", metadata={"Date": None}, bbox_inches="tight")
plt.close(fig)
