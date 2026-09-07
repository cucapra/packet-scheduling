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

SETTINGS = {'runs': ['link', 'reserved'],
 'titles': {'link': 'Strict* link', 'reserved': 'Reserved-PE Strict wrapper'}}

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

fig.savefig(HERE / "figure.png", dpi=180, bbox_inches="tight")
fig.savefig(HERE / "figure.svg", metadata={"Date": None}, bbox_inches="tight")
plt.close(fig)
