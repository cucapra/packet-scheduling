#!/usr/bin/env python3
"""Replot with only this file, data.csv, and Matplotlib: python plot.py."""
import csv
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
from matplotlib import pyplot as plt

HERE = Path(__file__).resolve().parent
with (HERE / "data.csv").open(newline="") as source:
    rows = list(csv.DictReader(source))
titles = {"rio": "Rio", "prefill": "Whole-tree prefill", "reset": "Stop-the-world reset",
          "control-p2": "Target-policy control"}
colors = {"rio": "#1f77b4", "prefill": "#ff7f0e", "reset": "#d62728", "control-p2": "#777777"}
figure = rows[0].get("figure", "add")
if figure != "bank-replay":
    request = figure
    fig, axes = plt.subplots(1, 3, figsize=(15, 4.6), constrained_layout=True)
    for run in titles:
        data = sorted((r for r in rows if r["request"] == request and r["run"] == run),
                      key=lambda r: int(r["flows"]))
        x = [int(r["flows"]) for r in data]
        for axis, field in zip(axes, ("main_install_instructions", "cycles_to_completion", "peak_added_delay_cycles")):
            axis.plot(x, [float(r[field]) for r in data], "o-", color=colors[run], label=titles[run])
        if run != "control-p2":
            axes[0].plot(x, [int(r["guarded_instructions"]) for r in data], "x--", color=colors[run], alpha=.7)
    for axis in axes:
        axis.set_xscale("log", base=2)
        axis.set_xticks([4, 8, 16, 32, 64], [4, 8, 16, 32, 64])
        axis.set_xlabel("Existing flows N = 2m")
        axis.grid(alpha=.2)
    axes[0].set_ylabel("Instructions (solid: main; dashed: guarded)")
    axes[1].set_ylabel("All configuration ready − t₁ (cycles)")
    axes[2].set_ylabel("Peak paired added delay vs p2 (cycles)")
    axes[2].set_title("Untouched flow: " + data[0]["untouched_flow"], fontsize=10)
    axes[0].legend(fontsize=8)
    fig.suptitle("R-add: one new two-flow SP tenant" if request == "add" else "R-reweight: tenant_01 weight 1 → 2")
    for extension in ("png", "svg"):
        fig.savefig(HERE / f"figure.{extension}", dpi=180)
    plt.close(fig)

else:
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.5), constrained_layout=True)
    for axis, request in zip(axes, ("add", "reweight")):
        for run in ("rio", "prefill", "reset"):
            data = sorted((r for r in rows if r["request"] == request and r["run"] == run),
                          key=lambda r: int(r["flows"]))
            axis.plot([int(r["flows"]) for r in data], [int(r["main_bank_replay_cycles"]) for r in data],
                      "o-", color=colors[run], label=titles[run])
        axis.set(xlabel="Existing flows N = 2m", ylabel="Main-commit bank replay (cycles)", title="R-" + request)
        axis.set_xscale("log", base=2)
        axis.set_xticks([4, 8, 16, 32, 64], [4, 8, 16, 32, 64])
        axis.grid(alpha=.2)
    axes[0].legend(fontsize=8)
    for extension in ("png", "svg"):
        fig.savefig(HERE / f"figure.{extension}", dpi=180)
    plt.close(fig)
