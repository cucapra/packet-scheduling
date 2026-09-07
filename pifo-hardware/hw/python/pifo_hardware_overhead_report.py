#!/usr/bin/env python3
"""Build the complete R1–R4 report from completed, archived measurements only.

No vendor tools, licenses, or synthesis databases are needed. Original experiment
artifacts remain unchanged. The explicit Quartus M20K control is a distinct case,
never silently substituted into the automatic-mapping sweep.
"""
import argparse
import base64
import csv
import hashlib
import html
import json
from pathlib import Path
import shutil
import zipfile

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from matplotlib.ticker import FuncFormatter, ScalarFormatter

PROJECT = Path(__file__).resolve().parents[2]
PLATFORMS = ("quartus", "vivado")
NAMES = {
    "logic_alms": "Estimated ALMs", "logic_aluts": "Combinational ALUTs",
    "logic_luts": "CLB LUTs", "registers": "Registers / FFs",
    "block_memory_bits": "Inferred RAM bits", "mlab_memory_bits": "MLAB bits",
    "bram36_tiles": "BRAM36 equivalents", "uram288_blocks": "URAM288 blocks",
    "bram_uram_allocated_bits": "Allocated BRAM + URAM bits",
    "lutram_luts": "LUTs used as RAM", "dsp_blocks": "DSP blocks",
}
METRICS = {
    "quartus": ("logic_alms", "logic_aluts", "registers", "block_memory_bits",
                "mlab_memory_bits", "dsp_blocks"),
    "vivado": ("logic_luts", "registers", "bram36_tiles", "bram_uram_allocated_bits",
               "lutram_luts", "uram288_blocks", "dsp_blocks"),
}
GROUPS = {
    "logic": {"quartus": "logic_alms", "vivado": "logic_luts"},
    "registers": dict.fromkeys(PLATFORMS, "registers"),
    "memory": {"quartus": "block_memory_bits", "vivado": "bram36_tiles"},
}
VARIANTS = {
    "static": ("Ordinary tables", "#4b6478", "s"),
    "dynamic": ("Atomic read/copy", "#c46a1d", "o"),
    "replay": ("Replay, automatic mapping", "#007d91", "^"),
    "replay_m20k": ("Replay, journal M20K", "#7761a8", "*"),
}
COMPONENTS = {
    "post_mapper": ("Post-mapper banks", "#007d91"),
    "engine_cam": ("Unbanked engineCAM", "#7b8e9c"),
    "pre_mapper": ("Pre-mapper banks", "#c46a1d"),
    "replay_log": ("Instruction journal", "#7761a8"),
    "other": ("Other RAM", "#c6cdd2"),
}


def rows(path):
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream))


def write_csv(path, data):
    if not data:
        raise ValueError(f"No data for {path}")
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(data[0]), lineterminator="\n")
        writer.writeheader()
        writer.writerows(data)


def count(value, sign=False):
    if value is None:
        return "Unreported"
    spec = "+," if sign else ","
    return format(value, spec + (".0f" if float(value).is_integer() else ".1f"))


def percent(value, baseline):
    return 100 * (value - baseline) / baseline if baseline else None


def pct(value):
    return "N/A" if value is None else f"{value:+.2f}%"


class Evidence:
    def __init__(self, root, config):
        self.root, self.config = root, config
        self.inputs, self.data, self.statuses = {}, {}, {}
        self.checked_rows = 0
        for case in ("r1-fixed", "r2-vflows", "r3-lookup-pipeline", "r4-replay",
                     "r4-replay/journal-m20k"):
            records = self.read_json(f"{case}/run-status.json")
            if not records or any(r["status"] != "synthesis_complete" for r in records):
                raise ValueError(f"Incomplete synthesis: {case}")
            self.statuses[case] = records
        expected_counts = {"r1-fixed": 4, "r2-vflows": 24, "r3-lookup-pipeline": 4,
                           "r4-replay": 18, "r4-replay/journal-m20k": 4}
        for case, expected in expected_counts.items():
            assert len(self.statuses[case]) == expected, case
        for case in ("r1-fixed", "r2-vflows", "r4-replay"):
            statuses = {(r["platform"], r["vflows"], r["configuration"]): r
                        for r in self.statuses[case]}
            for row in self.read_csv(f"{case}/resources.csv"):
                platform, flows, variant = row["platform"], int(row["vflows"]), row["configuration"]
                value = float(row["value"])
                status = statuses[platform, flows, variant]
                assert status["resources"][row["resource"]] == value, row
                hardware = status["hardware"]
                for key, expected in (("num_engines", config["num_engines"]),
                                      ("global_flow_id_capacity", flows),
                                      ("shared_entries_per_pe", config["entries_per_pe"]),
                                      ("pifo_backend", "external"),
                                      ("priority_bits", config["priority_bits"])):
                    assert hardware[key] == expected, (case, key)
                if variant == "replay":
                    assert hardware["replay_log_depth"] == config["replay_log_depth"]
                source = root / case / row["source"]
                assert source.is_file(), source
                key = platform, flows, variant, row["resource"]
                if key in self.data:
                    assert self.data[key]["value"] == value, ("Reused baseline differs", key)
                else:
                    self.data[key] = dict(platform=platform, vflows=flows, variant=variant,
                                          resource=row["resource"], value=value,
                                          source="../" + str(source.relative_to(root)))
                self.checked_rows += 1
        control = self.read_json("r4-replay/journal-m20k/full-validation.json")
        assert control["status"] == "passed"
        assert control["canonical_rtl_and_mifs_unchanged"]
        assert control["other_ram_instances_unchanged"] == 22
        assert control["drc_rule_summary_unchanged"]
        self.control = control
        statuses = {r["case"]: r for r in self.statuses["r4-replay/journal-m20k"]}
        for row in self.read_csv("r4-replay/journal-m20k/resources.csv"):
            value = float(row["value"])
            assert value == statuses[row["case"]]["resources"][row["resource"]]
            self.checked_rows += 1
            if row["case"] != config["quartus_final_replay_case"]:
                continue
            assert value == control["m20k_resources"][row["resource"]]
            self.data["quartus", 1024, "replay_m20k", row["resource"]] = dict(
                platform="quartus", vflows=1024, variant="replay_m20k",
                resource=row["resource"], value=value,
                source="../r4-replay/journal-m20k/" + row["source"])
        for platform in PLATFORMS:
            for flows in config["vflows"]:
                for variant in ("static", "dynamic"):
                    for metric in GROUPS.values():
                        self.get(platform, flows, variant, metric[platform])
            for flows in config["replay_vflows"]:
                for metric in GROUPS.values():
                    self.get(platform, flows, "replay", metric[platform])
        self.pipeline = self.read_csv("r3-lookup-pipeline/resources.csv")
        for row in self.pipeline:
            status = next(r for r in self.statuses["r3-lookup-pipeline"]
                          if r["platform"] == row["platform"] and
                          r["latency_cycles"] == int(row["latency_cycles"]))
            assert status["resources"][row["resource"]] == float(row["value"])
            self.checked_rows += 1
        self.memory = self.read_csv("r4-replay/memory-breakdown.csv")
        grouped = {}
        for row in self.memory:
            key = row["platform"], int(row["vflows"]), row["configuration"], row["resource"]
            grouped[key] = grouped.get(key, 0) + float(row["value"])
        for key, value in grouped.items():
            assert self.get(*key) == value, ("RAM components disagree", key)
        self.budget = self.read_csv("pifo-storage-budget.csv")
        self.controller = self.read_json("validation/rio-replay-controller-smoke/validation.json")
        assert self.controller["status"] == "passed"
        self.comparisons = []
        for (platform, flows, variant, resource), row in sorted(self.data.items()):
            if variant == "static":
                continue
            bases = ["static"] if variant == "dynamic" else ["static", "dynamic"]
            if variant == "replay_m20k":
                bases.append("replay")
            for baseline in bases:
                key = platform, flows, baseline, resource
                if key not in self.data:
                    continue  # Missing Quartus read/copy MLAB is not zero.
                base, value = self.data[key]["value"], row["value"]
                self.comparisons.append(dict(platform=platform, vflows=flows, variant=variant,
                    baseline=baseline, resource=resource, baseline_value=base, value=value,
                    absolute_change=value-base, percent_change=percent(value, base)))

    def remember(self, relative):
        path = self.root / relative
        self.inputs[relative] = hashlib.sha256(path.read_bytes()).hexdigest()
        return path

    def read_csv(self, relative):
        return rows(self.remember(relative))

    def read_json(self, relative):
        return json.loads(self.remember(relative).read_text())

    def get(self, platform, flows, variant, resource):
        return self.data[platform, flows, variant, resource]["value"]

    def final_variant(self, platform):
        return "replay_m20k" if platform == "quartus" else "replay"

    def fixed_rows(self, platform, variant, baseline="static"):
        result = []
        for resource in METRICS[platform]:
            base_key, key = (platform, 1024, baseline, resource), (platform, 1024, variant, resource)
            if base_key not in self.data or key not in self.data:
                continue
            base, value = self.data[base_key]["value"], self.data[key]["value"]
            result.append([NAMES[resource], count(base), count(value), count(value-base, True),
                           pct(percent(value, base))])
        return result


def save_figure(fig, output, name, data, config):
    folder = output / "figures" / name
    write_csv(folder / "data.csv", data)
    for suffix in ("png", "svg", "pdf"):
        metadata = {"Date": None} if suffix == "svg" else (
            {"CreationDate": None, "ModDate": None} if suffix == "pdf" else None)
        fig.savefig(folder / f"figure.{suffix}", dpi=config["plot_dpi"],
                    bbox_inches="tight", metadata=metadata)
        if suffix == "svg":
            path = folder / "figure.svg"
            path.write_text("\n".join(line.rstrip() for line in path.read_text().splitlines()) + "\n")
    plt.close(fig)


def table_figure(output, config, name, title, headers, cells, data, note):
    fig, ax = plt.subplots(figsize=(12.8, 1.85 + .40 * len(cells)))
    ax.axis("off")
    table = ax.table(cellText=cells, colLabels=headers, cellLoc="right", loc="center",
                     colWidths=[.10, .25, .16, .16, .19, .14])
    table.auto_set_font_size(False)
    table.set_fontsize(11)
    table.scale(1, 1.65)
    for (r, c), cell in table.get_celld().items():
        cell.set_edgecolor("#d8e0e5")
        if r == 0:
            cell.set_facecolor("#18384a")
            cell.set_text_props(color="white", weight="bold")
        elif r % 2 == 0:
            cell.set_facecolor("#f0f4f6")
        if c < 2:
            cell.set_text_props(ha="left")
    fig.suptitle(title, weight="bold", y=.98, fontsize=15)
    fig.text(.02, .018, note, fontsize=10, color="#465564")
    save_figure(fig, output, name, data, config)


def flow_axis(ax, flows):
    ax.set_xscale("log", base=2)
    ax.set_xticks(flows)
    ax.xaxis.set_major_formatter(ScalarFormatter())
    ax.set_xlim(flows[0] / 1.16, flows[-1] * 1.22)
    ax.set_xlabel("vFlow / virtual-PIFO ID capacity")
    ax.grid(True, alpha=.2)


def plot_unit(platform, resource):
    if resource == "block_memory_bits":
        return 2**20, "Inferred RAM (Mibit)"
    return 1, NAMES[resource]


def make_figures(e, output, config):
    plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 11,
                         "axes.spines.top": False, "axes.spines.right": False,
                         "svg.fonttype": "none", "svg.hashsalt": "rio-hardware-report-v1",
                         "pdf.fonttype": 42})
    cells = []
    for p in PLATFORMS:
        cells.extend([[p.capitalize()] + r for r in e.fixed_rows(p, "dynamic")
                      if r[0] in {NAMES[g[p]] for g in GROUPS.values()} or
                      r[0] == NAMES["logic_aluts"]])
    table_figure(output, config, "r1-fixed", "R1 · Atomic read/copy versus ordinary tables\n"
                 "5 PEs · 1,024 vFlows · PIFO excluded",
                 ["Platform", "Resource", "Ordinary", "Read/copy", "Absolute change", "Change"],
                 cells, [r for r in e.comparisons if r["variant"] == "dynamic" and r["vflows"] == 1024],
                 "Synthesis only. Change = read/copy − ordinary; percentages divide by ordinary. Native vendor units differ.")
    for group, metrics in GROUPS.items():
        fig, axes = plt.subplots(1, 2, figsize=(12.2, 4.6), layout="constrained")
        plotted = []
        for ax, p in zip(axes, PLATFORMS):
            resource = metrics[p]
            divisor, label = plot_unit(p, resource)
            for variant in ("static", "dynamic"):
                name, color, marker = VARIANTS[variant]
                data = [e.data[p, f, variant, resource] for f in config["vflows"]]
                plotted.extend(data)
                ax.plot([r["vflows"] for r in data], [r["value"]/divisor for r in data],
                        label=name, color=color, marker=marker, linewidth=2.2)
            flow_axis(ax, config["vflows"])
            ax.set_yscale("log")
            ax.set_ylabel(label + " · log scale")
            ax.set_title(p.capitalize())
            ax.legend(frameon=True, facecolor="white", edgecolor="none", framealpha=1,
                      fontsize=10, loc="upper left")
            capacity = 487200 if p == "quartus" and group == "logic" else (
                216960 if p == "vivado" and group == "logic" else (
                480 if p == "vivado" and group == "memory" else None))
            if capacity:
                ax.axhline(capacity/divisor, ls=":", color="#8b3c39", linewidth=1.2)
                ax.text(.58, capacity/divisor, f"Target: {count(capacity)}", color="#8b3c39",
                        transform=ax.get_yaxis_transform(), fontsize=9, va="bottom", ha="center")
        fig.suptitle(f"R2 · {group.capitalize()} versus vFlows · 5 PEs · PIFO excluded", weight="bold")
        save_figure(fig, output, f"r2-{group}", plotted, config)
    fig, axes = plt.subplots(1, 2, figsize=(12.2, 4.8), layout="constrained")
    plotted = []
    for ax, p in zip(axes, PLATFORMS):
        for (group, metrics), color in zip(GROUPS.items(), ("#007d91", "#c46a1d", "#7761a8")):
            data = [r for r in e.comparisons if r["platform"] == p and
                    r["variant"] == "dynamic" and r["resource"] == metrics[p]]
            plotted.extend(data)
            ax.plot([r["vflows"] for r in data], [r["percent_change"] for r in data],
                    marker="o", color=color, linewidth=2, label=group.capitalize())
        flow_axis(ax, config["vflows"])
        ax.axhline(0, color="#666666", linewidth=.8)
        ax.set_ylabel("(Read/copy − ordinary) / ordinary [%]")
        ax.set_title(p.capitalize())
        ax.legend(frameon=False)
    fig.suptitle("R2 · Atomic read/copy overhead · 5 PEs · PIFO excluded", weight="bold")
    save_figure(fig, output, "r2-overhead", plotted, config)

    fig, axes = plt.subplots(2, 3, figsize=(12.8, 7.6), layout="constrained")
    plotted = []
    for axs, p in zip(axes, PLATFORMS):
        for ax, (group, metrics) in zip(axs, GROUPS.items()):
            resource = metrics[p]
            variants = ["static", "dynamic", e.final_variant(p)]
            values = [e.get(p, 1024, v, resource) for v in variants]
            ax.bar(range(3), [v/values[0] for v in values],
                   color=[VARIANTS[v][1] for v in variants], width=.64)
            for x, value in enumerate(values):
                ax.text(x, value/values[0]+.055, count(value), ha="center", fontsize=10)
            ax.set_xticks(range(3), ["Ordinary", "Read/copy", "Replay*" if p == "quartus" else "Replay"])
            ax.set_ylim(0, max(values)/values[0]+.45)
            ax.set_ylabel("Resource / ordinary")
            ax.set_title(f"{p.capitalize()} · {NAMES[resource]}")
            ax.axhline(1, color="#777777", ls=":", linewidth=.8)
            plotted.extend(e.data[p, 1024, v, resource] for v in variants)
    fig.suptitle("R4 · Final replay comparison at 1,024 vFlows\n"
                 "5 PEs · PIFO excluded · *Quartus journal explicitly assigned to M20K", weight="bold")
    save_figure(fig, output, "r4-fixed", plotted, config)

    fig, axes = plt.subplots(3, 2, figsize=(12.6, 11.4), layout="constrained")
    plotted = []
    for axs, (group, metrics) in zip(axes, GROUPS.items()):
        for ax, p in zip(axs, PLATFORMS):
            resource = metrics[p]
            divisor, label = plot_unit(p, resource)
            for variant in ("static", "dynamic", "replay"):
                name, color, marker = VARIANTS[variant]
                data = [e.data[p, f, variant, resource] for f in config["replay_vflows"]]
                plotted.extend(data)
                ax.plot([r["vflows"] for r in data], [r["value"]/divisor for r in data],
                        label=name, color=color, marker=marker, linewidth=2)
            if p == "quartus":
                row = e.data[p, 1024, "replay_m20k", resource]
                plotted.append(row)
                ax.plot(1024, row["value"]/divisor, marker="*", markersize=14,
                        color=VARIANTS["replay_m20k"][1], linestyle="none",
                        label="Journal M20K: 1,024 only")
            flow_axis(ax, config["replay_vflows"])
            ax.set_yscale("log")
            ax.set_ylabel(label + " · log scale")
            ax.set_title(p.capitalize())
            ax.legend(frameon=False, fontsize=9, loc="upper left")
    fig.suptitle("R4 · Replay sweep · 5 PEs · 16,384 journal entries · PIFO excluded\n"
                 "Automatic mapping retained; the star marks the separate Quartus M20K control", weight="bold")
    save_figure(fig, output, "r4-sweep", plotted, config)

    components = []
    for p in PLATFORMS:
        metric = GROUPS["memory"][p]
        for variant in ("static", "dynamic", e.final_variant(p)):
            original = "replay" if variant == "replay_m20k" else variant
            for component in COMPONENTS:
                old = next(r for r in e.memory if r["platform"] == p and
                           r["vflows"] == "1024" and r["configuration"] == original and
                           r["resource"] == metric and r["component"] == component)
                value = float(old["value"])
                source = "../r4-replay/memory-breakdown.csv"
                if variant == "replay_m20k":
                    if component == "replay_log":
                        value = int(e.control["added_journal_ram"]["Implementation Bits"])
                    source = "../r4-replay/journal-m20k/full-validation.json"
                components.append(dict(platform=p, variant=variant, resource=metric,
                                       component=component, value=value, source=source))
            assert sum(r["value"] for r in components if r["platform"] == p and
                       r["variant"] == variant) == e.get(p, 1024, variant, metric)
    fig, axes = plt.subplots(1, 2, figsize=(12.2, 5.4), layout="constrained")
    for ax, p in zip(axes, PLATFORMS):
        variants = ("static", "dynamic", e.final_variant(p))
        divisor, label = plot_unit(p, GROUPS["memory"][p])
        bottom = [0.0]*3
        for component, (name, color) in COMPONENTS.items():
            values = [next(r["value"] for r in components if r["platform"] == p and
                           r["variant"] == v and r["component"] == component)/divisor for v in variants]
            ax.bar(range(3), values, bottom=bottom, color=color, label=name, width=.62)
            bottom = [a+b for a,b in zip(bottom, values)]
        for x, value in enumerate(bottom):
            ax.text(x, value*1.018, f"{value:,.1f}", ha="center", fontsize=10)
        ax.set_ylim(0, max(bottom)*1.16)
        ax.set_xticks(range(3), ["Ordinary", "Read/copy", "Replay*" if p == "quartus" else "Replay"])
        ax.set_ylabel(label)
        ax.yaxis.set_major_formatter(FuncFormatter(lambda v, _: f"{v:,.0f}"))
        ax.set_title(p.capitalize())
    handles, labels = axes[0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="outside lower center", ncol=3, frameon=False, fontsize=10)
    fig.suptitle("R4 · RAM composition at 1,024 vFlows · PIFO excluded\n"
                 "*Quartus replay includes the 655,360-bit M20K journal", weight="bold")
    save_figure(fig, output, "r4-memory-breakdown", components, config)

    pipeline_values = {(r["platform"], int(r["latency_cycles"]), r["resource"]): float(r["value"])
                       for r in e.pipeline}
    pipeline_comparisons, cells = [], []
    for p in PLATFORMS:
        for resource in METRICS[p]:
            a, b = pipeline_values[p, 1, resource], pipeline_values[p, 8, resource]
            pipeline_comparisons.append(dict(platform=p, resource=resource, latency_1=a,
                latency_8=b, absolute_change=b-a, percent_change=percent(b, a)))
            if resource in {g[p] for g in GROUPS.values()} or resource == "lutram_luts":
                cells.append([p.capitalize(), NAMES[resource], count(a), count(b), count(b-a, True), pct(percent(b,a))])
    table_figure(output, config, "r3-pipeline", "R3 · Isolated 131,072 × 10-bit lookup\n"
                 "1-cycle versus 8-cycle reads · initiation interval: 1 cycle",
                 ["Platform", "Resource", "1-cycle read", "8-cycle read", "Absolute change", "Change"],
                 cells, pipeline_comparisons,
                 "Synthesis only; this pipeline is not integrated into R1/R2/R4. Unchanged block RAM; no measured Fmax improvement.")
    return components, pipeline_comparisons


class Report:
    """One content tree for Markdown, self-contained HTML, and paginated PDF."""
    def __init__(self, output, title):
        self.output, self.title, self.blocks = output, title, []

    def heading(self, title, level=2, page=False):
        self.blocks.append(("heading", (title, level, page)))

    def paragraph(self, text):
        self.blocks.append(("paragraph", text))

    def table(self, headers, data):
        self.blocks.append(("table", (headers, data)))

    def figure(self, name, caption):
        self.blocks.append(("figure", (name, caption)))

    def link(self, label, target):
        self.blocks.append(("link", (label, target)))

    def code(self, text):
        self.blocks.append(("code", text))

    def write(self):
        from reportlab import rl_config
        from reportlab.lib import colors
        from reportlab.lib.enums import TA_LEFT
        from reportlab.lib.pagesizes import A4
        from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
        from reportlab.pdfbase import pdfmetrics
        from reportlab.pdfbase.ttfonts import TTFont
        from reportlab.pdfgen.canvas import Canvas
        from reportlab.platypus import (SimpleDocTemplate, Paragraph, Spacer, Table,
                                       TableStyle, Image, PageBreakIfNotEmpty, Preformatted, KeepTogether)
        from PIL import Image as PILImage
        # Binary Flate streams avoid Git treating an ASCII85-only PDF as text.
        rl_config.useA85 = False
        for name, family, weight in (("Rio", "DejaVu Sans", "normal"),
                                     ("RioBold", "DejaVu Sans", "bold"),
                                     ("RioMono", "DejaVu Sans Mono", "normal")):
            pdfmetrics.registerFont(TTFont(name, font_manager.findfont(
                font_manager.FontProperties(family=family, weight=weight))))
        styles = getSampleStyleSheet()
        styles.add(ParagraphStyle("RioBody", fontName="Rio", fontSize=9.3, leading=13.2, spaceAfter=8))
        styles.add(ParagraphStyle("RioCell", fontName="Rio", fontSize=7.2, leading=10, alignment=TA_LEFT))
        styles.add(ParagraphStyle("RioHeadCell", parent=styles["RioCell"], fontName="RioBold", textColor=colors.white))
        styles.add(ParagraphStyle("RioCaption", parent=styles["RioBody"], fontSize=8.2, leading=11,
                                  textColor=colors.HexColor("#4b6478")))
        styles.add(ParagraphStyle("RioCode", fontName="RioMono", fontSize=7.3, leading=11, spaceAfter=10))
        for level in (1, 2, 3):
            styles.add(ParagraphStyle(f"RioH{level}", fontName="RioBold", fontSize=22 if level==1 else 14 if level==2 else 11,
                leading=27 if level==1 else 19 if level==2 else 15, spaceBefore=8, spaceAfter=12,
                textColor=colors.HexColor("#18384a"), keepWithNext=True))
        story, md, h = [], [], []
        width = A4[0] - 80
        def para(value, style="RioBody"):
            return Paragraph(html.escape(str(value)).replace("\n", "<br/>"), styles[style])
        for kind, value in self.blocks:
            if kind == "heading":
                title, level, page = value
                md += ["#"*level + " " + title, ""]
                h.append(f"<h{level}>{html.escape(title)}</h{level}>")
                if page:
                    # A table can fill a page exactly. Do not emit a blank page
                    # when the next section already starts on an empty frame.
                    while story and isinstance(story[-1], Spacer):
                        story.pop()
                    story.append(PageBreakIfNotEmpty())
                story.append(para(title, f"RioH{level}"))
            elif kind == "paragraph":
                md += [value, ""]
                h.append(f"<p>{html.escape(value)}</p>")
                story.append(para(value))
            elif kind == "link":
                label, target = value
                md += [f"[{label}]({target})", ""]
                h.append(f'<p><a href="{html.escape(target, quote=True)}">{html.escape(label)}</a></p>')
                story.append(para(f"{label}: {target}", "RioCaption"))
            elif kind == "code":
                md += ["```bash", value, "```", ""]
                h.append(f"<pre>{html.escape(value)}</pre>")
                story.append(Preformatted(value, styles["RioCode"]))
            elif kind == "table":
                headers, data = value
                md += ["| " + " | ".join(headers) + " |", "|" + "---|"*len(headers)]
                md += ["| " + " | ".join(map(str, row)) + " |" for row in data]
                md.append("")
                h.append("<div class='table'><table><thead><tr>" + "".join(f"<th>{html.escape(v)}</th>" for v in headers) + "</tr></thead><tbody>" +
                         "".join("<tr>" + "".join(f"<td>{html.escape(str(v))}</td>" for v in row) + "</tr>" for row in data) + "</tbody></table></div>")
                cells = [[para(v, "RioHeadCell") for v in headers]] + [[para(v, "RioCell") for v in row] for row in data]
                widths = [width/len(headers)]*len(headers)
                if len(headers) == 5:
                    proportions = (.29,.20,.20,.19,.12)
                    if headers[0] == "vFlows":
                        proportions = (.12,.20,.21,.21,.26)
                    elif headers[0] == "Platform":
                        proportions = (.14,.25,.23,.19,.19)
                    widths = [width*x for x in proportions]
                elif len(headers) == 6:
                    widths = [width*x for x in (.10,.23,.19,.19,.17,.12)]
                table = Table(cells, colWidths=widths, repeatRows=1, hAlign="LEFT")
                table.setStyle(TableStyle([
                    ("BACKGROUND", (0,0), (-1,0), colors.HexColor("#18384a")),
                    ("ROWBACKGROUNDS", (0,1), (-1,-1), [colors.white, colors.HexColor("#f0f4f6")]),
                    ("VALIGN", (0,0), (-1,-1), "TOP"),
                    ("TOPPADDING", (0,0), (-1,-1), 5), ("BOTTOMPADDING", (0,0), (-1,-1), 5),
                    ("LINEBELOW", (0,-1), (-1,-1), .4, colors.HexColor("#c8d3db"))]))
                story.extend([table, Spacer(1, 12)])
            elif kind == "figure":
                name, caption = value
                relative = f"figures/{name}/figure.png"
                md += [f"![{caption}]({relative})", "", caption, "",
                       f"[SVG](figures/{name}/figure.svg) · [PDF](figures/{name}/figure.pdf) · [CSV](figures/{name}/data.csv)", ""]
                data_url = "data:image/png;base64," + base64.b64encode((self.output / relative).read_bytes()).decode()
                h.append(f'<figure><img src="{data_url}" alt="{html.escape(caption, quote=True)}"><figcaption>{html.escape(caption)}</figcaption></figure>')
                with PILImage.open(self.output / relative) as picture:
                    iw, ih = picture.size
                height = width * ih/iw
                story.append(KeepTogether([Image(str(self.output / relative), width=width, height=height),
                                           para(caption, "RioCaption")]))
        (self.output / "report.md").write_text("\n".join(md))
        css = """body{font:16px/1.6 system-ui,sans-serif;max-width:1100px;margin:40px auto;padding:0 24px;color:#223746}
        h1,h2,h3{line-height:1.25;color:#18384a}h2{margin-top:48px}a{color:#006d85}img{width:100%;height:auto}
        figure{margin:28px 0}figcaption{color:#526777;font-size:14px}.table{overflow:auto}table{border-collapse:collapse;width:100%;font-size:14px}
        th{background:#18384a;color:white;text-align:left}td,th{padding:9px 12px;border-bottom:1px solid #d7e0e6}
        tr:nth-child(even){background:#f0f4f6}pre{background:#f0f4f6;padding:16px;overflow:auto;font-size:13px}
        @media print{body{font-size:10pt}h2{break-before:page}figure,table{break-inside:avoid}}"""
        (self.output / "report.html").write_text("<!doctype html><html lang='en'><meta charset='utf-8'>"
            "<meta name='viewport' content='width=device-width,initial-scale=1'>"
            f"<title>{html.escape(self.title)}</title><style>{css}</style><body>" + "\n".join(h) + "</body></html>")
        def footer(canvas, doc):
            canvas.setFont("Rio", 7.5)
            canvas.setFillColor(colors.HexColor("#637687"))
            canvas.drawString(40, 26, "RIO · Archived synthesis estimates · PIFO cores excluded")
            canvas.drawRightString(A4[0]-40, 26, str(doc.page))
        doc = SimpleDocTemplate(str(self.output / "report.pdf"), pagesize=A4,
                                leftMargin=40, rightMargin=40, topMargin=35, bottomMargin=42,
                                title=self.title, author="RIO hardware experiments")
        doc.build(story, onFirstPage=footer, onLaterPages=footer,
                  canvasmaker=lambda *a, **k: Canvas(*a, **(k | {"invariant": 1})))


def make_report(e, output, config, components, pipeline):
    report = Report(output, config["title"])
    report.heading(config["title"], 1)
    report.paragraph(f"Completed evidence through {config['evidence_date']} · synthesis milestone "
                     f"{config['synthesis_milestone']} · Quartus Pro 25.3.1 and Vivado 2025.2.")
    report.paragraph("All requested fixed-size, vFlow-sweep, isolated lookup-pipeline, and controller-replay synthesis cases are complete. "
        "This report regenerates figures from the archived measurements. It includes the final Quartus journal-in-M20K control and preserves the original automatic-mapping results.")
    report.paragraph("Controller replay substantially reduces the cost of the original read/copy implementation, but these results do not yet support a claim of negligible dynamic-configuration overhead. "
        "The latest replay implementation still adds about 41–43% logic over ordinary tables at 1,024 vFlows, and the dense table geometry exceeds the reference devices' memory capacity.")
    summary = []
    for p in PLATFORMS:
        v = e.final_variant(p)
        for group in ("logic", "memory"):
            metric = GROUPS[group][p]
            value, base, old = [e.get(p, 1024, x, metric) for x in (v, "static", "dynamic")]
            summary.append([p.capitalize(), NAMES[metric], count(value), pct(percent(value, base)), pct(percent(value, old))])
    report.table(["Platform", "Resource", "Final replay", "vs ordinary", "vs read/copy"], summary)
    report.paragraph("Final replay means journal-only M20K assignment for Quartus at 1,024 IDs, and the original RAM-mapped replay run for Vivado. "
        "Percentages use each platform's own resource units. All counts exclude PIFO cores; they are not total-scheduler overhead percentages.")
    report.table(["Experiment", "Requested comparison", "Completed evidence"], [
        ["R1", "5 PEs, 1,024 vFlows, 1,024 shared PIFO entries per PE reserved", "4 vendor/variant results"],
        ["R2", "32, 64, 128, 256, 512, 1,024 vFlows; ordinary vs read/copy", "24 results; R1 reuses the 1,024 point"],
        ["R3", "131,072 × 10-bit lookup; 1 vs 8 cycles", "4 synthesis results plus simulation"],
        ["R4", "Replay vs both baselines at 32, 128, 1,024 vFlows", "18 results, including reused baselines"],
        ["Journal control", "Quartus automatic vs M20K; isolated and full core", "4 results, including the original full-core replay reference"],
    ])

    report.heading("Measurement scope and tool setup", page=True)
    report.paragraph("All mesh comparisons use five PEs, eight-bit ranks, a 100 MHz clock constraint, and 1,024 shared PIFO slots per PE as an integration budget. "
        "An explicit external-PIFO interface exposes requests, responses, empty status, and drain events. The controller, command queues and routing, mapper tables, brain/state logic, PE streams, and crossbar remain observable runtime hardware. "
        "Sorting, PIFO entry storage, occupancy tracking, and drain detection are outside the netlist.")
    report.paragraph("The ordinary baseline has one synchronous RAM per mapper. It retains the configuration controller and normal command path, applies writes immediately, consumes commit messages as no-ops, and prunes commit/swap/synchronization/drain-armed rewrite logic. "
        "It is still programmable; this comparison measures the cost of atomic reconfiguration relative to ordinary lookup tables, not the cost of all programmability relative to a compile-time fixed scheduler.")
    report.paragraph("Read/copy uses the original atomic double banks and synchronization reads. Replay uses two banks with one packet-read port and one write port each, plus a shared instruction journal and commit/replay control. All journal and controller costs are included.")
    report.table(["Setting", "Quartus", "Vivado"], [
        ["Installed version", "Pro 25.3.1 Build 100", "2025.2"],
        ["Install root", "/data/work/quartus/quartus", "/data/work/vivado/2025.2/Vivado"],
        ["Target", "Agilex 7 AGFB014R24B2E2V", "Kintex UltraScale+ xcku5p-ffvb676-2-e"],
        ["Board definition", "Agilex 7 F-Series devkit BTS; no NIC/OpenCL shell", "KCU116, board definition 1.5"],
        ["Synthesis settings", "Balanced, virtual data pins, 8 threads", "Out of context, rebuilt hierarchy, RuntimeOptimized, 8 threads"],
        ["Constraint", "100 MHz target", "100 MHz target"],
        ["License", "Local LR-187458 license used successfully", "Installed Standard license used successfully"],
        ["Implementation", "Not run", "Not run"],
    ])
    report.paragraph("Vivado estimates use the documented estimation-only hook that skips its pre-mapping device-capacity gate. It does not bypass licensing or change the reported device capacities. "
        "The hook was checked on an unchanged smaller RAM control. Quartus uses an equivalent compact zero-filled MIF initialization view, validated against the canonical RTL. "
        "Synthesis success and the 100 MHz constraint establish neither a physical fit nor achieved frequency.")
    report.paragraph("Resource accounting: Quartus ALMs are estimated logic usage; ALUTs and registers are also reported. Its RAM metric is inferred Implementation Bits, including width pruning and replication, not fitted M20K allocation. "
        "Vivado reports mapped CLB LUTs, FFs, and BRAM36 equivalents (one RAMB36 or two RAMB18); allocated memory bits include primitive allocation. URAM and DSP usage are zero in these mesh runs. "
        "LUT RAM is reported separately and is already part of total LUT usage. ALMs and LUTs must not be compared as interchangeable units.")
    report.paragraph("For every pair: absolute change = variant − baseline; percentage change = 100 × (variant − baseline) / baseline. A zero baseline produces N/A. "
        "Unreported resources stay unreported. The 1,024-ID Quartus read/copy RAM aggregate overflows a signed 32-bit field; the positive detailed RAM rows and disjoint hierarchy totals reconcile exactly to 2,516,583,192 bits. "
        "The raw report is preserved, and its missing MLAB count is not treated as zero.")
    report.link("Detailed installed-tool setup and reproduction", "../../../synthesis/README.md")

    report.heading("R1: fixed setup, ordinary versus atomic read/copy", page=True)
    report.paragraph("The requested fixed point is five PEs, 1,024 vFlows, and a reserved capacity of 1,024 PIFO entries per PE. These are legal RTL parameters. The PIFO implementation is excluded under the agreed experiment scope.")
    for p in PLATFORMS:
        report.heading(p.capitalize(), 3)
        report.table(["Resource", "Ordinary", "Read/copy", "Absolute change", "Change"], e.fixed_rows(p, "dynamic"))
    report.paragraph("The original implementation adds 111.40% estimated ALMs on Quartus and 57.78% LUTs on Vivado. Memory grows by 185.71% and 128.56%, respectively. "
        "The lower Vivado FF count is a netlist mapping result; it does not offset additional LUT and RAM costs in a common unit. This baseline comparison does not demonstrate limited hardware overhead.")
    report.figure("r1-fixed", "Figure 1. Fixed-point resource comparison. The CSV contains all reported resource categories; the figure selects the primary logic, register, and memory metrics.")
    report.link("Original R1 report and raw-evidence links", "../r1-fixed/report.md")

    report.heading("R2: resource scaling with vFlow capacity", page=True)
    report.paragraph("Only the vFlow/virtual-PIFO ID capacity changes: 32, 64, 128, 256, 512, and 1,024. The PE count, reserved PIFO capacity, rank width, and original lookup latency remain fixed. "
        "Figures 2–4 use logarithmic resource axes so all measured sizes remain visible. Lines connect completed points as visual guides; no missing size is estimated. Figure 5 uses a linear percentage axis.")
    for name, caption in (
        ("r2-logic", "Figure 2. Native logic usage versus vFlow capacity. Dotted lines show reported target logic capacity; passing a logic limit does not establish fit."),
        ("r2-registers", "Figure 3. Register/FF counts versus vFlow capacity. Vendor memory and logic mapping create non-smooth scaling."),
        ("r2-memory", "Figure 4. Quartus inferred RAM in Mibit (2^20 bits), and Vivado BRAM36 equivalents. These are different accounting metrics. The Vivado dotted line is the target's 480-tile capacity."),
        ("r2-overhead", "Figure 5. Atomic read/copy overhead relative to ordinary tables for logic, registers, and block memory. The denominator excludes PIFO hardware.")):
        report.figure(name, caption)
    for p in PLATFORMS:
        report.heading(f"{p.capitalize()} sweep: absolute resource counts", 3)
        values = []
        for f in config["vflows"]:
            for v in ("static", "dynamic"):
                values.append([str(f), "Ordinary" if v == "static" else "Read/copy"] +
                              [count(e.get(p, f, v, g[p])) for g in GROUPS.values()])
        report.table(["vFlows", "Variant"] + [NAMES[g[p]] for g in GROUPS.values()], values)
    report.paragraph("In the current namespace, five PEs require three engine-ID bits. With V flow IDs, token width is log2(V) + 3, and each deep post-mapper/flow-state address space has 8 × V² words. "
        "At V = 1,024, that is 8,388,608 words per PE. This quadratic growth is present in the ordinary baseline as well as both dynamic versions; it must be separated from atomicity overhead.")
    report.link("Every R2 resource difference, including absolute and percentage changes", "../r2-vflows/comparison.csv")

    report.heading("R3: pipeline exploration for a large lookup", page=True)
    report.paragraph("This component experiment uses one ordinary 131,072 × 10-bit lookup, matching a post-mapper bank at 128 IDs. It compares one-cycle and eight-cycle read latency; both accept one request per cycle. "
        "The added pipeline is not integrated into the R1/R2/R4 mesh.")
    report.figure("r3-pipeline", "Figure 6. Pipelining leaves block-memory usage unchanged at the tested geometry and adds registers or LUT RAM. No routed timing improvement is established.")
    report.paragraph("Additional R3 counters: Quartus ALUTs change from 286 to 285 (−1, −0.35%); MLAB and DSP counts remain zero. "
        "Vivado allocates 1,327,104 block-memory bits in both variants and uses zero URAM and DSP blocks. The complete comparison CSV contains every counter, including zero-baseline N/A percentages.")
    report.link("All R3 resource counters and differences", "pipeline-comparison.csv")
    report.paragraph("Simulation passed data/valid alignment, bubbles, bursts, highest addresses, collisions, and reset during outstanding reads (255 and 249 responses for the two latencies). "
        "A useful next experiment is explicit RAM banking with a registered bank-selection tree and matching request metadata and response buffering in both configurations. "
        "Simple output pipelining does not remove table words or extra read-port replicas.")
    report.paragraph("For the much deeper 1,024-ID engineCAM tables, Vivado reports insufficient internal pipeline stages for automatic URAM mapping, including a diagnostic requesting 104 stages. "
        "This is a tool diagnostic, not a synthesized 104-cycle implementation. Neither the isolated R3 result nor that diagnostic establishes a feasible large-table implementation or an Fmax improvement.")
    report.link("R3 configuration, simulation, and reports", "../r3-lookup-pipeline/report.md")

    report.heading("R4: controller instruction replay", page=True)
    report.paragraph("The controller reserves journal space for each accepted banked pre/post-mapper update, records it in order, and sends it to the shadow bank. "
        "Commit swaps all mapper banks together. The controller then replays the same ordered writes into the old active bank, which is now the shadow. "
        "No next commit executes until the final replayed write is accepted. Packet lookup reads continue during replay; each accepted request retains its selected bank across a swap.")
    report.paragraph("Each mapper bank has one synchronous read and one write port. The journal also uses one read and one write port. Unbanked brain/state/front-rewrite commands execute once and are not replayed. "
        "Both banks start equal; applying the same ordered update sequence to each restores equality after replay, including repeated writes and unchanged addresses.")
    report.paragraph("The log holds 16,384 instructions globally across all PEs at every tested size. Record widths are 25, 31, and 40 bits at 32, 128, and 1,024 IDs. "
        "The largest declared journal is 655,360 bits. Drivers must respect available credits and issue commit before attempting more banked updates than available space; transactions are not silently split. "
        "At 1,024 IDs the log can hold one pre- and one post-update per vFlow per PE (10,240 updates), but cannot hold an arbitrary full dense-table rewrite. A larger transaction needs a larger journal or separately published batches.")
    report.paragraph("The old read/copy synchronization takes D + 1 busy cycles for a depth-D table, or 8,388,609 at the largest point. Replay takes N issue cycles when the journal and write path accept one instruction per cycle, with global N ≤ 16,384 here. "
        "Backpressure can extend this time. These are RTL cycle counts, not routed latency measurements. The focused simulation observed 52 replay-busy cycles for 52 instructions. Runtime reset during a transaction is not crash recovery.")
    report.figure("r4-fixed", "Figure 7. Final fixed-point comparison, normalized to ordinary tables. Labels show absolute native counts. Quartus replay uses the explicit journal-only M20K assignment; Vivado uses its original RAM-mapped journal.")
    for p in PLATFORMS:
        final = e.final_variant(p)
        report.heading(f"{p.capitalize()}: final replay versus ordinary", 3)
        report.table(["Resource", "Ordinary", "Final replay", "Absolute change", "Change"], e.fixed_rows(p, final))
        report.heading(f"{p.capitalize()}: final replay versus read/copy", 3)
        report.table(["Resource", "Read/copy", "Final replay", "Absolute change", "Change"], e.fixed_rows(p, final, "dynamic"))

    report.heading("R4 sweep and the Quartus journal mapping control", page=True)
    report.figure("r4-sweep", "Figure 8. Completed replay measurements at 32, 128, and 1,024 IDs. The original automatic Quartus result is retained, including its register spike. The M20K star is one separate measured control at 1,024 IDs; no M20K-forced sweep is implied.")
    for p in PLATFORMS:
        report.heading(f"{p.capitalize()}: replay counts", 3)
        values = []
        for f in config["replay_vflows"]:
            values.append([str(f), "Automatic"] + [count(e.get(p, f, "replay", g[p])) for g in GROUPS.values()])
        if p == "quartus":
            values.append(["1024", "Journal M20K"] + [count(e.get(p, 1024, "replay_m20k", g[p])) for g in GROUPS.values()])
        report.table(["vFlows", "Mapping"] + [NAMES[g[p]] for g in GROUPS.values()], values)
    report.paragraph("At 1,024 IDs, Quartus automatic mapping implements the 655,360-bit journal as logic/register storage. Its hierarchy contains 655,446 registers including control, and the full design uses 923,812 registers. "
        "The mapper banks still use simple dual-port RAM. Assigning only the journal array to M20K produces 268,415 registers and 355,832 ALMs in a complete resynthesis of the same core.")
    report.code("set_instance_assignment -name RAMSTYLE_ATTRIBUTE M20K \\\n  -entity StreamFifo_52 -to logic_ram")
    report.table(["Resource", "Automatic replay", "Journal M20K", "Absolute change", "Change"],
                 e.fixed_rows("quartus", "replay_m20k", "replay"))
    report.paragraph("Canonical RTL and all compact MIF inputs are identical; the only hardware-setting change is the journal assignment. The other 22 RAM instances are unchanged. "
        "The isolated journal maps to RAM with or without the assignment and has identical counts, demonstrating why subtracting isolated journal area would not predict the full-core result. "
        "Quartus's smaller replay journals and all Vivado replay journals already use RAM automatically. The Vivado 1,024-ID journal occupies 18 BRAM36 tiles.")
    report.link("Full-core M20K equivalence and mapping audit", "../r4-replay/journal-m20k/full-validation.json")

    report.heading("What causes the remaining memory overhead?", page=True)
    report.figure("r4-memory-breakdown", "Figure 9. RAM components reconcile exactly to each top-level count. The small journal, pre-mapper, and other-RAM contributions may be visually tiny; their exact values are tabulated below. LUT/FF implementations are outside this RAM-only breakdown.")
    for p in PLATFORMS:
        report.heading(f"{p.capitalize()}: {NAMES[GROUPS['memory'][p]]}", 3)
        values = []
        for component, (name, _) in COMPONENTS.items():
            values.append([name] + [count(next(r["value"] for r in components if r["platform"] == p and
                          r["variant"] == v and r["component"] == component))
                          for v in ("static", "dynamic", e.final_variant(p))])
        report.table(["Component", "Ordinary", "Read/copy", "Final replay"], values)
    report.paragraph("The post-mapper accounts for four ordinary-bank equivalents in read/copy and two in replay at this point: replay removes the copy-read replicas but retains the second atomic bank. "
        "The unbanked engineCAM tables are unchanged and remain large. A zero RAM entry for a component does not mean it is absent: for example, Quartus pre-mapper logic/register implementations remain in the total logic/FF counts. "
        "Rebuilt hierarchy can move logic between modules, so module LUT subtotals are not isolated controller-cost estimates.")

    report.heading("Device limits and the deferred PIFO budget", page=True)
    capacities = []
    for p, resource, available in (("quartus", "logic_alms", 487200),
                                    ("vivado", "logic_luts", 216960),
                                    ("vivado", "bram36_tiles", 480)):
        for v in ("static", "dynamic", e.final_variant(p)):
            used = e.get(p, 1024, v, resource)
            capacities.append([p.capitalize(), NAMES[resource], "Final replay" if "replay" in v else VARIANTS[v][0],
                               count(used), count(available), f"{100*used/available:,.2f}%"])
    report.table(["Platform", "Resource", "Variant", "Used", "Available", "Utilization"], capacities)
    report.paragraph("The fixed configuration is a valid RTL sizing point, but it is not a deployable result on these target devices. "
        "The Vivado ordinary design alone requires 35,850 BRAM36 equivalents against 480 available; the final replay design requires 51,230.5. "
        "Quartus reports inferred bits, not fitted M20K counts; those should not be converted into an exact physical block count without mapping/implementation evidence. "
        "Passing the Quartus replay logic-capacity check does not establish memory fit, placement, routing, or timing closure.")
    report.paragraph("For a later common PIFO component cost P measured in one vendor resource, total ordinary ≈ ordinary RIO + P and total dynamic ≈ dynamic RIO + P. "
        "Combined overhead would be 100 × (dynamic RIO − ordinary RIO) / (ordinary RIO + P). The absolute difference stays the same. "
        "Measure the PIFO macro and required empty/drain adapter at the same widths, capacity, target, and synthesis settings; cross-boundary optimization makes the sum approximate.")
    report.table(["vFlows", "PEs", "Entries per PE", "Raw entry payload bits"],
                 [[r["vflows"], r["pes"], r["entries_per_pe"], count(float(r["raw_entry_storage_bits"]))] for r in e.budget])
    report.paragraph("The raw PIFO payload budget is 5 × 1,024 × (2 × log2(V) + 11) bits: 158,720 bits at 1,024 IDs. "
        "It excludes sorting/comparators, movement, valid/occupancy state, priority encoding, drain detection, and FPGA mapping overhead. It is not an ALM/LUT/FF/BRAM estimate. "
        "Earlier whole-mesh results and the 32-ID whole-minus-RIO diagnostic are preserved separately; that residual is not a standalone PIFO cost and is not extrapolated to 1,024 IDs. "
        "The experimental stock PIFO has a recorded ordering defect and is not used to support these comparisons.")
    report.link("Preserved whole-mesh results", "../whole-mesh/r1-fixed/report.md")

    report.heading("Validation, conclusions, and reproduction", page=True)
    report.table(["Validation", "Completed evidence"], [
        ["Ordinary / read-copy packet and configuration tests", "7 packets; 633 / 1,494 cycles. Commit semantics, staged/immediate visibility, and highest encoded IDs checked."],
        ["External-PIFO boundary", "Bound-house-PIFO tests preserve behavior; generated interfaces expose runtime requests/responses and contain no PIFO core."],
        ["Replay full small mesh", "7 packets in 633 cycles; staged visibility and repeated commits passed."],
        ["Replay controller stress test", "29 commits, 52 updates, 88 lookups in 270 cycles; 52 replay-busy cycles, 15 lookups during replay, 22 on swap cycles."],
        ["Replay invariants", "Both banks checked after batches; duplicate writes, partial epochs, FIFO wrap, empty/full-log commits, and queued next-epoch credits checked."],
        ["Memory implementation", "Quartus port audits verify 1R/1W mapper banks. M20K journal control preserves 22 other RAM instances and canonical inputs."],
        ["Pipeline component", "Data/valid alignment, bursts, bubbles, collisions, high addresses, and reset tested for both latencies."],
        ["Report integrity", "Completed statuses, matching reused baselines, per-run resource totals, component sums, and source-file existence checked before plotting."],
    ])
    report.paragraph("The final Quartus M20K run completed with zero synthesis errors and 31 warnings. Its DRC summary is unchanged from automatic replay: a medium Reset Release IP reachability rule remains in this standalone core without the board shell. "
        "No combinational-loop or inferred-latch violations are reported. This is not a claim that all implementation DRCs or board-level reset integration have passed.")
    report.paragraph("The evidence supports a narrower and defensible claim: recording and replaying controller updates removes copy-read memory replication while retaining atomic publication, and reduces both logic and RAM relative to the original implementation on both vendors. "
        "The present dense design does not yet establish limited total atomicity overhead at the requested 1,024-flow point.")
    report.paragraph("The next architectural comparison should decouple flow and virtual-PIFO namespaces or use a compact association table, applied identically to ordinary and replay versions. "
        "Then measure feasible capacities, explore explicit RAM banking/pipelining with full-mesh backpressure and commit tests, and integrate a validated PIFO macro to report total scheduler cost. "
        "Those experiments remain future work; no resource savings or timing improvements from them are included here.")
    report.paragraph("All nine figures are provided as PNG, editable SVG, and vector PDF with their underlying CSV data. The report is also supplied as Markdown, a self-contained HTML file with embedded figures, and a paginated PDF. "
        "The bundle includes these small report artifacts; raw vendor evidence remains in the repository's experiment directories. CSV source paths resolve from the full-report directory; input-hash paths in validation.json resolve from its parent hardware-overhead directory.")
    report.code("# From pifo-hardware; rendering requires no FPGA license.\n"
                ".venv/bin/pip install -r requirements.txt\n"
                ".venv/bin/python hw/python/pifo_hardware_overhead_report.py")
    report.link("Original R1/R2/R3 experiment definitions and synthesis commands", "../../../experiments/hardware-overhead/README.md")
    report.link("Replay protocol, tests, and synthesis commands", "../../../experiments/hardware-overhead/REPLAY.md")
    report.link("Journal-only M20K reproduction and evidence", "../r4-replay/journal-m20k/README.md")
    report.link("Consolidated resource data", "resources.csv")
    report.link("All ordinary/read-copy/replay comparisons", "comparison.csv")
    report.link("Input hashes and report validation", "validation.json")
    report.write()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, default=PROJECT / "experiments/hardware-overhead/full-report.json")
    args = parser.parse_args()
    config = json.loads(args.config.read_text())
    root, output = PROJECT / config["results_dir"], PROJECT / config["output_dir"]
    if output == root or root in output.parents and output.name != "full-report":
        raise ValueError("The consolidated output must not replace a source experiment")
    evidence = Evidence(root, config)
    output.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(args.config, output / "experiment-config.json")
    write_csv(output / "resources.csv", [r for _, r in sorted(evidence.data.items())])
    write_csv(output / "comparison.csv", evidence.comparisons)
    components, pipeline = make_figures(evidence, output, config)
    write_csv(output / "memory-breakdown.csv", components)
    write_csv(output / "pipeline-comparison.csv", pipeline)
    make_report(evidence, output, config, components, pipeline)
    validation = dict(status="passed", evidence_date=config["evidence_date"],
        synthesis_milestone=config["synthesis_milestone"], vendor_tools_invoked=False,
        archived_resource_rows_checked=evidence.checked_rows,
        completed_results_per_experiment={k: len(v) for k,v in evidence.statuses.items()},
        note="Counts include reused synthesis references; these are not all distinct vendor runs.",
        quartus_m20k_is_separate_case=True, memory_component_totals_reconciled=True,
        input_path_base="../", csv_source_path_base="./", input_sha256=evidence.inputs,
        renderer_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        matplotlib_version=matplotlib.__version__)
    import reportlab
    validation["reportlab_version"] = reportlab.Version
    (output / "validation.json").write_text(json.dumps(validation, indent=2) + "\n")
    # Stable archive timestamps and ordering keep re-rendering reviewable.
    bundle = output / "rio-hardware-overhead-report.zip"
    with zipfile.ZipFile(bundle, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        files = [p for p in output.rglob("*") if p.is_file() and p != bundle]
        for path in sorted(files):
            info = zipfile.ZipInfo(str(path.relative_to(output)), date_time=(2026,9,7,0,0,0))
            info.compress_type = zipfile.ZIP_DEFLATED
            archive.writestr(info, path.read_bytes())
    print(json.dumps({"status": "complete", "output": str(output),
                      "figures": len(list((output / "figures").glob("*/figure.png"))),
                      "archived_rows_checked": evidence.checked_rows,
                      "vendor_tools_invoked": False}, indent=2))


if __name__ == "__main__":
    main()
