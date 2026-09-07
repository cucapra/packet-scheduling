"""Render hardware-overhead tables and curves from archived measured CSV data."""
from __future__ import annotations

import csv
from pathlib import Path
import shutil


def read_rows(path: Path) -> list[dict]:
    with path.open() as stream:
        return list(csv.DictReader(stream))


def render(output: Path, config: dict) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import FuncFormatter,ScalarFormatter
    from pifo_hardware_overhead import RESOURCE_NAMES,write_csv,count_text

    plt.rcParams.update({"font.family":"DejaVu Sans","font.size":10,
                         "axes.spines.top":False,"axes.spines.right":False,
                         "svg.fonttype":"none"})
    comparison=read_rows(output/"comparison.csv")
    resources=read_rows(output/"resources.csv")
    figures=output/"figures"
    dpi=config["plot"]["dpi"]
    h=config["hardware"]
    fixed_label=f'{h["num_engines"]} PEs · {h["entries_per_pe"]:,} entries per PE'
    if h["pifo_backend"]=="external":
        fixed_label=f'{h["num_engines"]} PEs · PIFO excluded'
    if len(config["hardware"]["vflows"])==1:
        folder=figures/"resource-table"; folder.mkdir(parents=True,exist_ok=True)
        shutil.copy2(output/"comparison.csv",folder/"data.csv")
        selected=[r for r in comparison if r["resource"] not in ("mlab_memory_bits","dsp_blocks","lutram_luts")]
        cells=[]
        for row in selected:
            cells.append([row["platform"].capitalize(),RESOURCE_NAMES[row["resource"]][0],
                          count_text(float(row["static"])),count_text(float(row["dynamic"])),
                          count_text(float(row["absolute_change"]),True),
                          f'{float(row["percent_change"]):+.2f}%' if row["percent_change"] else "N/A"])
        fig,ax=plt.subplots(figsize=(12,max(3.4,1.8+0.38*len(cells))))
        ax.axis("off")
        if cells:
            table=ax.table(cellText=cells,colLabels=["Platform","Resource","Ordinary tables","RIO dynamic","Difference","Overhead"],
                           colWidths=[.12,.28,.15,.15,.15,.15],cellLoc="right",loc="center")
            table.auto_set_font_size(False); table.set_fontsize(10); table.scale(1,1.7)
            for (r,c),cell in table.get_celld().items():
                cell.set_edgecolor("#d1d5db")
                if r==0: cell.set_facecolor("#17324d"); cell.set_text_props(color="white",weight="bold")
                elif r%2==0: cell.set_facecolor("#f0f4f8")
                if c<2: cell.set_text_props(ha="left")
        else:
            ax.text(.5,.5,"No completed matched synthesis pair\nSee report.md and run-status.json",ha="center",va="center")
        ax.set_title(f'RIO dynamic configuration hardware overhead\n{fixed_label} · {h["vflows"][0]:,} vFlows',pad=22,fontweight="bold")
        fig.text(.02,.015,"Synthesis only. Difference = RIO − ordinary tables; percentages use ordinary tables. Vendor logic units differ.",fontsize=9,color="#555555")
        for suffix in ("svg","png"): fig.savefig(folder/f"figure.{suffix}",dpi=dpi,bbox_inches="tight")
        plt.close(fig)
        return

    groups={"logic":{"quartus":"logic_alms","vivado":"logic_luts"},
            "registers":{"quartus":"registers","vivado":"registers"},
            "memory":{"quartus":"block_memory_bits","vivado":"bram_uram_allocated_bits"}}
    for name,metrics in groups.items():
        folder=figures/name; folder.mkdir(parents=True,exist_ok=True)
        rows=[r for r in resources if r["resource"]==metrics[r["platform"]]]
        write_csv(folder/"data.csv",rows,list(resources[0]) if resources else
                  ["platform","vflows","configuration","resource","unit","value","part","source"])
        fig,axes=plt.subplots(1,2,figsize=(11,4.3),layout="constrained")
        for ax,platform in zip(axes,("quartus","vivado")):
            plotted=False
            for variant,color,marker,label in (("static","#52687a","s","Ordinary tables"),
                                               ("dynamic","#007e87","o","RIO dynamic")):
                points={int(r["vflows"]):float(r["value"]) for r in rows
                        if r["platform"]==platform and r["configuration"]==variant}
                if points:
                    xs=sorted(h["vflows"])
                    ys=[points.get(x,float("nan")) for x in xs]
                    ax.plot(xs,ys,label=label,color=color,marker=marker,linewidth=2,markersize=5)
                    plotted=True
            ax.set_xscale("log",base=2)
            ax.set_xticks(config["hardware"]["vflows"])
            ax.set_xlim(min(h["vflows"])/1.12,max(h["vflows"])*1.12)
            ax.xaxis.set_major_formatter(ScalarFormatter())
            ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:f"{x:,.0f}"))
            ax.set_xlabel("vFlow / virtual-PIFO ID capacity")
            ax.set_ylabel(RESOURCE_NAMES[metrics[platform]][0])
            ax.set_title(platform.capitalize())
            ax.grid(True,alpha=.2)
            ax.set_ylim(bottom=0)
            if plotted: ax.legend(frameon=False)
            else:
                ax.set_yticks([])
                ax.text(.5,.5,"No completed measurements",transform=ax.transAxes,ha="center")
        fig.suptitle(f"Resource scaling: {name} · {fixed_label}",fontweight="bold")
        for suffix in ("svg","png"): fig.savefig(folder/f"figure.{suffix}",dpi=dpi,bbox_inches="tight")
        plt.close(fig)

    folder=figures/"overhead-percent"; folder.mkdir(parents=True,exist_ok=True)
    measured=[r for r in comparison if r["percent_change"] and
              r["resource"] in {m[r["platform"]] for m in groups.values()}]
    write_csv(folder/"data.csv",measured,list(comparison[0]) if comparison else
              ["platform","vflows","resource","unit","static","dynamic","absolute_change","percent_change"])
    fig,axes=plt.subplots(1,2,figsize=(11,4.3),layout="constrained")
    for ax,platform in zip(axes,("quartus","vivado")):
        plotted=False
        for category,color in (("logic","#007e87"),("registers","#a15d18"),("memory","#7854a3")):
            points={int(r["vflows"]):float(r["percent_change"]) for r in measured
                    if r["platform"]==platform and r["resource"]==groups[category][platform]}
            if points:
                xs=sorted(h["vflows"])
                ys=[points.get(x,float("nan")) for x in xs]
                ax.plot(xs,ys,marker="o",color=color,label=category.capitalize())
                plotted=True
        ax.set_xscale("log",base=2); ax.set_xticks(config["hardware"]["vflows"])
        ax.set_xlim(min(h["vflows"])/1.12,max(h["vflows"])*1.12)
        ax.xaxis.set_major_formatter(ScalarFormatter())
        ax.set_xlabel("vFlow / virtual-PIFO ID capacity"); ax.set_ylabel("(Dynamic − ordinary) / ordinary [%]")
        ax.set_title(platform.capitalize()); ax.grid(True,alpha=.2); ax.axhline(0,color="#aaaaaa",linewidth=.8)
        if plotted: ax.legend(frameon=False)
        else:
            ax.set_yticks([])
            ax.text(.5,.5,"No completed matched pairs",transform=ax.transAxes,ha="center")
    fig.suptitle("Measured dynamic configuration overhead" +
                 (" · PIFO excluded" if h["pifo_backend"]=="external" else ""),fontweight="bold")
    for suffix in ("svg","png"): fig.savefig(folder/f"figure.{suffix}",dpi=dpi,bbox_inches="tight")
    plt.close(fig)
