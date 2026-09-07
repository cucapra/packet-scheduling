#!/usr/bin/env python3
"""Run/reuse vendor synthesis experiments and preserve raw evidence plus CSVs.

The ordinary baseline retains command ingress/routing, uses single-bank mapper
RAMs, and consumes commit messages as no-ops. Percent overhead is relative to
that baseline, independently for each vendor and resource unit.
"""
from __future__ import annotations

import argparse
from concurrent.futures import ThreadPoolExecutor
from contextlib import ExitStack
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys
import time

PROJECT=Path(__file__).resolve().parents[2]
sys.path.insert(0,str(PROJECT/"synthesis"))
from summarize_quartus import summarize as summarize_quartus
from summarize_vivado import summarize as summarize_vivado

RESOURCE_NAMES={
    "logic_alms": ("Estimated ALMs","ALMs"),
    "logic_aluts": ("Combinational ALUTs","ALUTs"),
    "logic_luts": ("CLB LUTs","LUTs"),
    "registers": ("Registers / FFs","bits"),
    "block_memory_bits": ("Mapped block-memory bits","bits"),
    "mlab_memory_bits": ("MLAB memory bits","bits"),
    "bram36_tiles": ("BRAM36 tile equivalents","tiles"),
    "uram288_blocks": ("URAM288 blocks","blocks"),
    "bram_uram_allocated_bits": ("Allocated BRAM + URAM bits","bits"),
    "lutram_luts": ("LUTs used as RAM","LUTs"),
    "dsp_blocks": ("DSP blocks","blocks"),
}


def count_text(value: int | float, signed: bool=False) -> str:
    """Keep fractional BRAM tile equivalents instead of rounding them away."""
    value=float(value)
    precision=0 if value.is_integer() else 1
    return format(value,f'{"+" if signed else ""},.{precision}f')


def capacity_checks(platform: str, parsed: dict) -> list[dict]:
    """Compare only capacities explicitly present in the vendor reports."""
    checks=[]
    if platform=="vivado":
        for resource,name in (("logic_luts","CLB LUTs"),("registers","CLB Registers"),
                              ("bram36_tiles","Block RAM Tile"),("uram288_blocks","URAM"),
                              ("dsp_blocks","DSPs")):
            row=parsed["resources"].get(name,{})
            used,available=row.get("Used"),row.get("Available")
            if isinstance(used,(int,float)) and isinstance(available,(int,float)):
                checks.append({"resource":resource,"used":used,"available":available})
    else:
        for rows in parsed["tables"].values():
            for row in rows:
                if row and row[0]=="Logic utilization estimate (in ALMs)":
                    match=re.match(r"([\d,]+)\s*/\s*([\d,]+)",row[1])
                    if match:
                        used,available=(int(x.replace(",","")) for x in match.groups())
                        checks.append({"resource":"logic_alms","used":used,"available":available})
    for check in checks:
        check["utilization_percent"]=100*check["used"]/check["available"] if check["available"] else None
        check["exceeds_capacity"]=check["used"]>check["available"]
    return checks


def load_config(path: Path) -> dict:
    config=json.loads(path.read_text())
    if config.get("schema")!="rio-hardware-overhead-v1":
        raise ValueError("expected rio-hardware-overhead-v1 configuration")
    h=config["hardware"]
    if h["flow_namespace"]!="coupled-to-vpifo-ids" or h["pifo_backend"] not in ("house","external"):
        raise ValueError("experiment requires the current namespace and house or external PIFO")
    if h["num_engines"]<1 or h["entries_per_pe"]<2:
        raise ValueError("invalid fixed hardware dimensions")
    if not h["vflows"] or len(set(h["vflows"]))!=len(h["vflows"]):
        raise ValueError("vflows must be a nonempty list of distinct sizes")
    for flows in h["vflows"]:
        if flows<2 or flows & (flows-1) or h["entries_per_pe"]%flows:
            raise ValueError("vflows must be powers of two dividing fixed capacity")
    if config["configurations"]!=["static","dynamic"]:
        raise ValueError("compare static then dynamic configuration")
    if set(config["platforms"])!={"quartus","vivado"}:
        raise ValueError("both platforms are required")
    return config


def build_name(config: dict, flows: int, variant: str, platform: str) -> str:
    h=config["hardware"]
    prefix="rio-only-" if h["pifo_backend"]=="external" else ""
    return f'{prefix}pe{h["num_engines"]}-v{flows}-c{h["entries_per_pe"]}-{variant}-{platform}'


def command(config: dict, flows: int, variant: str, platform: str, root: Path) -> list[str]:
    h=config["hardware"]
    result=[sys.executable,str(PROJECT/"synthesis/run.py"),"--tool",platform,
            "--build-root",str(root),"--name",build_name(config,flows,variant,platform),
            "--engines",str(h["num_engines"]),"--vpifos",str(flows),
            "--entries-per-pe",str(h["entries_per_pe"]),"--priority-bits",str(h["priority_bits"]),
            "--pifo-backend",h["pifo_backend"],"--configuration",variant,
            "--clock-mhz",str(config["clock_mhz"]),"--threads",str(config["threads"]),
            "--vivado-directive",config["vivado_directive"]]
    if platform=="vivado" and config.get("vivado_capacity_policy")=="allow_oversized_estimates":
        result.append("--vivado-allow-over-capacity")
    if platform=="quartus" and config.get("quartus_initialization")=="compact_mif":
        result.append("--quartus-compact-init")
    return result


def resources(build: Path, platform: str) -> tuple[dict,dict]:
    if platform=="quartus":
        parsed=summarize_quartus(build/"output_files/pifo.syn.rpt")
        rows=next(v for k,v in parsed["tables"].items() if k.startswith("Synthesis Resource Usage Summary"))
        values={r[0]:r[1] for r in rows}
        mapping={"logic_alms":"Estimate of Logic utilization (ALMs needed)",
                 "logic_aluts":"Combinational ALUT usage for logic",
                 "registers":"Dedicated logic registers","block_memory_bits":"Total block memory bits",
                 "mlab_memory_bits":"Total MLAB memory bits","dsp_blocks":"Total DSP Blocks"}
        if "recovered_memory_total" in parsed:
            values["Total block memory bits"]=parsed["recovered_memory_total"]["block_memory_bits"]
        return {k:int(values[v]) for k,v in mapping.items()
                if k not in parsed.get("unreported_resources",[])},parsed
    parsed=summarize_vivado(build)
    r=parsed["resources"]
    return {"logic_luts":r["CLB LUTs"]["Used"],"registers":parsed["flip_flop_primitives"],
            "bram36_tiles":parsed["mapped_memory"]["bram36_tile_equivalents"],
            "uram288_blocks":parsed["mapped_memory"]["uram_primitives"],
            "bram_uram_allocated_bits":parsed["mapped_memory"]["reported_bram_uram_allocated_bits"],
            "lutram_luts":r.get("LUT as Memory",{}).get("Used",0),
            "dsp_blocks":parsed["dsp_primitives"]},parsed


def reusable(build: Path, config: dict, flows: int, variant: str, platform: str,
             allow_running: bool=False, allow_prepared: bool=False) -> bool:
    try:
        m=json.loads((build/"manifest.json").read_text())
        h=m["hardware"]
        if h["pifo_backend"]!=config["hardware"]["pifo_backend"]:
            return False
        statuses={"synthesis_complete"}
        if allow_running: statuses.add("synthesis_running")
        if allow_prepared: statuses.add("prepared")
        if m["status"] not in statuses or m["tool"]!=platform:
            return False
        expected=(config["hardware"]["num_engines"],flows,config["hardware"]["entries_per_pe"],variant)
        if (h["num_engines"],h["num_vpifos_per_pe"],h["shared_entries_per_pe"],h["configuration"])!=expected:
            return False
        if variant=="replay" and h.get("replay_log_depth")!=config["hardware"].get("replay_log_depth"):
            return False
        if m["clock_target_mhz"]!=config["clock_mhz"] or h["priority_bits"]!=config["hardware"]["priority_bits"]:
            return False
        if platform=="vivado" and m.get("vivado_directive")!=config["vivado_directive"]:
            return False
        actual=set(str(p.relative_to(PROJECT)) for p in (PROJECT/"hw/spinal/rio").glob("*.scala"))
        if actual!={p for p in m["source_sha256"] if p.endswith(".scala")}:
            return False
        for name,want in m["source_sha256"].items():
            if hashlib.sha256((PROJECT/name).read_bytes()).hexdigest()!=want:
                return False
        for name,want in m["rtl_sha256"].items():
            if hashlib.sha256((build/"rtl"/name).read_bytes()).hexdigest()!=want:
                return False
        if m["status"]=="synthesis_complete":
            resources(build,platform)
        return True
    except (KeyError,OSError,ValueError,StopIteration):
        return False


def live_runner(build: Path) -> int | None:
    """Find a running synthesis wrapper before reusing or preparing its files."""
    for process in Path("/proc").glob("[0-9]*"):
        try:
            args=(process/"cmdline").read_bytes().decode().split("\0")
            if not any(x.endswith("synthesis/run.py") for x in args):
                continue
            if "--name" not in args or args[args.index("--name")+1]!=build.name:
                continue
            root=(Path(args[args.index("--build-root")+1]) if "--build-root" in args
                  else PROJECT/"synthesis/build")
            if root.resolve()==build.parent.resolve():
                return int(process.name)
        except (OSError,UnicodeError,IndexError):
            continue
    return None


def adopt(build: Path, platform: str, pid: int) -> int:
    print(f"Waiting for existing {platform} job: {build.name} (PID {pid})",flush=True)
    while Path(f"/proc/{pid}").exists():
        try:
            status=json.loads((build/"manifest.json").read_text()).get("status")
        except (OSError,ValueError):
            time.sleep(5)
            continue
        if status not in ("synthesis_running","prepared"):
            break
        time.sleep(5)
    try:
        if json.loads((build/"manifest.json").read_text()).get("status")!="synthesis_complete":
            return 1
        resources(build,platform)
        return 0
    except (KeyError,OSError,ValueError,StopIteration):
        return 1


def execute(args: list[str], log: Path) -> int:
    print("Running:"," ".join(args),flush=True)
    with log.open("w") as stream:
        return subprocess.run(args,cwd=PROJECT,stdout=stream,stderr=subprocess.STDOUT).returncode


def archive(build: Path, output: Path, platform: str) -> dict:
    output.mkdir(parents=True,exist_ok=True)
    paths=["manifest.json",f"{platform}-version.txt","generate.log","project.log","synthesis.log",
           "pifo.qsf","pifo.sdc","pifo.xdc","synthesis_complete.txt","quartus-initialization.json"]
    if platform=="quartus":
        paths += ["output_files/pifo.syn.rpt","output_files/pifo.drc.partitioned.rpt",
                  "output_files/pifo.drc.synthesized.rpt"]
    else:
        paths += ["reports/utilization.rpt","reports/utilization-hierarchy.rpt",
                  "reports/ram-utilization.rpt","reports/primitive-counts.tsv"]
    for name in paths:
        src=build/name
        if src.exists():
            target=output/name
            target.parent.mkdir(parents=True,exist_ok=True)
            shutil.copy2(src,target)
    result={"status":"incomplete","build":str(build),"platform":platform,"resources":{}}
    try:
        m=json.loads((build/"manifest.json").read_text())
        result.update(status=m["status"],part=m["part"],hardware=m["hardware"],
                      source_sha256=m["source_sha256"],rtl_sha256=m.get("rtl_sha256",{}),
                      tool_version=m.get("tool_version"),clock_target_mhz=m["clock_target_mhz"])
        if platform=="vivado":
            result["device_capacity_check"]=m.get("vivado_capacity_check","enforced")
        if m["status"]=="synthesis_complete":
            values,parsed=resources(build,platform)
            (output/"resource-summary.json").write_text(json.dumps(parsed,indent=2)+"\n")
            result["resources"]=values
            result["capacity_checks"]=capacity_checks(platform,parsed)
            if parsed.get("resource_notes"):
                result["resource_notes"]=parsed["resource_notes"]
                result["unreported_resources"]=parsed.get("unreported_resources",[])
        elif "failed" in m["status"]:
            log=build/"synthesis.log"
            if log.exists():
                contents=log.read_text(errors="replace")
                result["failure_messages"]=[line for line in contents.splitlines()
                                            if re.match(r"(?:ERROR:|Error\s*\()",line)]
                match=re.search(r"Design needs ([\d,]+) (\w+) which is more than device capacity of ([\d,]+)",contents)
                if match:
                    result["capacity_failure"]={"resource":match[2],
                        "required":int(match[1].replace(",","")),"available":int(match[3].replace(",","")),
                        "stage":"failed_synthesis_capacity_check"}
    except (OSError,ValueError,KeyError,StopIteration) as error:
        result["status"]="invalid_result"
        result["error"]=str(error)
    (output/"result.json").write_text(json.dumps(result,indent=2)+"\n")
    return result


def write_csv(path: Path, rows: list[dict], fields: list[str]) -> None:
    path.parent.mkdir(parents=True,exist_ok=True)
    with path.open("w",newline="") as stream:
        writer=csv.DictWriter(stream,fieldnames=fields)
        writer.writeheader(); writer.writerows(rows)


def collect(config: dict, root: Path, output: Path) -> None:
    rows=[]; comparisons=[]; statuses=[]; capacities=[]; failures=[]; failure_pairs=[]
    for flows in config["hardware"]["vflows"]:
        for platform in config["platforms"]:
            pair={}; evidence={}
            for variant in config["configurations"]:
                name=build_name(config,flows,variant,platform)
                result=archive(root/name,output/"runs"/name,platform)
                statuses.append({"vflows":flows,"configuration":variant,**result})
                pair[variant]=result["resources"]
                evidence[variant]=result
                if "capacity_failure" in result:
                    failures.append({"platform":platform,"vflows":flows,"configuration":variant,
                                     "part":result["part"],**result["capacity_failure"]})
                for check in result.get("capacity_checks",[]):
                    capacities.append({"platform":platform,"vflows":flows,"configuration":variant,
                                       "part":result["part"],**check})
                for resource,value in result["resources"].items():
                    rows.append({"platform":platform,"vflows":flows,"configuration":variant,
                                 "resource":resource,"unit":RESOURCE_NAMES[resource][1],"value":value,
                                 "part":result["part"],"source":f"runs/{name}/resource-summary.json"})
            both_failed=all("capacity_failure" in evidence[v] for v in config["configurations"])
            if (pair["static"] and pair["dynamic"]) or both_failed:
                for key in ("part","tool_version","source_sha256","clock_target_mhz"):
                    if evidence["static"][key]!=evidence["dynamic"][key]:
                        raise ValueError(f"Unmatched {platform}/{flows} comparison: {key} differs")
            if both_failed:
                a,b=(evidence[v]["capacity_failure"] for v in config["configurations"])
                if a["resource"]==b["resource"] and a["available"]==b["available"]:
                    failure_pairs.append({"platform":platform,"vflows":flows,"resource":a["resource"],
                        "static":a["required"],"dynamic":b["required"],"available":a["available"],
                        "absolute_change":b["required"]-a["required"],
                        "percent_change":100*(b["required"]-a["required"])/a["required"]})
            for resource in sorted(set(pair["static"]) & set(pair["dynamic"])):
                static,dynamic=pair["static"][resource],pair["dynamic"][resource]
                comparisons.append({"platform":platform,"vflows":flows,"resource":resource,
                                    "unit":RESOURCE_NAMES[resource][1],"static":static,"dynamic":dynamic,
                                    "absolute_change":dynamic-static,
                                    "percent_change":100*(dynamic-static)/static if static else ""})
    write_csv(output/"resources.csv",rows,["platform","vflows","configuration","resource","unit","value","part","source"])
    write_csv(output/"comparison.csv",comparisons,["platform","vflows","resource","unit","static","dynamic","absolute_change","percent_change"])
    write_csv(output/"device-capacity.csv",capacities,["platform","vflows","configuration","part","resource","used","available","utilization_percent","exceeds_capacity"])
    write_csv(output/"failed-capacity-checks.csv",failures,["platform","vflows","configuration","part","resource","required","available","stage"])
    write_csv(output/"failed-capacity-comparison.csv",failure_pairs,["platform","vflows","resource","static","dynamic","available","absolute_change","percent_change"])
    (output/"run-status.json").write_text(json.dumps(statuses,indent=2)+"\n")
    lines=[f'# {config["title"]}',"",'Difference = dynamic − static; percentage uses static as the denominator.',
           'Blank percentages mean a zero baseline. Only completed synthesis reports supply resource values.',"",
           '| Platform | vFlows | Resource | Static | Dynamic | Absolute change | Change |',
           '|---|---:|---|---:|---:|---:|---:|']
    if config["hardware"]["pifo_backend"]=="external":
        lines[2:2]=['**RIO logic only; PIFO cores are excluded through explicit top-level interfaces.**',
                    'Percentages use the ordinary RIO logic as denominator, not the total scheduler.',
                    'PIFO storage, sorting, occupancy and drain detection require a separate resource budget.', '']
    for row in comparisons:
        percent=f'{row["percent_change"]:+.2f}%' if row["percent_change"]!="" else "N/A"
        lines.append(f'| {row["platform"]} | {row["vflows"]} | {RESOURCE_NAMES[row["resource"]][0]} | '
                     f'{count_text(row["static"])} | {count_text(row["dynamic"])} | {count_text(row["absolute_change"],True)} | {percent} |')
    missing=[x for x in statuses if x["status"]!="synthesis_complete"]
    if missing:
        lines += ["","Incomplete or failed points (not treated as zero resource usage):",""]
        for result in missing:
            detail=(" "+result["failure_messages"][0]) if result.get("failure_messages") else ""
            lines.append(f'- {result["platform"]}, {result["configuration"]}, {result["vflows"]} vFlows: '
                         f'`{result["status"]}`.{detail}')
    notes=[(result,note) for result in statuses for note in result.get("resource_notes",[])]
    if notes:
        lines += ["", "Report accounting notes:", ""]
        lines += [f'- {result["platform"]}, {result["configuration"]}, {result["vflows"]} IDs: {note}'
                  for result,note in notes]
    if failure_pairs:
        lines += ["","RAM requirements reported by failed synthesis capacity checks:","",
                  '**These are diagnostic counts before successful synthesis completion, not final utilization reports.**',"",
                  '| Platform | vFlows | Resource | Static required | Dynamic required | Difference | Change | Device capacity |',
                  '|---|---:|---|---:|---:|---:|---:|---:|']
        lines += [f'| {row["platform"]} | {row["vflows"]} | {row["resource"]} equivalents | '
                  f'{count_text(row["static"])} | {count_text(row["dynamic"])} | '
                  f'{count_text(row["absolute_change"],True)} | {row["percent_change"]:+.2f}% | '
                  f'{count_text(row["available"])} |' for row in failure_pairs]
    exceeded=[row for row in capacities if row["exceeds_capacity"]]
    if exceeded:
        lines += ["","Resource counts exceeding the target device's reported capacity:","",
                  '| Platform | vFlows | Configuration | Resource | Used | Available | Utilization |',
                  '|---|---:|---|---|---:|---:|---:|']
        lines += [f'| {row["platform"]} | {row["vflows"]} | {row["configuration"]} | '
                  f'{RESOURCE_NAMES[row["resource"]][0]} | {count_text(row["used"])} | '
                  f'{count_text(row["available"])} | {row["utilization_percent"]:.2f}% |' for row in exceeded]
    lines += ["",'`device-capacity.csv` compares capacities present in the vendor reports. Passing these checks does not establish routability.',
              'Quartus synthesis reports an ALM capacity but does not provide a fitted M20K allocation here.']
    if any(x.get("device_capacity_check")=="disabled_for_estimation" for x in statuses):
        lines += ["",'Oversized Vivado estimates use a version-specific hook that skips the pre-mapping device-capacity check.',
                  'RTL and optimization passes are unchanged; reported device capacities remain physical limits. See each run manifest.',
                  'Control-probe resource counts were identical with and without the hook. No implementation was attempted.']
    lines += ["",'Synthesis-only estimates; no routed fit or timing closure. The two vendors use different logic units.',
              'The flow/vPIFO namespaces remain coupled, and the dense per-port/token tables grow quadratically.',""]
    (output/"report.md").write_text("\n".join(lines))


def main(default_case: str | None=None) -> None:
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config",type=Path,default=PROJECT/f"experiments/hardware-overhead/{default_case or 'r1-fixed'}.json")
    parser.add_argument("--build-root",type=Path,default=PROJECT/"synthesis/build/hardware-overhead")
    parser.add_argument("--output-dir",type=Path)
    parser.add_argument("--render-only",action="store_true")
    parser.add_argument("--collect-only",action="store_true")
    parser.add_argument("--jobs",type=int,default=2)
    parser.add_argument("--adopt-running",action="store_true",
                        help="Wait for compatible live run.py jobs instead of replacing their builds")
    parser.add_argument("--only-vflows",help="Run only these comma-separated grid points; still collect the full experiment")
    parser.add_argument("--license",default="/data/work/quartus/licenses/LR-187458_License.dat")
    args=parser.parse_args()
    config=load_config(args.config)
    selected=set(map(int,args.only_vflows.split(','))) if args.only_vflows else set(config["hardware"]["vflows"])
    if not selected.issubset(config["hardware"]["vflows"]):
        parser.error("--only-vflows must select points from the experiment configuration")
    root=args.build_root.resolve(); root.mkdir(parents=True,exist_ok=True)
    output=(args.output_dir or PROJECT/config["output_dir"]).resolve(); output.mkdir(parents=True,exist_ok=True)
    if args.jobs<1: parser.error("--jobs must be positive")
    if args.render_only:
        config=load_config(output/"experiment-config.json")
    else:
        (output/"experiment-config.json").write_text(json.dumps(config,indent=2)+"\n")
        if not args.collect_only:
            (output/"execution.json").write_text(json.dumps({"build_root":str(root),"jobs":args.jobs,
                "selected_vflows":sorted(selected),"adopt_running":args.adopt_running,
                "started_at_utc":time.strftime("%Y-%m-%dT%H:%M:%SZ",time.gmtime()),
                "argv":sys.argv},indent=2)+"\n")
        elif not (output/"execution.json").exists():
            (output/"execution.json").write_text(json.dumps({"build_root":str(root),"mode":"collect-only"},indent=2)+"\n")
    if not args.render_only and not args.collect_only:
        futures=[]
        with ExitStack() as stack:
            if args.jobs==1:
                executor=stack.enter_context(ThreadPoolExecutor(max_workers=1))
                executors={platform:executor for platform in config["platforms"]}
            else:
                executors={platform:stack.enter_context(ThreadPoolExecutor(max_workers=workers))
                           for platform,workers in (("quartus",args.jobs//2),
                                                    ("vivado",args.jobs-args.jobs//2))}
            for flows in config["hardware"]["vflows"]:
                if flows not in selected:
                    continue
                for variant in config["configurations"]:
                    qname=build_name(config,flows,variant,"quartus")
                    qbuild=root/qname
                    qargs=command(config,flows,variant,"quartus",root)+["--license",args.license]
                    qdone=reusable(qbuild,config,flows,variant,"quartus")
                    qlive=live_runner(qbuild)
                    if qlive and not (args.adopt_running and reusable(qbuild,config,flows,variant,"quartus",True)):
                        raise RuntimeError(f"Refusing to overwrite live build {qname}; use --adopt-running with matching settings")
                    if not qdone and not qlive and not reusable(qbuild,config,flows,variant,"quartus",allow_prepared=True):
                        code=execute(qargs+["--prepare-only"],output/f"{qname}-prepare.log")
                        if code:
                            print(f"Preparation failed: {qname}",flush=True)
                            continue
                    vname=build_name(config,flows,variant,"vivado")
                    vbuild=root/vname
                    vargs=command(config,flows,variant,"vivado",root)
                    vdone=reusable(vbuild,config,flows,variant,"vivado")
                    vlive=live_runner(vbuild)
                    if vlive and not (args.adopt_running and reusable(vbuild,config,flows,variant,"vivado",True)):
                        raise RuntimeError(f"Refusing to overwrite live build {vname}; use --adopt-running with matching settings")
                    if vlive:
                        futures.append(executors["vivado"].submit(adopt,vbuild,"vivado",vlive))
                    elif not vdone:
                        code=0 if reusable(vbuild,config,flows,variant,"vivado",allow_prepared=True) else execute(
                            vargs+["--rtl-from",str(qbuild),"--prepare-only"],output/f"{vname}-prepare.log")
                        if code:
                            print(f"Preparation failed: {vname}",flush=True)
                        else:
                            futures.append(executors["vivado"].submit(execute,vargs+["--reuse-rtl"],output/f"{vname}-run.log"))
                    if qlive:
                        futures.append(executors["quartus"].submit(adopt,qbuild,"quartus",qlive))
                    elif not qdone:
                        futures.append(executors["quartus"].submit(execute,qargs+["--reuse-rtl"],output/f"{qname}-run.log"))
            for future in futures:
                code=future.result()
                print(f"Synthesis job finished: exit={code}",flush=True)
    if not args.render_only:
        collect(config,root,output)
    from pifo_hardware_overhead_figures import render
    render(output,config)
    print(f"Experiment artifacts: {output}")
    if not args.render_only:
        statuses=json.loads((output/"run-status.json").read_text())
        if any(row["status"]!="synthesis_complete" for row in statuses):
            raise SystemExit("Experiment has failed/incomplete runs; see run-status.json")


if __name__=="__main__":
    main()
