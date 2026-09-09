"""Extract native vendor counts and preserve synthesis evidence."""
import json
from pathlib import Path
import re
import shutil
import sys
PROJECT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(PROJECT / 'synthesis'))
from summarize_quartus import summarize as summarize_quartus
from summarize_vivado import summarize as summarize_vivado

RESOURCE_NAMES={
    "logic_alms": ("Estimated ALMs","ALMs"),
    "logic_aluts": ("Combinational ALUTs","ALUTs"),
    "logic_luts": ("CLB LUTs","LUTs"),
    "registers": ("Registers / FFs","bits"),
    "block_memory_bits": ("Inferred RAM bits","bits"),
    "mlab_memory_bits": ("MLAB memory bits","bits"),
    "bram36_tiles": ("BRAM36 tile equivalents","tiles"),
    "m20k_equivalents": ("M20K equivalents (est.)","M20K equivalents"),
    "uram288_blocks": ("URAM288 blocks","blocks"),
    "bram_uram_allocated_bits": ("Allocated BRAM + URAM bits","bits"),
    "lutram_luts": ("LUTs as memory (RAM/SRL)","LUTs"),
    "distributed_ram_luts": ("LUTs as distributed RAM","LUTs"),
    "srl_luts": ("LUTs as shift registers","LUTs"),
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
        if "zero_memory_usage" in parsed:
            values["Total block memory bits"]=parsed["zero_memory_usage"]["block_memory_bits"]
            values["Total MLAB memory bits"]=parsed["zero_memory_usage"]["mlab_memory_bits"]
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
