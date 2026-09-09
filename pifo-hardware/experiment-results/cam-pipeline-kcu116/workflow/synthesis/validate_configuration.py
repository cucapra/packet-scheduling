#!/usr/bin/env python3
"""Validate atomic visibility or immediate writes/ignored commits in a small mesh."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

from run import vivado_root


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build",type=Path)
    parser.add_argument("--pifo-reference",type=Path,
                        help="For an external-PIFO build, bind the house core from a matching generated full-mesh build")
    args=parser.parse_args()
    build=args.build.resolve()
    manifest=json.loads((build/"manifest.json").read_text())
    h=manifest["hardware"]
    assert (h["num_engines"],h["num_vpifos_per_pe"],h["shared_entries_per_pe"]) == (1,8,32)
    assert h["pifo_backend"] in ("house","external")
    dynamic=h["configuration"] != "static"
    root=vivado_root(None)
    work=build/"configuration-validation"
    work.mkdir(exist_ok=True)
    for name,want in manifest["rtl_sha256"].items():
        source=build/"rtl"/name
        assert hashlib.sha256(source.read_bytes()).hexdigest()==want,name
        shutil.copy2(source,work/name)
    source=Path(__file__).with_name("configuration_contract_tb.sv")
    bench=work/source.name
    bench.write_text(f"`define DYNAMIC_CONFIG {int(dynamic)}\n"+source.read_text())
    result={"hardware":h,"rtl_sha256":manifest["rtl_sha256"],
            "testbench_sha256":hashlib.sha256(source.read_bytes()).hexdigest(),"status":"running"}
    if h["pifo_backend"]=="external":
        if args.pifo_reference is None:
            parser.error("external PIFO validation requires --pifo-reference")
        reference=args.pifo_reference.resolve()
        ref_manifest=json.loads((reference/"manifest.json").read_text())
        for key in ("num_engines","num_vpifos_per_pe","shared_entries_per_pe","priority_bits"):
            assert h[key]==ref_manifest["hardware"][key],key
        assert ref_manifest["hardware"]["pifo_backend"]=="house"
        original=(reference/"rtl/PifoMesh.v").read_text()
        assert hashlib.sha256(original.encode()).hexdigest()==ref_manifest["rtl_sha256"]["PifoMesh.v"]
        core=re.search(r'\bmodule ConcurrentPifoRTL\b.*?\bendmodule\b',original,re.S)[0]
        rtl=(work/"PifoMesh.v").read_text()
        header=re.search(r'\bmodule PifoMesh\s*\((.*?)\);',rtl,re.S)[1]
        ports=re.findall(r'\b(input|output)\s+wire\s*(\[\d+:\d+\])?\s*(\w+)',header)
        boundary=[p for p in ports if p[2].startswith("io_pifo_0_")]
        assert len(boundary)==22, "Incomplete PIFO boundary including accepted-push notifications"
        public=[p for p in ports if p not in boundary]
        wrapper="\nmodule PifoMesh (\n"+",\n".join(f"  {d} wire {w} {n}" for d,w,n in public)+"\n);\n"
        wrapper+="\n".join(f"  wire {w} {n};" for _,w,n in boundary)+"\n"
        wrapper+="  RioMesh rio (\n"+",\n".join(f"    .{n}({n})" for _,_,n in ports)+"\n  );\n"
        wrapper+="  ConcurrentPifoRTL pifo (\n"+",\n".join(
            f"    .{n.replace('io_pifo_0_','io_',1)}({n})" for _,_,n in boundary)
        wrapper+=",\n    .clk(clk), .reset(reset)\n  );\nendmodule\n"
        bound=re.sub(r'\bmodule PifoMesh\b','module RioMesh',rtl,count=1)+wrapper+core+"\n"
        (work/"PifoMesh.v").write_text(bound)
        result["pifo_binding"]={"reference_manifest":str(reference/"manifest.json"),
            "reference_rtl_sha256":ref_manifest["rtl_sha256"]["PifoMesh.v"],
            "house_core_sha256":hashlib.sha256(core.encode()).hexdigest(),
            "bound_rtl_sha256":hashlib.sha256(bound.encode()).hexdigest(),
            "boundary_signal_count":len(boundary)}
    try:
        commands=[("compile",[root/"bin/xvlog","-sv",work/"PifoMesh.v",work/"priority_encode_log.v",bench]),
                  ("elaborate",[root/"bin/xelab","configuration_contract_tb","-s","configuration_contract","--mt","4"]),
                  ("simulate",[root/"bin/xsim","configuration_contract","-runall"])]
        for stage,command in commands:
            print(stage,work,flush=True)
            with (work/f"{stage}.log").open("w") as log:
                subprocess.run(command,cwd=work,stdout=log,stderr=subprocess.STDOUT,check=True,timeout=120)
        log=(work/"simulate.log").read_text()
        result["status"]="passed" if "CONFIGURATION_CONTRACT_PASS" in log and "CONFIGURATION_FAIL" not in log else "failed"
        result["evidence"]=[line for line in log.splitlines() if "CONFIGURATION_" in line]
    except (subprocess.CalledProcessError,subprocess.TimeoutExpired) as error:
        result["status"]="failed"
        result["error"]=str(error)
    (work/"validation.json").write_text(json.dumps(result,indent=2)+"\n")
    print(json.dumps(result,indent=2))
    return 0 if result["status"]=="passed" else 1


if __name__ == "__main__":
    sys.exit(main())
