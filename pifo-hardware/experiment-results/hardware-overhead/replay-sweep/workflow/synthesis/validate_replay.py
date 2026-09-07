#!/usr/bin/env python3
"""Validate replay ordering, log credits, global commits, and bank equivalence."""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

from run import vivado_root


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build",type=Path)
    args=parser.parse_args();build=args.build.resolve()
    manifest=json.loads((build/"manifest.json").read_text());h=manifest["hardware"]
    assert (h["configuration"],h["pifo_backend"],h["num_engines"],h["num_vpifos_per_pe"],h["replay_log_depth"]) == ("replay","external",2,8,4)
    work=build/"replay-validation";work.mkdir(exist_ok=True)
    for name,want in manifest["rtl_sha256"].items():
        source=build/"rtl"/name
        assert hashlib.sha256(source.read_bytes()).hexdigest()==want,name
        shutil.copy2(source,work/name)
    bench=Path(__file__).with_name("replay_controller_tb.sv");shutil.copy2(bench,work/bench.name)
    tool=vivado_root(None)
    result={"hardware":h,"rtl_sha256":manifest["rtl_sha256"],"testbench_sha256":hashlib.sha256(bench.read_bytes()).hexdigest(),"status":"running"}
    try:
        for stage,command in [("compile",[tool/"bin/xvlog","-sv","PifoMesh.v",bench.name]),
                              ("elaborate",[tool/"bin/xelab","replay_controller_tb","-s","replay_controller","--mt","4"]),
                              ("simulate",[tool/"bin/xsim","replay_controller","-runall"])]:
            with (work/f"{stage}.log").open("w") as stream:
                subprocess.run(command,cwd=work,stdout=stream,stderr=subprocess.STDOUT,check=True,timeout=120)
        log=(work/"simulate.log").read_text()
        assert "REPLAY_CONTROLLER_PASS" in log and "REPLAY_FAIL" not in log
        result["status"]="passed";result["evidence"]=[line for line in log.splitlines() if "REPLAY_" in line]
    except (subprocess.CalledProcessError,subprocess.TimeoutExpired,AssertionError) as error:
        result["status"]="failed";result["error"]=str(error)
    (work/"validation.json").write_text(json.dumps(result,indent=2)+"\n")
    print(json.dumps(result,indent=2))
    return 0 if result["status"]=="passed" else 1


if __name__=="__main__":
    sys.exit(main())
