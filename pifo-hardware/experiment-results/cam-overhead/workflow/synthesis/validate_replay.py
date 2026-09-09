#!/usr/bin/env python3
"""Simulate shared-FIFO replay and audit generated RAM ports; no synthesis."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

from run import vivado_root


def structure(rtl, hardware):
    modules = re.findall(r'^module\s+(\w+)\b.*?^endmodule', rtl, re.M | re.S)
    fifo = re.search(r'^module ReplayControlFifo\b.*?^endmodule', rtl, re.M | re.S).group()
    arrays = re.findall(r'\breg\s+\[(\d+):0\]\s+(\w+)\s+\[0:(\d+)\]', fifo)
    width = 3 + hardware['engine_id_bits'] + hardware['vpifo_id_bits'] + hardware['token_bits'] + hardware['flow_state_bits']
    assert arrays == [(str(width-1), 'storage', str(hardware['commit_queue_depth']-1))], arrays
    assert len(re.findall(r'\bstorage\[[^\]]+\]\s*<=', fifo)) == 1, 'Expected one FIFO write port'
    assert len(re.findall(r'<=\s*storage\[', fifo)) == 1, 'Expected one FIFO read port'
    assert re.search(r'always @\(posedge clk\) begin\s+if\(readEnable\) begin\s+\w+ <= storage\[', fifo), 'Expected a synchronous FIFO read'
    top = re.search(r'^module PifoMesh\b.*?^endmodule', rtl, re.M | re.S).group()
    assert not re.search(r'^\s*StreamFifo\w*\s+\w+\s*\(', top, re.M), 'Separate controller FIFO remains'
    assert len(re.findall(r'^\s*ReplayControlFifo\s+\w+\s*\(', top, re.M)) == 1
    assert 'MapperReplayInstruction' not in rtl
    mapper_count = 0
    for name in modules:
        if not re.fullmatch(r'ReplayMapper(?:_\d+)?', name):continue
        body = re.search(rf'^module {name}\b.*?^endmodule', rtl, re.M | re.S).group()
        for bank in ('banks_0', 'banks_1'):
            assert len(re.findall(rf'\b{bank}\[[^\]]+\]\s*<=', body)) == 1, f'{name}.{bank}: expected one write port'
            assert len(re.findall(rf'<=\s*{bank}\[', body)) == 1, f'{name}.{bank}: expected one read port'
        mapper_count += 1
    mapper_instances = len(re.findall(r'^\s*ReplayMapper(?:_\d+)?\s+\w+\s*\(', rtl, re.M))
    assert mapper_count >= 2 and mapper_instances == 2*hardware['num_engines'], (mapper_count, mapper_instances)
    return dict(status='passed', controller_arrays=1, controller_words=hardware['commit_queue_depth'],
                controller_word_bits=width, controller_storage_bits=width*hardware['commit_queue_depth'],
                controller_read_ports=1, controller_write_ports=1, separate_journal=False,
                mapper_definitions_checked=mapper_count, mapper_instances=mapper_instances,
                mapper_read_ports_per_bank=1, mapper_write_ports_per_bank=1,
                evidence='Generated RTL declarations and synchronous RAM accesses; not a synthesis mapping')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("build",type=Path)
    args=parser.parse_args();build=args.build.resolve()
    manifest=json.loads((build/"manifest.json").read_text());h=manifest["hardware"]
    assert (h["configuration"],h["pifo_backend"],h["num_engines"],h["num_vpifos_per_pe"],h["mapper_sync"]) == ("replay","external",2,8,"shared_control_fifo_replay")
    depth=h['commit_queue_depth']
    work=build/"replay-validation";work.mkdir(exist_ok=True)
    for name,want in manifest["rtl_sha256"].items():
        source=build/"rtl"/name
        assert hashlib.sha256(source.read_bytes()).hexdigest()==want,name
        shutil.copy2(source,work/name)
    names=['shared_control_fifo_tb']+(['replay_controller_tb'] if depth>=4 else [])
    benches=[Path(__file__).with_name(name+'.sv') for name in names]
    for bench in benches:
        (work/bench.name).write_text(f'`define CONTROL_DEPTH {depth}\n'+bench.read_text())
    tool=vivado_root(None)
    result={"hardware":h,"rtl_sha256":manifest["rtl_sha256"],
            "testbench_sha256":{b.name:hashlib.sha256((work/b.name).read_bytes()).hexdigest() for b in benches},
            "status":"running","synthesis_run":False,"implementation_run":False,"evidence":[]}
    try:
        result['structure']=structure((work/'PifoMesh.v').read_text(),h)
        for name,bench in zip(names,benches):
            for stage,command in [("compile",[tool/"bin/xvlog","-sv","PifoMesh.v",bench.name]),
                                  ("elaborate",[tool/"bin/xelab",name,"-s",name,"--mt","4"]),
                                  ("simulate",[tool/"bin/xsim",name,"-runall"])]:
                with (work/f"{name}-{stage}.log").open("w") as stream:
                    subprocess.run(command,cwd=work,stdout=stream,stderr=subprocess.STDOUT,check=True,timeout=120)
            log=(work/f'{name}-simulate.log').read_text()
            marker='SHARED_FIFO_PASS' if name=='shared_control_fifo_tb' else 'REPLAY_CONTROLLER_PASS'
            assert marker in log and '_FAIL:' not in log,log[-2000:]
            result['evidence'] += [line for line in log.splitlines() if marker in line]
        result["status"]="passed"
    except (subprocess.CalledProcessError,subprocess.TimeoutExpired,AssertionError) as error:
        result["status"]="failed";result["error"]=str(error)
    (work/"validation.json").write_text(json.dumps(result,indent=2)+"\n")
    print(json.dumps({k:result[k] for k in ('status','evidence')},indent=2))
    if 'error' in result:print(result['error'])
    return 0 if result["status"]=="passed" else 1


if __name__=="__main__":
    sys.exit(main())
