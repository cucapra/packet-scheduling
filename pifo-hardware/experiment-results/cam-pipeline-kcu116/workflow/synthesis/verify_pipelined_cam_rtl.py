#!/usr/bin/env python3
"""Check standalone pipelined CAM structure and pinned upstream inputs; no synthesis."""
import argparse
import hashlib
import json
from pathlib import Path
import re


def verify(build, capacity, entries_per_stage, replay):
    text = (build / 'PipelinedCamMapper.v').read_text()
    stages = (capacity + entries_per_stage - 1) // entries_per_stage
    banks = 2 if replay else 1
    instances = re.findall(r'cam_srl\s*#\((.*?)\)\s+(\w+)\s*\(', text, re.S)
    assert len(instances) == banks * stages, len(instances)
    for parameters, name in instances:
        match = re.fullmatch(r'banks_(\d+)_stages_(\d+)_core', name)
        assert match, name
        bank, stage = map(int, match.groups())
        assert bank < banks and stage < stages
        count = min(entries_per_stage, capacity - stage * entries_per_stage)
        address_bits = max(1, (count - 1).bit_length())
        assert int(re.search(r'\.ADDR_WIDTH\s*\(\s*(\d+)', parameters)[1]) == address_bits
    memories = []
    for high, name, last in re.findall(r'reg\s*\[(\d+):0\]\s+(\w+)\s*\[0:(\d+)\];', text):
        assert re.fullmatch(r'banks_\d+_values', name), f'unexpected RAM: {name}'
        reads = len(re.findall(r'<=\s*' + name + r'\[', text))
        writes = len(re.findall(name + r'\[[^\]\n]+\]\s*<=', text))
        assert int(last) + 1 == capacity
        assert reads == writes == 1, (name, reads, writes)
        memories.append(dict(name=name, width=int(high) + 1, depth=capacity,
                             read_ports=reads, write_ports=writes))
    assert len(memories) == banks
    # Registered accumulated match metadata separates consecutive core encoders.
    for bank in range(banks):
        for stage in range(1, stages):
            assert re.search(rf'banks_{bank}_stages_{stage}_aligned_hit\s*<=\s*'
                             rf'banks_{bank}_stages_{stage - 1}_output_payload_hit;', text)
    if replay:
        assert 'assign banks_0_selectedWrite = (! writeBank);' in text
        assert re.search(r'assign banks_0_writeEnable = .*banks_0_selectedWrite', text)
        assert re.search(r'assign banks_1_writeEnable = .*writeBank', text)
        assert 'assert((! (banks_0_writeEnable && banks_1_writeEnable)))' in text
    upstream = json.loads((build / 'verilog-cam-UPSTREAM.json').read_text())
    assert upstream['commit'] == '69002598f12d7418d44bbaa88ea0be3bf3b14e6c'
    for filename in ('cam_srl.v', 'priority_encoder.v'):
        assert hashlib.sha256((build / filename).read_bytes()).hexdigest() == upstream['files']['rtl/' + filename]
    assert hashlib.sha256((build / 'COPYING.verilog-cam').read_bytes()).hexdigest() == upstream['files']['COPYING']
    return dict(status='passed', build=str(build.resolve()), capacity_per_bank=capacity,
                entries_per_stage=entries_per_stage, banks=banks, stages=stages,
                minimum_read_latency=stages + 1, upstream_srl_instances=len(instances),
                value_memories=memories, extra_command_journal=False,
                response_buffer_uses_registers=True, synthesis_run=False,
                physical_resources_measured=False, upstream=upstream['commit'],
                rtl_sha256={name: hashlib.sha256((build / name).read_bytes()).hexdigest()
                            for name in ('PipelinedCamMapper.v', 'cam_srl.v', 'priority_encoder.v')})


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('--capacity', type=int, required=True)
    parser.add_argument('--entries-per-stage', type=int, required=True)
    parser.add_argument('--replay', action='store_true')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    result = verify(args.build, args.capacity, args.entries_per_stage, args.replay)
    content = json.dumps(result, indent=2) + '\n'
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(content)
    print(content, end='')
