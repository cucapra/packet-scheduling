#!/usr/bin/env python3
"""Audit staged CAM hierarchy, bank RAM ports and controller storage before synthesis."""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import re


def verify(build):
    m = json.loads((build / 'manifest.json').read_text())
    h = m['hardware']
    assert h['lookup_backend'] == 'cam-pipelined'
    rtl = (build / 'rtl/PifoMesh.v').read_text()
    modules = dict(re.findall(r'(?ms)^module\s+(\w+)\b(.*?)^endmodule', rtl))
    assert 'BoundedCamMapper' not in rtl
    kinds = '|'.join(map(re.escape, [*modules, 'cam_srl']))
    instance = re.compile(rf'(?ms)^\s*({kinds})\s*(?:#\(.*?\)\s*)?(\w+)\s*\(')
    counts = Counter()
    def visit(kind):
        counts[kind] += 1
        if kind == 'cam_srl':
            return
        for child, _ in instance.findall(modules[kind]):
            visit(child)
    visit('PifoMesh')
    cams = {kind: count for kind, count in counts.items() if kind.startswith('PipelinedCamMapper')}
    assert sum(cams.values()) == 2 * h['num_engines']
    assert counts['cam_srl'] == h['num_engines'] * (1 + h['mapper_banks']) * h['cam_stages']
    memories = []
    for kind, instances in cams.items():
        body = modules[kind]
        arrays = re.findall(r'reg\s*\[(\d+):0\]\s+(banks_\d+_values)\s*\[0:(\d+)\];', body)
        assert len(arrays) in (1, 2)
        for high, name, last in arrays:
            reads = len(re.findall(r'<=\s*' + name + r'\[', body))
            writes = len(re.findall(name + r'\[[^\]\n]+\]\s*<=', body))
            assert reads == writes == 1 and int(last) + 1 == h['cam_entries_per_bank']
            memories.append(dict(module=kind, instances=instances, array=name,
                width=int(high) + 1, depth=int(last) + 1, read_ports=reads, write_ports=writes))
        if len(arrays) == 2:
            assert 'assign banks_0_selectedWrite = (! writeBank);' in body
            assert 'assert((! (banks_0_writeEnable && banks_1_writeEnable)))' in body
        for bank in range(len(arrays)):
            for stage in range(1, h['cam_stages']):
                assert re.search(rf'banks_{bank}_stages_{stage}_aligned_hit\s*<=\s*'
                                 rf'banks_{bank}_stages_{stage - 1}_output_payload_hit;', body)
    controller = []
    for kind, body in modules.items():
        if kind.startswith('ReplayControlFifo'):
            arrays = re.findall(r'reg\s*\[(\d+):0\]\s+(\w+)\s*\[0:(\d+)\];', body)
            assert len(arrays) == 1, arrays
            high, name, last = arrays[0]
            assert int(last) + 1 == h['commit_queue_depth'] == 256
            controller.append(dict(module=kind, array=name, width=int(high) + 1, depth=256))
    assert len(controller) == (1 if h['configuration'] == 'replay' else 0)
    upstream = json.loads((Path(__file__).resolve().parents[1] / 'hw/verilog/vendor/verilog-cam/UPSTREAM.json').read_text())
    for filename in ('cam_srl.v', 'priority_encoder.v'):
        assert hashlib.sha256((build / 'rtl' / filename).read_bytes()).hexdigest() == upstream['files']['rtl/' + filename]
    return dict(status='passed', build=str(build.resolve()), configuration=h['configuration'],
        vflows=h['global_flow_id_capacity'], entries_per_stage=h['cam_entries_per_stage'],
        stages=h['cam_stages'], minimum_lookup_latency=h['cam_minimum_read_latency'],
        cam_instances=sum(cams.values()), srl_cam_instances=counts['cam_srl'],
        value_memories=memories, replay_controller_storage=controller,
        copied_upstream_hashes_checked=True, implementation_run=False, synthesis_run=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build', type=Path)
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    result = verify(args.build.resolve())
    output = json.dumps(result, indent=2) + '\n'
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(output)
    print(output, end='')
