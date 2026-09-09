#!/usr/bin/env python3
"""Audit controller storage, mapper RAM ports, and native resource attribution."""
from collections import defaultdict
import hashlib
import json
from pathlib import Path
import re

from pifo_synthesis_results import PROJECT
from pifo_shared_fifo_figures import write_csv
from pifo_shared_fifo_experiments import OUT, FLOWS, name, save, DEFAULT_BUILD_ROOT
from verify_cam_rtl import inspect as inspect_cam

ROOT = DEFAULT_BUILD_ROOT


def group(path):
    if 'deque_dequeMapper' in path:
        return 'post_mapper'
    if 'enque_enqueMapper' in path:
        return 'pre_mapper'
    if 'replayControl' in path or 'io_controlRequest_fifo' in path:
        return 'controller_fifo'
    if 'deque_frontRewrite' in path:
        return 'front_rewrite'
    if 'enque_brain' in path:
        return 'brain'
    if 'guard' in path:
        return 'drain_guard'
    if 'xbar' in path:
        return 'crossbar'
    return 'routing_and_other'


def audit_rtl(build):
    m = json.loads((build/'manifest.json').read_text())
    rtl = (build/'rtl/PifoMesh.v').read_text()
    assert hashlib.sha256(rtl.encode()).hexdigest() == m['rtl_sha256']['PifoMesh.v']
    replay = m['hardware']['configuration']=='replay'
    cam = m['hardware'].get('lookup_backend') == 'cam'
    cam_validation = inspect_cam(build) if cam else None
    modules = re.findall(r'\bmodule (\w+)\b(.*?)\bendmodule\b',rtl,re.S)
    assert not any(n.startswith('TransactionalMapper') for n,_ in modules)
    assert 'journal' not in rtl.lower()
    records = []
    for module, body in modules:
        arrays = re.findall(r'reg \[(\d+):0\] (\w+) \[0:(\d+)\];',body)
        ordinary_fifo = (not replay and module.startswith('StreamFifo') and
                         any(int(last)+1==m['hardware']['commit_queue_depth'] for _,_,last in arrays))
        if not ordinary_fifo and not module.startswith(('ReplayMapper','DirectMapper','ReplayControlFifo')):
            continue
        expected = ({'logic_ram'} if ordinary_fifo else {'storage'} if module=='ReplayControlFifo'
                    else {'banks_0','banks_1'} if replay else {'ram'})
        assert {a[1] for a in arrays}==expected,(module,arrays)
        for top, array, last in arrays:
            writes = re.findall(r'\b'+array+r'\[[^\n]+?\]\s*<=',body)
            reads = re.findall(r'<=\s*'+array+r'\[',body)
            assert len(writes)==len(reads)==1,(module,array)
            records.append(dict(module=module,array=array,width=int(top)+1,depth=int(last)+1,
                                read_ports=1,write_ports=1,
                                component='controller_fifo' if ordinary_fifo or module=='ReplayControlFifo' else 'mapper'))
        if module.startswith('ReplayMapper'):
            assert '(io_writeReq_fire && activeBank)' in body
            assert '(io_writeReq_fire && (! activeBank))' in body
    if replay:
        assert len([r for r in records if r['array']=='storage'])==1
        assert len([r for r in records if r['array'].startswith('banks_')])==(10 if cam else 20)
    else:
        assert len(records)==(6 if cam else 11)
    controller=[r for r in records if r['component']=='controller_fifo']
    assert len(controller)==1 and controller[0]['depth']==m['hardware']['commit_queue_depth']
    controller_bits=controller[0]['width']*controller[0]['depth']
    mapper_bits=sum(r['width']*r['depth'] for r in records if r['component']=='mapper')
    if cam:
        mapper_bits += cam_validation['cam_value_bits'] - m['logical_storage_bits']['flow_state_tables']
    assert controller_bits==m['logical_storage_bits']['control_queue_payload']
    assert mapper_bits==sum(m['logical_storage_bits'][k] for k in ('pre_mapper_banks','post_mapper_banks'))
    return dict(status='passed',configuration=m['hardware']['configuration'],
                vflows=m['hardware']['global_flow_id_capacity'],
                separate_journal=False,mapper_bank_writes_mutually_exclusive=replay,
                logical_controller_bits=controller_bits,logical_mapper_bits=mapper_bits,
                arrays=records, cam_value_ram_validation=cam_validation)


def native(build, platform):
    parsed = json.loads((OUT/'runs'/build.name/'resource-summary.json').read_text())
    counts = defaultdict(lambda: defaultdict(int))
    if platform=='quartus':
        ram = next(t for k,t in parsed['tables'].items() if 'RAM Summary' in k)
        memories = [dict(zip(ram[0],r)) for r in ram[1:]]
        for row in memories:
            g = group(row['Name'])
            counts[g]['ram_bits'] += int(row['Implementation Bits'])
            if g in ('pre_mapper','post_mapper','controller_fifo'):
                assert row['Mode']=='Simple Dual Port',row
        hierarchy = next(t for k,t in parsed['tables'].items() if 'Resource Utilization by Entity' in k)
        for row in hierarchy[1:]:
            g = group(row[7])
            # Parentheses are each entity's own use, excluding descendants.
            for resource,index in [('logic_aluts',1),('registers',2)]:
                value = re.fullmatch(r'\d+ \((\d+)\)',row[index])
                assert value,row
                counts[g][resource] += int(value[1])
        return counts, memories
    for row in parsed['ram_instances']:
        counts[group(row['Memory Name'])]['ram_bits'] += row['Available Bits']
    rows = parsed['hierarchy']
    by_path = {r['path']:r for r in rows}
    for path,row in by_path.items():
        children = [r for p,r in by_path.items() if p.rpartition('/')[0]==path]
        for resource,key in [('logic_luts','Total LUTs'),('registers','FFs')]:
            own = row[key]-sum(r[key] for r in children)
            assert own>=0,(path,key)
            counts[group(path)][resource] += own
    text = (build/'reports/ram-utilization.rpt').read_text()
    memories=[]
    for line in text.splitlines():
        cells=[x.strip() for x in line.strip().strip('|').split('|')]
        if len(cells)==6 and cells[3]=='RAM_SDP':
            memories.append(dict(name=cells[0],mode=cells[3],port_a=cells[4],port_b=cells[5]))
    return counts,memories


def main(root=DEFAULT_BUILD_ROOT):
    records, rows, differences = [], [], []
    for flows in FLOWS:
        for platform in ('quartus','vivado'):
            pair={}
            logical={}
            for variant in ('static','replay'):
                build=root/name(flows,variant,platform)
                if not (OUT/'runs'/build.name/'resource-summary.json').exists():
                    continue
                if platform=='quartus':
                    records.append(audit_rtl(build))
                counts,memories=native(build,platform)
                manifest=json.loads((build/'manifest.json').read_text())
                logical[variant]=manifest['logical_storage_bits']
                result=json.loads((OUT/'runs'/build.name/'result.json').read_text())
                memory_key='block_memory_bits' if platform=='quartus' else 'bram_uram_allocated_bits'
                assert sum(v.get('ram_bits',0) for v in counts.values())==result['resources'][memory_key]
                pair[variant]=counts
                save(OUT/'runs'/build.name/'memory-attribution.json',dict(groups=counts,memories=memories,
                    logical_storage_bits=logical[variant],
                    controller_uses_block_ram=counts['controller_fifo'].get('ram_bits',0)>0,
                    pre_mapper_uses_block_ram=counts['pre_mapper'].get('ram_bits',0)>0))
                for g,resources in counts.items():
                    for resource,value in resources.items():
                        rows.append(dict(platform=platform,vflows=flows,configuration=variant,
                                         component=g,resource=resource,value=value))
            if set(pair)!={'static','replay'}:
                continue
            a,b=pair['static'],pair['replay']
            controller_delta=b['controller_fifo']['ram_bits']-a['controller_fifo']['ram_bits']
            actual=sum(v['ram_bits'] for v in b.values())-sum(v['ram_bits'] for v in a.values())
            mapper=sum(b[g]['ram_bits']-a[g]['ram_bits'] for g in ('pre_mapper','post_mapper'))
            controller_logical_delta=logical['replay']['control_queue_payload']-logical['static']['control_queue_payload']
            assert controller_logical_delta==0
            logical_delta=sum(logical['replay'][g]-logical['static'][g] for g in ('pre_mapper_banks','post_mapper_banks'))
            v=(flows-1).bit_length()
            cam = manifest['hardware'].get('lookup_backend') == 'cam'
            depth = manifest['hardware']['cam_entries_per_bank'] if cam else 8*flows*flows
            expected=5*(flows*v+depth*(v+3))
            assert logical_delta==expected,(flows,logical_delta,expected)
            # Mapping to registers/LUTRAM is a measured outcome, not an audit
            # failure. Keep logical capacity separate from block-memory usage.
            differences.append(dict(platform=platform,vflows=flows,controller_ram_delta=controller_delta,
                controller_logical_storage_delta_bits=controller_logical_delta,
                logical_mapper_storage_delta_bits=logical_delta,
                logical_cam_tag_valid_delta_bits=(logical['replay'].get('cam_tag_valid_registers',0)
                                                  - logical['static'].get('cam_tag_valid_registers',0)),
                mapped_ram_delta=actual,mapper_ram_delta=mapper,
                non_mapper_ram_delta=actual-mapper,
                all_extra_ram_is_mapper_banks=(actual==mapper),
                controller_block_ram_bits={k:pair[k]['controller_fifo']['ram_bits'] for k in pair},
                pre_mapper_block_ram_bits={k:pair[k]['pre_mapper']['ram_bits'] for k in pair}))
    validation=OUT/'validation'
    save(validation/'ram-ports.json',dict(status='passed',cases=records))
    save(validation/'memory-overhead.json',dict(status='passed',cases=differences,
        quartus_unit='inferred implementation bits',vivado_unit='allocated BRAM/URAM bits',
        logic_register_lutram_storage_excluded_from_block_ram_total=True))
    if rows:
        write_csv(validation/'component-resources.csv',rows)
    print('SHARED_FIFO_MEMORY_AUDIT_PASS',len(records),'RTL cases,',len(differences),'native pairs')


if __name__=='__main__':
    main()
