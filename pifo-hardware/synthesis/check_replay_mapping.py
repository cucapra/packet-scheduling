#!/usr/bin/env python3
"""Check Quartus's inferred mapper port modes and absence of copy-read replicas."""
import argparse
import json
from pathlib import Path
import re

from summarize_quartus import summarize


def journal_mapping(report, hardware):
    """Report block RAM or an evidenced register implementation, never assume zero."""
    table = next(rows for name, rows in report['tables'].items() if 'RAM Summary' in name)
    rows = [dict(zip(table[0], row)) for row in table[1:]]
    logs = [row for row in rows
            if re.match(r'streamFifo_\d+\|logic_ram', row['Name'])
            and int(row['Port A Depth']) == hardware['replay_log_depth']]
    declared = hardware['replay_log_depth'] * hardware['replay_instruction_bits']
    if logs:
        assert len(logs) == 1, logs
        assert logs[0]['Mode'] == 'Simple Dual Port', logs[0]
        assert int(logs[0]['Implementation Bits']) == declared, logs[0]
        return {'implementation': 'simple_dual_port_ram', 'declared_bits': declared,
                'ram_instance': logs[0]}
    hierarchy = next(rows for name, rows in report['tables'].items()
                     if 'Resource Utilization by Entity' in name)
    candidates = []
    for cells in hierarchy[1:]:
        row = dict(zip(hierarchy[0], cells))
        if (re.fullmatch(r'streamFifo_\d+', row['Full Hierarchy Name'])
                and int(row['Dedicated Logic Registers'].split()[0]) >= declared
                and int(row['Block Memory Bits']) == 0):
            candidates.append(row)
    assert len(candidates) == 1, ('Journal has neither RAM nor register evidence', candidates)
    return {'implementation': 'logic_and_registers', 'declared_bits': declared,
            'hierarchy': candidates[0],
            'note': 'Journal storage is included in top-level logic/register counts; it is not block RAM.'}


def check(build, require_log_ram=False):
    manifest=json.loads((build/'manifest.json').read_text());h=manifest['hardware']
    assert manifest['status']=='synthesis_complete' and h['configuration']=='replay'
    report=summarize(build/'output_files/pifo.syn.rpt')
    table=next(rows for name,rows in report['tables'].items() if 'RAM Summary' in name)
    rows=[dict(zip(table[0],r)) for r in table[1:]]
    banks=[r for r in rows if re.search(r'pifoEngines_\d+\|(?:enque_enqueMapper|deque_dequeMapper)\|banks_[01]_rtl_',r['Name'])]
    # Some small pre-mappers may map to logic, so require the two deep post
    # banks per PE and audit every RAM-mapped mapper bank that remains.
    post=[r for r in banks if '|deque_dequeMapper|' in r['Name']]
    assert len(post)==2*h['num_engines'], 'Unexpected post-mapper RAM replication'
    for pe in range(h['num_engines']):
        for bank in (0,1):
            selected=[r for r in post if f'pifoEngines_{pe}|deque_dequeMapper|banks_{bank}_rtl_' in r['Name']]
            assert len(selected)==1,(pe,bank)
    for row in banks:
        assert row['Mode']=='Simple Dual Port',row
        assert '_rtl_0|' in row['Name'],row
    log=journal_mapping(report,h)
    if require_log_ram:
        assert log['implementation']=='simple_dual_port_ram',log
    return {'status':'passed','build':str(build),'hardware':h,'mapper_ram_banks':banks,
            'controller_log':log,'post_mapper_banks_per_pe':2,
            'copy_read_replicas':0,'evidence':'Quartus Synthesis RAM Summary; inferred simple dual-port memories, not a fitted block allocation'}


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build',type=Path);parser.add_argument('output',type=Path)
    parser.add_argument('--require-log-ram',action='store_true')
    args=parser.parse_args();result=check(args.build,args.require_log_ram)
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(result,indent=2)+'\n')
    print('Verified two post-mapper banks per PE and no copy-read replicas; journal:',
          result['controller_log']['implementation'])
