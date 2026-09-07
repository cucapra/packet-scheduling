#!/usr/bin/env python3
"""Check Quartus's inferred mapper port modes and absence of copy-read replicas."""
import argparse
import json
from pathlib import Path
import re

from summarize_quartus import summarize


def check(build):
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
    logs=[r for r in rows if int(r['Port A Depth'])==h['replay_log_depth'] and
          not r['Name'].startswith(('pifoEngines_','xbar|'))]
    assert len(logs)==1,logs
    assert logs[0]['Mode']=='Simple Dual Port',logs[0]
    return {'status':'passed','build':str(build),'hardware':h,'mapper_ram_banks':banks,
            'controller_log':logs[0],'post_mapper_banks_per_pe':2,
            'copy_read_replicas':0,'evidence':'Quartus Synthesis RAM Summary; inferred simple dual-port memories, not a fitted block allocation'}


if __name__=='__main__':
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('build',type=Path);parser.add_argument('output',type=Path)
    args=parser.parse_args();result=check(args.build)
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(result,indent=2)+'\n')
    print('Verified two post-mapper banks per PE, no copy-read replicas, and a simple dual-port replay log.')
