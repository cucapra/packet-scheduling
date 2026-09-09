#!/usr/bin/env python3
"""Measure controller replay and compare with preserved static/read-copy runs."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import csv
import json
from pathlib import Path
import subprocess
import sys
import time

from pifo_hardware_overhead import (PROJECT, RESOURCE_NAMES, archive, count_text,
                                    live_runner, adopt, reusable, write_csv)


def name(config, flows, platform):
    h=config["hardware"]
    return f'rio-replay-pe{h["num_engines"]}-v{flows}-c{h["entries_per_pe"]}-j{h["replay_log_depth"]}-{platform}'


def command(config, flows, platform, root):
    h=config["hardware"]
    args=[sys.executable,str(PROJECT/"synthesis/run.py"),"--tool",platform,"--build-root",str(root),
          "--name",name(config,flows,platform),"--configuration","replay","--pifo-backend","external",
          "--engines",str(h["num_engines"]),"--vpifos",str(flows),"--entries-per-pe",str(h["entries_per_pe"]),
          "--priority-bits",str(h["priority_bits"]),"--replay-log-depth",str(h["replay_log_depth"]),
          "--clock-mhz",str(config["clock_mhz"]),"--threads",str(config["threads"]),
          "--vivado-directive",config["vivado_directive"]]
    return args+(["--quartus-compact-init","--license","/data/work/quartus/licenses/LR-187458_License.dat"]
                 if platform=="quartus" else ["--vivado-allow-over-capacity"])


def execute(args, log, flows, minimum_gib=0):
    if flows>=1024 and minimum_gib:
        while True:
            memory=dict(line.split(':',1) for line in Path('/proc/meminfo').read_text().splitlines())
            available=int(memory['MemAvailable'].split()[0])/1024**2
            active=[]
            for process in Path('/proc').glob('[0-9]*'):
                try:
                    command=(process/'cmdline').read_bytes().decode().split('\0')
                    if (any(x.endswith('synthesis/run.py') for x in command) and
                        '--tool' in command and command[command.index('--tool')+1]=='vivado' and
                        '--vpifos' in command and int(command[command.index('--vpifos')+1])>=1024):
                        active.append(process.name)
                except (OSError,UnicodeError,ValueError,IndexError):
                    continue
            if available>=minimum_gib and not active:break
            print(f'Waiting before large replay job: {available:.1f} GiB available; require {minimum_gib}; '
                  f'other large Vivado wrappers: {active}',flush=True)
            time.sleep(15)
    print('Running:', ' '.join(args),flush=True)
    with log.open('w') as stream:
        return subprocess.run(args,cwd=PROJECT,stdout=stream,stderr=subprocess.STDOUT).returncode


def collect(config, root, references, output):
    reference_hashes=json.loads((PROJECT/config['reference_source_snapshot']).read_text())
    statuses=[];rows=[];comparisons=[];capacities=[]
    h=config['hardware']
    for flows in h['vflows']:
        for platform in config['platforms']:
            pair={}
            for variant in ['static','dynamic','replay']:
                build=(root/name(config,flows,platform) if variant=='replay' else
                       references/f'rio-only-pe{h["num_engines"]}-v{flows}-c{h["entries_per_pe"]}-{variant}-{platform}')
                result=archive(build,output/'runs'/build.name,platform)
                result.update(vflows=flows,configuration=variant)
                if result.get('hardware'):
                    rh=result['hardware']
                    for key,want in [('num_engines',h['num_engines']),('num_vpifos_per_pe',flows),
                                     ('shared_entries_per_pe',h['entries_per_pe']),('priority_bits',h['priority_bits']),
                                     ('pifo_backend','external'),('configuration',variant)]:
                        assert rh[key]==want,(build,key)
                    if variant!='replay':assert result['source_sha256']==reference_hashes,build
                    else:assert rh['replay_log_depth']==h['replay_log_depth']
                statuses.append(result);pair[variant]=result
                for check in result.get('capacity_checks',[]):
                    capacities.append({'platform':platform,'vflows':flows,'configuration':variant,**check})
                for resource,value in result['resources'].items():
                    rows.append({'platform':platform,'vflows':flows,'configuration':variant,'resource':resource,
                                 'value':value,'source':f'runs/{build.name}/resource-summary.json'})
            for baseline in ['static','dynamic']:
                a,b=pair[baseline],pair['replay']
                if not a['resources'] or not b['resources']:continue
                for key in ['part','tool_version','clock_target_mhz']:assert a[key]==b[key],key
                for resource,x in a['resources'].items():
                    if resource not in b['resources']:continue
                    y=b['resources'][resource]
                    comparisons.append({'platform':platform,'vflows':flows,'baseline':baseline,'resource':resource,
                                        'baseline_value':x,'replay_value':y,'absolute_change':y-x,
                                        'percent_change':100*(y-x)/x if x else ''})
    (output/'run-status.json').write_text(json.dumps(statuses,indent=2)+'\n')
    write_csv(output/'resources.csv',rows,['platform','vflows','configuration','resource','value','source'])
    write_csv(output/'comparison.csv',comparisons,['platform','vflows','baseline','resource','baseline_value','replay_value','absolute_change','percent_change'])
    write_csv(output/'device-capacity.csv',capacities,['platform','vflows','configuration','resource','used','available','utilization_percent','exceeds_capacity'])


def render(output,config):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.ticker import ScalarFormatter,FuncFormatter
    with (output/'resources.csv').open() as stream:rows=list(csv.DictReader(stream))
    with (output/'comparison.csv').open() as stream:comparisons=list(csv.DictReader(stream))
    statuses=json.loads((output/'run-status.json').read_text())
    h=config['hardware']
    lines=[f'# {config["title"]}','',
           f'Five PEs, fixed 1,024-entry PIFO integration budget per PE; PIFO cores excluded. Global replay log: {h["replay_log_depth"]:,} mapper instructions.',
           'Static uses one ordinary bank. Dynamic uses the original read/copy synchronization. Replay uses two 1R/1W banks and a shared controller log.',
           'The log reserves space at ingress, records only pre/post mapper updates, and replays them in order after an atomic global swap.',
           'Configuration ingress and subsequent commits wait during replay; packet lookups continue. Batches must fit the advertised log credits.',
           'Synthesis only. Large estimates exceed device memory capacities; no implementation or timing-closure claim.','']
    placement = output / 'journal-m20k/run-status.json'
    if placement.exists():
        controls = json.loads(placement.read_text())
        mapped = next((r for r in controls if r.get('case') == 'full-m20k'
                       and r['status'] == 'synthesis_complete'), None)
        if mapped:
            values = mapped['resources']
            lines += [f"A completed [Quartus journal-placement control](journal-m20k/report.md) "
                      f"uses the same core RTL with only the journal assigned to M20K: "
                      f"{values['logic_alms']:,} ALMs, {values['registers']:,} registers, and "
                      f"{values['block_memory_bits']:,} RAM bits at 1,024 IDs. "
                      "The tables and plots below retain the original automatic-placement runs.", '']
    fixed=max(h['vflows'])
    fixed_pairs=[r for r in comparisons if int(r['vflows'])==fixed and r['baseline']=='dynamic']
    if fixed_pairs:
        values={(r['platform'],r['configuration'],r['resource']):float(r['value'])
                for r in rows if int(r['vflows'])==fixed}
        lines += [f'## Fixed {fixed:,}-ID comparison','',
                  'Changes below compare replay with read/copy; the full table also reports changes from ordinary tables.','',
                  '| Platform | Resource | Ordinary | Read/copy | Replay | Replay − read/copy | Change |',
                  '|---|---|---:|---:|---:|---:|---:|']
        for r in fixed_pairs:
            ordinary=values.get((r['platform'],'static',r['resource']))
            percentage=f'{float(r["percent_change"]):+.2f}%' if r['percent_change'] else 'N/A'
            lines.append(f'| {r["platform"]} | {RESOURCE_NAMES[r["resource"]][0]} | '
                         f'{count_text(ordinary) if ordinary is not None else "Incomplete"} | '
                         f'{count_text(float(r["baseline_value"]))} | {count_text(float(r["replay_value"]))} | '
                         f'{count_text(float(r["absolute_change"]),True)} | {percentage} |')
        lines += ['']
    lines += ['## All measured differences','',
           '| Platform | vFlows | Comparison baseline | Resource | Baseline | Replay | Difference | Change |',
           '|---|---:|---|---|---:|---:|---:|---:|']
    for r in comparisons:
        percentage=f'{float(r["percent_change"]):+.2f}%' if r['percent_change'] else 'N/A'
        lines.append(f'| {r["platform"]} | {r["vflows"]} | {r["baseline"]} | {RESOURCE_NAMES[r["resource"]][0]} | '
                     f'{count_text(float(r["baseline_value"]))} | {count_text(float(r["replay_value"]))} | '
                     f'{count_text(float(r["absolute_change"]),True)} | {percentage} |')
    missing=[r for r in statuses if r['status']!='synthesis_complete']
    if missing:
        lines+=['','Incomplete measurements (never interpreted as zero):','']
        lines += [f'- {r["platform"]}, {r["vflows"]} IDs, {r["configuration"]}: `{r["status"]}`.' for r in missing]
    notes=[(result,note) for result in statuses for note in result.get('resource_notes',[])]
    if notes:
        lines += ['', 'Report accounting notes:', '']
        lines += [f'- {result["platform"]}, {result["configuration"]}, {result["vflows"]} IDs: {note}'
                  for result,note in notes]
    from pifo_replay_memory_breakdown import journal_notes
    for note in journal_notes(output, statuses):
        lines += ['', note]
    lines+=['', 'Reference static/read-copy measurements retain their exact archived source snapshot. Replay has a separately hashed source snapshot; device, widths, PE count, external PIFO boundary, clock, and vendor directive match.',
            'The replay log is fixed at the same depth across this sweep. Its cost is therefore more prominent at small table sizes.', '']
    (output/'report.md').write_text('\n'.join(lines))
    for group,metrics in [('logic',{'quartus':'logic_alms','vivado':'logic_luts'}),
                          ('memory',{'quartus':'block_memory_bits','vivado':'bram_uram_allocated_bits'}),
                          ('registers',{'quartus':'registers','vivado':'registers'})]:
        folder=output/'figures'/group;folder.mkdir(parents=True,exist_ok=True)
        data=[r for r in rows if r['resource']==metrics[r['platform']]]
        write_csv(folder/'data.csv',data,['platform','vflows','configuration','resource','value','source'])
        fig,axes=plt.subplots(1,2,figsize=(11,4.3),layout='constrained')
        for ax,platform in zip(axes,['quartus','vivado']):
            for variant,label,color in [('static','Ordinary tables','#52687a'),('dynamic','Read/copy','#a15d18'),('replay','Instruction replay','#007e87')]:
                points={int(r['vflows']):float(r['value']) for r in data if r['platform']==platform and r['configuration']==variant}
                if points:ax.plot(h['vflows'],[points.get(v,float('nan')) for v in h['vflows']],marker='o',label=label,color=color)
            ax.set_xscale('log',base=2);ax.set_xticks(h['vflows']);ax.xaxis.set_major_formatter(ScalarFormatter())
            ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:f'{x:,.0f}'))
            ax.set_xlabel('vFlow / virtual-PIFO ID capacity');ax.set_ylabel(RESOURCE_NAMES[metrics[platform]][0])
            ax.set_title(platform.capitalize());ax.set_ylim(bottom=0);ax.grid(True,alpha=.2)
            if ax.lines:ax.legend(frameon=False)
        fig.suptitle(f'Controller replay: {group} · 5 PEs · PIFO excluded',fontweight='bold')
        for suffix in ['png','svg']:fig.savefig(folder/f'figure.{suffix}',dpi=180,bbox_inches='tight')
        plt.close(fig)
    from pifo_replay_memory_breakdown import collect as collect_memory, render as render_memory
    memory_rows, memory_statuses = collect_memory(output)
    render_memory(output, memory_rows, memory_statuses)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config',type=Path,default=PROJECT/'experiments/hardware-overhead/r4-replay.json')
    parser.add_argument('--build-root',type=Path,default=Path('/data/work/rio-synthesis/hardware-replay'))
    parser.add_argument('--reference-root',type=Path,default=Path('/data/work/rio-synthesis/hardware-overhead'))
    parser.add_argument('--collect-only',action='store_true');parser.add_argument('--render-only',action='store_true')
    args=parser.parse_args();config=json.loads(args.config.read_text());assert config['schema']=='rio-hardware-replay-v1'
    if not args.collect_only and not args.render_only:
        parser.error('This historical separate-journal sweep requires the source/workflow at d5a10e8. '
                     'Use --collect-only or --render-only for saved results.')
    root=args.build_root.resolve();output=PROJECT/config['output_dir'];output.mkdir(parents=True,exist_ok=True)
    if args.render_only:render(output,json.loads((output/'experiment-config.json').read_text()));return
    (output/'experiment-config.json').write_text(json.dumps(config,indent=2)+'\n')
    if not args.collect_only:
        (output/'execution.json').write_text(json.dumps({'argv':sys.argv,'build_root':str(root),'reference_root':str(args.reference_root),
            'jobs_per_vendor':1,'large_run_min_available_gib':config['large_run_min_available_gib'],
            'serialize_large_vivado_wrappers':True},indent=2)+'\n')
        with ThreadPoolExecutor(max_workers=1) as qp,ThreadPoolExecutor(max_workers=1) as vp:
            futures=[]
            for flows in config['hardware']['vflows']:
                qb=root/name(config,flows,'quartus');qa=command(config,flows,'quartus',root)
                qlive=live_runner(qb)
                qdone=reusable(qb,config,flows,'replay','quartus')
                if qlive:
                    assert reusable(qb,config,flows,'replay','quartus',allow_running=True),qb
                elif not qdone:
                    if not reusable(qb,config,flows,'replay','quartus',allow_prepared=True):
                        code=execute(qa+['--prepare-only'],output/(qb.name+'-prepare.log'),flows)
                        if code:raise RuntimeError(f'Preparation failed: {qb}')
                vb=root/name(config,flows,'vivado');va=command(config,flows,'vivado',root)
                live=live_runner(vb)
                if live:
                    assert reusable(vb,config,flows,'replay','vivado',allow_running=True),vb
                    futures.append(vp.submit(adopt,vb,'vivado',live))
                elif not reusable(vb,config,flows,'replay','vivado'):
                    if not reusable(vb,config,flows,'replay','vivado',allow_prepared=True):
                        code=execute(va+['--rtl-from',str(qb),'--prepare-only'],output/(vb.name+'-prepare.log'),flows)
                        if code:raise RuntimeError(f'Preparation failed: {vb}')
                    futures.append(vp.submit(execute,va+['--reuse-rtl'],output/(vb.name+'-run.log'),flows,config['large_run_min_available_gib']))
                # Finish copying canonical RTL before the Quartus wrapper
                # replaces its manifest while starting synthesis.
                if qlive:futures.append(qp.submit(adopt,qb,'quartus',qlive))
                elif not qdone:futures.append(qp.submit(execute,qa+['--reuse-rtl'],output/(qb.name+'-run.log'),flows))
            for f in futures:print('Replay synthesis job finished:',f.result(),flush=True)
    collect(config,root,args.reference_root.resolve(),output);render(output,config)
    statuses=json.loads((output/'run-status.json').read_text())
    if any(r['status']!='synthesis_complete' for r in statuses):raise SystemExit('Some reference/replay measurements are incomplete; see run-status.json')
    print(output)


if __name__=='__main__':main()
