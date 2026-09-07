#!/usr/bin/env python3
"""R1/R2: ordinary tables versus replay, including measured PIFOs in totals."""
import argparse
from concurrent.futures import ThreadPoolExecutor
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

from pifo_hardware_overhead import PROJECT, RESOURCE_NAMES, archive, resources, count_text

EVIDENCE = PROJECT/'experiment-results/hardware-overhead'
DEFAULT_BUILD = Path('/data/work/rio-synthesis/hardware-replay-final')


def write_csv(path, data, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields or list(data[0]), lineterminator='\n')
        writer.writeheader(); writer.writerows(data)


def read_csv(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def build_name(config, flows, platform):
    h = config['hardware']
    return f'rio-replay-pe{h["num_engines"]}-v{flows}-c{h["entries_per_pe"]}-j{h["replay_log_depth"]}-{platform}'


def reference(config, flows, variant, platform, root):
    if variant == 'static':
        return EVIDENCE/'r2-vflows/runs'/f'rio-only-pe5-v{flows}-c1024-static-{platform}'
    if platform == 'quartus' and flows == 1024:
        return EVIDENCE/'r4-replay/journal-m20k/runs/full-m20k'
    if platform == 'vivado' and flows in (32,128,1024):
        return EVIDENCE/'r4-replay/runs'/build_name(config, flows, platform)
    return root/build_name(config, flows, platform)


def command(config, flows, platform, root):
    h = config['hardware']
    args = [sys.executable, str(PROJECT/'synthesis/run.py'), '--tool', platform,
            '--build-root', str(root), '--name', build_name(config, flows, platform),
            '--configuration', 'replay', '--pifo-backend', 'external',
            '--engines', str(h['num_engines']), '--vpifos', str(flows),
            '--entries-per-pe', str(h['entries_per_pe']), '--priority-bits', str(h['priority_bits']),
            '--replay-log-depth', str(h['replay_log_depth']), '--clock-mhz', str(config['clock_mhz']),
            '--threads', str(config['threads']), '--vivado-directive', config['vivado_directive']]
    return args + (['--quartus-compact-init','--quartus-replay-journal-ramstyle','M20K',
                    '--license','/data/work/quartus/licenses/LR-187458_License.dat']
                   if platform == 'quartus' else ['--vivado-allow-over-capacity'])


def execute(args, log):
    print('Running:', ' '.join(args), flush=True)
    log.parent.mkdir(parents=True, exist_ok=True)
    with log.open('w') as stream:
        subprocess.run(args, cwd=PROJECT, stdout=stream, stderr=subprocess.STDOUT, check=True)


def reusable(build, config, flows, platform):
    if not (build/'manifest.json').exists():
        return False
    m = json.loads((build/'manifest.json').read_text())
    if m['status'] == 'synthesis_running':
        raise RuntimeError(f'Refusing to overwrite live replay synthesis: {build}')
    if m['status'] != 'synthesis_complete':
        raise RuntimeError(f'Inspect incomplete build before retrying: {build}')
    h = m['hardware']
    assert h['configuration'] == 'replay' and h['global_flow_id_capacity'] == flows
    assert h['num_engines'] == config['hardware']['num_engines']
    assert h['replay_log_depth'] == config['hardware']['replay_log_depth']
    if platform == 'quartus':
        assert m['quartus_replay_journal_ramstyle'] == 'M20K'
    for path, expected in m['source_sha256'].items():
        assert hashlib.sha256((PROJECT/path).read_bytes()).hexdigest() == expected, path
    resources(build, platform)
    return True


def synthesize(config, root, selected):
    with ThreadPoolExecutor(max_workers=1) as qp, ThreadPoolExecutor(max_workers=1) as vp:
        futures = []
        for flows in config['hardware']['vflows']:
            if flows not in selected:
                continue
            needed = []
            for platform in config['platforms']:
                src = reference(config, flows, 'replay', platform, root)
                if src.is_relative_to(EVIDENCE):
                    assert json.loads((src/'result.json').read_text())['status'] == 'synthesis_complete'
                    continue
                if not reusable(src, config, flows, platform):
                    needed.append(platform)
            if not needed:
                continue
            qb = root/build_name(config, flows, 'quartus')
            # Prepare a common RTL view and copy it before either vendor starts
            # updating the source build's manifest.
            if 'quartus' in needed:
                execute(command(config, flows, 'quartus', root)+['--prepare-only'], root/(qb.name+'-prepare.log'))
            if 'vivado' in needed:
                vb = root/build_name(config, flows, 'vivado')
                execute(command(config, flows, 'vivado', root)+['--rtl-from',str(qb),'--prepare-only'],
                        root/(vb.name+'-prepare.log'))
                futures.append(vp.submit(execute, command(config, flows, 'vivado', root)+['--reuse-rtl'],
                                         root/(vb.name+'-run.log')))
            if 'quartus' in needed:
                futures.append(qp.submit(execute, command(config, flows, 'quartus', root)+['--reuse-rtl'],
                                         root/(qb.name+'-run.log')))
        for future in futures:
            future.result()


def collect(config, root, output):
    h = config['hardware']
    budget_dir = EVIDENCE/'pifo-component'
    budget = {(r['platform'],int(r['vflows']),r['resource']):r for r in read_csv(budget_dir/'resources.csv')}
    statuses, rio_rows, totals, comparisons, capacities = [], [], [], [], []
    for flows in h['vflows']:
        for platform in config['platforms']:
            pairs = {}
            for variant in ('static','replay'):
                src = reference(config, flows, variant, platform, root)
                if src.is_relative_to(EVIDENCE):
                    result = json.loads((src/'result.json').read_text())
                else:
                    target = EVIDENCE/'replay-sweep/runs'/src.name
                    result = archive(src, target, platform)
                    if platform == 'quartus' and result['status'] == 'synthesis_complete':
                        sys.path.insert(0,str(PROJECT/'synthesis'))
                        from check_replay_mapping import check
                        (target/'memory-ports.json').write_text(json.dumps(check(src,True),indent=2)+'\n')
                    src = target
                assert result['status'] == 'synthesis_complete', src
                rh = result['hardware']
                for key,want in [('configuration',variant),('num_engines',h['num_engines']),
                                 ('global_flow_id_capacity',flows),('shared_entries_per_pe',h['entries_per_pe']),
                                 ('pifo_backend','external'),('priority_bits',h['priority_bits'])]:
                    assert rh[key] == want,(src,key)
                if variant == 'replay':
                    assert rh['replay_log_depth'] == h['replay_log_depth']
                manifest = json.loads((src/'manifest.json').read_text())
                assert manifest['threads'] == config['threads']
                assert result['clock_target_mhz'] == config['clock_mhz']
                source = os.path.relpath(src/'resource-summary.json',output)
                statuses.append(dict(platform=platform,vflows=flows,configuration=variant,
                    status=result['status'],part=result['part'],source=source,
                    source_sha256=hashlib.sha256((src/'resource-summary.json').read_bytes()).hexdigest()))
                pairs[variant] = result
                for resource,value in result['resources'].items():
                    pifo = budget[platform,flows,resource]
                    assert int(pifo['pifo_count']) == h['num_engines']
                    pifo_source = budget_dir/pifo['source']
                    pm = json.loads((pifo_source.parent/'manifest.json').read_text())
                    pr = json.loads((pifo_source.parent/'result.json').read_text())
                    assert pr['status'] == 'synthesis_complete'
                    assert float(pifo['one_pifo']) == pr['resources'][resource]
                    assert float(pifo['pifo_total']) == h['num_engines']*pr['resources'][resource]
                    assert pm['part'] == result['part'] and pm['tool_version'] == result['tool_version']
                    assert pm['clock_target_mhz'] == config['clock_mhz'] and pm['threads'] == config['threads']
                    assert pm['hardware']['token_bits'] == rh['token_bits']
                    assert pm['hardware']['global_flow_id_capacity'] == flows
                    assert pm['hardware']['enabled_push_ports'] == 1 and pm['hardware']['push2_valid'] == 0
                    assert pm['hardware']['shared_entries_per_pe'] == h['entries_per_pe']
                    assert pm['hardware']['priority_bits'] == h['priority_bits']
                    p = float(pifo['pifo_total'])
                    rio_rows.append(dict(platform=platform,vflows=flows,configuration=variant,resource=resource,
                                         unit=RESOURCE_NAMES[resource][1],value=value,source=source))
                    totals.append(dict(platform=platform,vflows=flows,configuration=variant,resource=resource,
                        unit=RESOURCE_NAMES[resource][1],rio_only=value,pifo_one=float(pifo['one_pifo']),
                        pifo_count=h['num_engines'],pifo_total=p,value=value+p,source=source,
                        pifo_source=os.path.relpath(pifo_source,output),
                        measurement_kind='sum_of_synthesized_components'))
                    available = next((c['available'] for c in result.get('capacity_checks',[]) if c['resource']==resource),None)
                    if available is not None:
                        capacities.append(dict(platform=platform,vflows=flows,configuration=variant,resource=resource,
                            used=value+p,available=available,utilization_percent=100*(value+p)/available,
                            exceeds_capacity=value+p>available,measurement_kind='sum_of_synthesized_components'))
            for key in ('part','tool_version','clock_target_mhz'):
                assert pairs['static'][key] == pairs['replay'][key],key
            for resource,a in pairs['static']['resources'].items():
                b = pairs['replay']['resources'][resource]
                pifo = budget[platform,flows,resource]
                p = float(pifo['pifo_total'])
                comparisons.append(dict(platform=platform,vflows=flows,resource=resource,unit=RESOURCE_NAMES[resource][1],
                    static_rio=a,replay_rio=b,pifo_one=float(pifo['one_pifo']),pifo_count=h['num_engines'],pifo_total=p,
                    static=a+p,replay=b+p,absolute_change=b-a,
                    percent_change=100*(b-a)/(a+p) if a+p else '',
                    percent_denominator='ordinary_RIO_plus_five_PIFOs',measurement_kind='sum_of_synthesized_components'))
    write_csv(output/'rio-only-resources.csv',rio_rows)
    write_csv(output/'resources.csv',totals)
    write_csv(output/'comparison.csv',comparisons)
    write_csv(output/'device-capacity.csv',capacities)
    (output/'run-status.json').write_text(json.dumps(statuses,indent=2)+'\n')
    (output/'measurement-notes.json').write_text(json.dumps(dict(
        scope='ordinary RIO versus controller replay; five measured house PIFO components added to both',
        absolute_change='replay_RIO - ordinary_RIO',
        percent_change='100 * (replay_RIO - ordinary_RIO) / (ordinary_RIO + 5 * one_PIFO)',
        component_sum_is_estimate=True, implementation_run=False, device_fit_established=False,
        quartus_memory='inferred implementation bits, not fitted M20K allocation',
        vivado_memory='mapped BRAM/URAM allocation',
        quartus_replay_journal='M20K assignment on every plotted Quartus replay case',
        default_source_changes='EngineConfig and CLI now default to replay; explicit static/replay RTL semantics unchanged',
        default_validation='../validation/default-replay/default-equivalence.json',
        prior_validation='../validation/replay-reference-equivalence.json'),indent=2)+'\n')
    (output/'report.md').unlink(missing_ok=True)


def render(config, output):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.ticker import ScalarFormatter, FuncFormatter
    plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'svg.fonttype':'none',
                         'svg.hashsalt':'rio-replay-with-pifo-v1','axes.spines.top':False,'axes.spines.right':False})
    h = config['hardware']; data = read_csv(output/'resources.csv'); comparison = read_csv(output/'comparison.csv')
    groups = {'logic':{'quartus':'logic_alms','vivado':'logic_luts'},
              'registers':{'quartus':'registers','vivado':'registers'},
              'memory':{'quartus':'block_memory_bits','vivado':'bram36_tiles'}}
    def save(fig, folder, selected):
        write_csv(folder/'data.csv',selected)
        for suffix in ('png','svg'):
            fig.savefig(folder/f'figure.{suffix}',dpi=config['plot']['dpi'],bbox_inches='tight',
                        metadata={'Date':None} if suffix=='svg' else None)
        svg=folder/'figure.svg';svg.write_text('\n'.join(s.rstrip() for s in svg.read_text().splitlines())+'\n')
        plt.close(fig)
    if len(h['vflows']) == 1:
        selected = [r for r in comparison if r['resource'] in {m[r['platform']] for m in groups.values()} or r['resource']=='logic_aluts']
        cells = [[r['platform'].capitalize(),RESOURCE_NAMES[r['resource']][0],count_text(float(r['static'])),
                  count_text(float(r['replay'])),count_text(float(r['absolute_change']),True),
                  f'{float(r["percent_change"]):+.2f}%' if r['percent_change'] else 'N/A'] for r in selected]
        fig,ax=plt.subplots(figsize=(13,4.9));ax.axis('off')
        table=ax.table(cellText=cells,colLabels=['Platform','Resource','Ordinary + PIFOs','Replay + PIFOs','Absolute change','Change'],
                       colWidths=[.10,.25,.18,.18,.17,.12],cellLoc='right',loc='center')
        table.auto_set_font_size(False);table.set_fontsize(10.5);table.scale(1,1.8)
        for (r,c),cell in table.get_celld().items():
            cell.set_edgecolor('#d3dce2')
            if r==0:cell.set_facecolor('#17384a');cell.set_text_props(color='white',weight='bold')
            elif r%2==0:cell.set_facecolor('#eef3f6')
            if c<2:cell.set_text_props(ha='left')
        fig.suptitle('RIO controller replay overhead\n5 PEs · 1,024 vFlows · 1,024 PIFO entries per PE',fontweight='bold')
        fig.text(.02,.045,'Totals = RIO synthesis + 5 × measured PIFO. Percentage = difference / ordinary total.',fontsize=10)
        fig.text(.02,.008,'Component estimates; totals exceed target capacity. Quartus RAM is inferred bits; vendor logic units differ.',fontsize=9)
        save(fig,output/'figures/resource-table',comparison)
        return
    def axis(ax):
        ax.set_xscale('log',base=2);ax.set_xticks(h['vflows']);ax.xaxis.set_major_formatter(ScalarFormatter())
        ax.set_xlabel('vFlow / virtual-PIFO ID capacity');ax.grid(True,alpha=.2)
        ax.set_xlim(h['vflows'][0]/1.14,h['vflows'][-1]*1.14)
    for group,metrics in groups.items():
        fig,axes=plt.subplots(1,2,figsize=(12,4.8),layout='constrained')
        selected=[r for r in data if r['resource']==metrics[r['platform']]]
        for ax,p in zip(axes,config['platforms']):
            for variant,label,color,marker in [('static','Ordinary + PIFOs','#52687a','s'),('replay','Replay + PIFOs','#007e87','o')]:
                points={int(r['vflows']):float(r['value']) for r in selected if r['platform']==p and r['configuration']==variant}
                assert set(points)==set(h['vflows'])
                ax.plot(h['vflows'],[points[f] for f in h['vflows']],color=color,marker=marker,label=label,linewidth=2)
            axis(ax);ax.set_title(p.capitalize());ax.set_ylabel(RESOURCE_NAMES[metrics[p]][0])
            if group=='memory':ax.set_yscale('log');ax.set_ylabel(RESOURCE_NAMES[metrics[p]][0]+' · log scale')
            else:ax.set_ylim(bottom=0);ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:f'{x:,.0f}'))
            ax.legend(frameon=False)
        fig.suptitle(f'RIO {group} versus vFlows · 5 PEs · 1,024 entries per PE\nIncludes five measured PIFOs; sum of synthesis components',fontweight='bold')
        save(fig,output/'figures'/group,selected)
    fig,axes=plt.subplots(1,2,figsize=(12,4.8),layout='constrained')
    selected=[]
    for ax,p in zip(axes,config['platforms']):
        for (group,metrics),color in zip(groups.items(),['#007e87','#a15d18','#7854a3']):
            points=[r for r in comparison if r['platform']==p and r['resource']==metrics[p]];selected+=points
            ax.plot([int(r['vflows']) for r in points],[float(r['percent_change']) for r in points],label=group.capitalize(),color=color,marker='o')
        axis(ax);ax.set_title(p.capitalize());ax.axhline(0,color='#999999',linewidth=.8)
        ax.set_ylabel('(Replay − ordinary) / (ordinary + PIFOs) [%]');ax.legend(frameon=False)
    fig.suptitle('Controller replay overhead · PIFO cost included in the denominator\n5 PEs · 1,024 entries per PE · sum of synthesis components',fontweight='bold')
    save(fig,output/'figures/overhead-percent',selected)


def main(default_case='r2-vflows'):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config',type=Path,default=PROJECT/f'experiments/hardware-overhead/{default_case}.json')
    parser.add_argument('--build-root',type=Path,default=DEFAULT_BUILD)
    parser.add_argument('--only-vflows',help='Synthesize selected points, retaining the full configured result grid')
    parser.add_argument('--synthesize-rio-only',action='store_true',help='Run only the missing replay synthesis queue')
    parser.add_argument('--collect-only',action='store_true')
    parser.add_argument('--render-only',action='store_true')
    args=parser.parse_args();config=json.loads(args.config.read_text())
    assert config['schema']=='rio-hardware-overhead-v2'
    assert config['configurations']==['static','replay']
    h=config['hardware'];assert h['pifo_backend']=='external'
    assert h['num_engines']==5 and h['entries_per_pe']==1024 and h['priority_bits']==8
    selected=set(map(int,args.only_vflows.split(','))) if args.only_vflows else set(h['vflows'])
    assert selected.issubset(h['vflows'])
    root=args.build_root.resolve()
    if not args.render_only:root.mkdir(parents=True,exist_ok=True)
    output=PROJECT/config['output_dir'];output.mkdir(parents=True,exist_ok=True)
    if not args.render_only:
        shutil.copy2(args.config,output/'experiment-config.json')
        (output/'execution.json').write_text(json.dumps(dict(argv=sys.argv,build_root=str(root),
            jobs_per_vendor=1,selected_vflows=sorted(selected),implementation=False),indent=2)+'\n')
    if not args.collect_only and not args.render_only:
        synthesize(config,root,selected)
        if args.synthesize_rio_only:return
        subprocess.run([sys.executable,str(PROJECT/'hw/python/pifo_resource_budget.py')],cwd=PROJECT,check=True)
    if not args.render_only:collect(config,root,output)
    render(config,output)
    print(output,flush=True)


if __name__=='__main__':main()
