#!/usr/bin/env python3
"""Synthesize paired shared-FIFO replay/ordinary RIO and matching house PIFOs."""
import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import time

from pifo_synthesis_results import PROJECT, RESOURCE_NAMES, archive
from pifo_shared_fifo_figures import FIGURES_VERSION, write_csv, render
from pifo_bram_accounting import TARGETS, bram_pair

FLOWS = [32, 64, 128, 256, 512, 1024]
OUT = PROJECT / 'experiment-results/shared-fifo-overhead'
CONFIG = dict(schema='rio-shared-fifo-overhead-v1', source_commit='34e35d2',
    configurations=['static', 'replay'], platforms=['quartus', 'vivado'],
    hardware=dict(num_engines=5, vflows=FLOWS, entries_per_pe=1024,
                  priority_bits=8, control_queue_depth=256, pifo_backend='external'),
    clock_mhz=100, threads=8, vivado_directive='RuntimeOptimized',
    plot=dict(dpi=180, version=FIGURES_VERSION), implementation_run=False,
    bram_targets=TARGETS,
    percent_denominator='ordinary_RIO_plus_five_matching_PIFOs')


def save(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(value, indent=2) + '\n')
    temporary.replace(path)


def name(flows, variant, platform):
    if variant == 'pifo':
        return f'pifo-house-v{flows}-c1024-{platform}'
    return f'pe5-v{flows}-c1024-q256-{variant}-{platform}'


def command(root, flows, variant, platform):
    args = [sys.executable, str(PROJECT/'synthesis/run.py'), '--tool', platform,
        '--build-root', str(root), '--name', name(flows, variant, platform),
        '--engines', '5', '--vpifos', str(flows), '--entries-per-pe', '1024',
        '--priority-bits', '8', '--control-queue-depth', '256',
        '--configuration', 'static' if variant == 'pifo' else variant,
        '--pifo-backend', 'house' if variant == 'pifo' else 'external',
        '--component', 'pifo' if variant == 'pifo' else 'rio',
        '--clock-mhz', '100', '--threads', '8',
        '--vivado-directive', 'RuntimeOptimized']
    return args + (['--quartus-compact-init', '--license',
                   '/data/work/quartus/licenses/LR-187458_License.dat']
                  if platform == 'quartus' else ['--vivado-allow-over-capacity'])


def execute(args, log):
    print(datetime.now(timezone.utc).isoformat(), 'RUN', ' '.join(args), flush=True)
    log.parent.mkdir(parents=True, exist_ok=True)
    with log.open('w') as stream:
        result = subprocess.run(args, cwd=PROJECT, stdout=stream, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(f'Command failed ({result.returncode}): {log}')


def reusable(build):
    manifest = build/'manifest.json'
    if not manifest.exists():
        return None
    m = json.loads(manifest.read_text())
    if m['status'] not in ('prepared', 'synthesis_complete'):
        raise RuntimeError(f'Inspect incomplete/live build before reuse: {build}')
    for path, expected in m['source_sha256'].items():
        assert hashlib.sha256((PROJECT/path).read_bytes()).hexdigest() == expected, path
    for path, expected in m['rtl_sha256'].items():
        assert hashlib.sha256((build/'rtl'/path).read_bytes()).hexdigest() == expected, path
    return m['status']


def synthesize(root, flows, variant, platform):
    build = root/name(flows, variant, platform)
    if reusable(build) != 'synthesis_complete':
        execute(command(root, flows, variant, platform)+['--reuse-rtl'],
                OUT/'logs'/(build.name+'-run.log'))
    result = archive(build, OUT/'runs'/build.name, platform)
    assert result['status'] == 'synthesis_complete', result
    print('COMPLETE', build.name, json.dumps(result['resources']), flush=True)
    return build.name


def collect(root):
    save(OUT/'experiment-config.json', CONFIG)
    save(OUT/'bram-targets.json', TARGETS)
    statuses, results = [], {}
    for flows in FLOWS:
        for variant in ('static', 'replay', 'pifo'):
            for platform in CONFIG['platforms']:
                build = root/name(flows, variant, platform)
                manifest = build/'manifest.json'
                try:
                    m = json.loads(manifest.read_text())
                    status = m['status']
                except (OSError, ValueError):
                    status = 'not_started'
                statuses.append(dict(vflows=flows, configuration=variant, platform=platform,
                                     status=status, build=str(build)))
                result_path = OUT/'runs'/build.name/'result.json'
                if result_path.exists():
                    result = json.loads(result_path.read_text())
                    if result['status'] == 'synthesis_complete':
                        results[flows, variant, platform] = result
    save(OUT/'run-status.json', statuses)
    comparisons, totals, rio_rows, budgets, capacities = [], [], [], [], []
    bram_resources, bram_comparisons = [], []
    for flows in FLOWS:
        for platform in CONFIG['platforms']:
            if not all((flows, variant, platform) in results for variant in ('static','replay','pifo')):
                continue
            ordinary, replay, pifo = (results[flows, variant, platform]
                                      for variant in ('static','replay','pifo'))
            for key in ('part', 'tool_version', 'clock_target_mhz'):
                assert ordinary[key] == replay[key] == pifo[key], key
            for r in (ordinary, replay):
                h = r['hardware']
                assert h['commit_queue_depth'] == 256 and h['num_engines'] == 5
                assert h['shared_entries_per_pe'] == 1024 and h['global_flow_id_capacity'] == flows
            assert replay['hardware']['separate_replay_journal'] is False
            assert pifo['hardware']['token_bits'] == replay['hardware']['token_bits']
            assert pifo['hardware']['enabled_push_ports'] == 1
            for resource, a in ordinary['resources'].items():
                b, one = replay['resources'][resource], pifo['resources'][resource]
                p = 5*one
                unit = RESOURCE_NAMES[resource][1]
                source = f'../runs/{name(flows, "pifo", platform)}/resource-summary.json'
                budgets.append(dict(platform=platform, vflows=flows, resource=resource,
                    one_pifo=one, pifo_count=5, pifo_total=p, source=source))
                comparisons.append(dict(platform=platform, vflows=flows, resource=resource, unit=unit,
                    static_rio=a, replay_rio=b, pifo_one=one, pifo_count=5, pifo_total=p,
                    static=a+p, replay=b+p, absolute_change=b-a,
                    percent_change=100*(b-a)/(a+p) if a+p else '',
                    percent_denominator=CONFIG['percent_denominator'],
                    measurement_kind='sum_of_synthesized_components'))
                for variant, value, result in [('static',a,ordinary), ('replay',b,replay)]:
                    src = f'../runs/{name(flows, variant, platform)}/resource-summary.json'
                    row = dict(platform=platform,vflows=flows,configuration=variant,
                               resource=resource,unit=unit,value=value,source=src)
                    rio_rows.append(row)
                    totals.append(dict(row, rio_only=value, pifo_one=one, pifo_count=5,
                        pifo_total=p, value=value+p, pifo_source=source,
                        measurement_kind='sum_of_synthesized_components'))
                    cap = next((c for c in result.get('capacity_checks',[]) if c['resource']==resource),None)
                    if cap:
                        available = cap['available']
                        capacities.append(dict(platform=platform,vflows=flows,configuration=variant,
                            resource=resource,used=value+p,available=available,
                            utilization_percent=100*(value+p)/available,exceeds_capacity=value+p>available))
            memory, memory_comparison = bram_pair(flows, platform, ordinary, replay, pifo,
                {v:f'../runs/{name(flows,v,platform)}/resource-summary.json' for v in ('static','replay')},
                f'../runs/{name(flows,"pifo",platform)}/resource-summary.json')
            bram_resources.extend(memory)
            bram_comparisons.append(memory_comparison)
            if platform == 'quartus':
                for row in memory:
                    capacities.append(dict(platform=platform,vflows=flows,configuration=row['configuration'],
                        resource=row['resource'],used=row['value'],available=row['available'],
                        utilization_percent=row['utilization_percent'],exceeds_capacity=row['exceeds_capacity']))
    for experiment, selected in [('r1-fixed',[1024]), ('r2-vflows',FLOWS)]:
        output = OUT/experiment
        config = dict(CONFIG, hardware=dict(CONFIG['hardware'],vflows=selected))
        save(output/'experiment-config.json', config)
        for filename, rows in [('resources.csv',totals),('rio-only-resources.csv',rio_rows),
                               ('comparison.csv',comparisons),('device-capacity.csv',capacities),
                               ('bram-resources.csv',bram_resources),('bram-comparison.csv',bram_comparisons)]:
            data = [r for r in rows if r['vflows'] in selected]
            if data:
                write_csv(output/filename, data)
        complete = all((f,v,p) in results for f in selected
                       for v in ('static','replay','pifo') for p in CONFIG['platforms'])
        marker = output/'figures-complete.json'
        previous = json.loads(marker.read_text()) if marker.exists() else {}
        if complete and previous.get('version') != FIGURES_VERSION:
            render(config, output)
            save(marker, dict(status='complete', version=FIGURES_VERSION))
    if budgets:
        write_csv(OUT/'pifo-component/resources.csv', budgets)
    return statuses


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-root', type=Path, default=Path('/data/work/rio-synthesis/shared-fifo-34e35d2'))
    parser.add_argument('--collect-only', action='store_true')
    args = parser.parse_args()
    root = args.build_root.resolve()
    root.mkdir(parents=True, exist_ok=True)
    OUT.mkdir(parents=True, exist_ok=True)
    save(OUT/'experiment-config.json', CONFIG)
    if args.collect_only:
        collect(root)
        return
    save(OUT/'execution.json', dict(argv=sys.argv, build_root=str(root),
        started_utc=datetime.now(timezone.utc).isoformat(), source_commit='34e35d2',
        jobs=dict(quartus_large=2,vivado_large=1,quartus_small=1,vivado_small=1),
        synthesis_only=True, implementation_run=False))
    snapshot = OUT/'workflow'
    for folder in ('hw/spinal/rio','synthesis','hw/python'):
        for path in (PROJECT/folder).glob('*'):
            if path.is_file() and path.suffix in ('.scala','.py','.tcl'):
                target = snapshot/path.relative_to(PROJECT)
                target.parent.mkdir(parents=True,exist_ok=True)
                shutil.copy2(path,target)
    (OUT/'source.patch').write_text(subprocess.check_output(['git','diff','34e35d2','--','pifo-hardware'],cwd=PROJECT.parent,text=True))
    pools = {('quartus',True):ThreadPoolExecutor(max_workers=2),
             ('vivado',True):ThreadPoolExecutor(max_workers=1),
             ('quartus',False):ThreadPoolExecutor(max_workers=1),
             ('vivado',False):ThreadPoolExecutor(max_workers=1)}
    futures = []
    try:
        # Start the fixed case early; independent pools keep smaller points moving.
        for flows in [32,1024,64,128,256,512]:
            for variant in ('replay','static','pifo'):
                qb = root/name(flows,variant,'quartus')
                if reusable(qb) is None:
                    execute(command(root,flows,variant,'quartus')+['--prepare-only'],
                            OUT/'logs'/(qb.name+'-prepare.log'))
                vb = root/name(flows,variant,'vivado')
                if reusable(vb) is None:
                    execute(command(root,flows,variant,'vivado')+['--rtl-from',str(qb),'--prepare-only'],
                            OUT/'logs'/(vb.name+'-prepare.log'))
                for platform in CONFIG['platforms']:
                    large = flows == 1024 and variant != 'pifo'
                    futures.append(pools[platform,large].submit(synthesize,root,flows,variant,platform))
                collect(root)
        while any(not f.done() for f in futures):
            collect(root)
            time.sleep(20)
        for f in futures:
            f.result()
    finally:
        for pool in pools.values():
            pool.shutdown(wait=True)
        collect(root)
    save(OUT/'completion.json', dict(status='synthesis_complete',runs=len(futures),
        completed_utc=datetime.now(timezone.utc).isoformat(),implementation_run=False))
    print('SHARED_FIFO_EXPERIMENTS_COMPLETE', OUT, flush=True)


if __name__ == '__main__':
    main()
