#!/usr/bin/env python3
"""Synthesize 16 CAM RIO cases and reuse verified matching PIFO measurements."""
import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
import fcntl
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tarfile
import time

from pifo_synthesis_results import PROJECT, RESOURCE_NAMES, archive
from pifo_shared_fifo_figures import FIGURES_VERSION, write_csv, render
from pifo_bram_accounting import TARGETS, bram_pair

FLOWS = [32, 128, 512, 1024]
OUT = PROJECT / 'experiment-results/cam-overhead'
PIFO_RESULTS = PROJECT / 'experiment-results/shared-fifo-overhead'
DEFAULT_BUILD_ROOT = Path('/data/work/rio-synthesis/cam-shared-fifo-20260909')
SOURCE_COMMIT = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=PROJECT, text=True).strip()
CONFIG = dict(schema='rio-cam-shared-fifo-overhead-v1', source_commit=SOURCE_COMMIT,
    source_identity='commit plus source.patch and workflow/source-sha256.json',
    configurations=['static', 'replay'], platforms=['quartus', 'vivado'],
    hardware=dict(num_engines=5, vflows=FLOWS, entries_per_pe=1024,
                  priority_bits=8, control_queue_depth=256, pifo_backend='external',
                  lookup_backend='cam', cam_entries_per_bank='2 * vflows',
                  cam_implementation='portable_parallel_tags_synchronous_values'),
    clock_mhz=100, threads=8, vivado_directive='RuntimeOptimized',
    plot=dict(dpi=180, version=FIGURES_VERSION), implementation_run=False,
    bram_targets=TARGETS,
    rio_synthesis_runs=16, pifo_measurements='reuse_prior_synthesis_after_RTL_equivalence_check',
    percent_denominator='ordinary_RIO_plus_five_matching_PIFOs')
JOBS = dict(quartus=4, vivado=4)


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
    if variant != 'pifo':
        args += ['--lookup-backend', 'cam', '--cam-entries-per-pe', str(2*flows)]
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
    if m['status'] not in ('generated', 'prepared', 'synthesis_running', 'synthesis_complete'):
        raise RuntimeError(f'Inspect incomplete/live build before reuse: {build}')
    for path, expected in m['source_sha256'].items():
        assert hashlib.sha256((PROJECT/path).read_bytes()).hexdigest() == expected, path
    for path, expected in m['rtl_sha256'].items():
        assert hashlib.sha256((build/'rtl'/path).read_bytes()).hexdigest() == expected, path
    return m['status']


def synthesize(root, flows, variant, platform):
    build = root/name(flows, variant, platform)
    # Prevent two managers from starting the same prepared job.
    with (build/'.synthesis.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        # A previous manager may still own the vendor process. Wait for its
        # result rather than launch a duplicate synthesis of this build.
        while reusable(build) == 'synthesis_running':
            live = False
            for process in Path('/proc').iterdir():
                if not process.name.isdecimal():
                    continue
                try:
                    argv = (process/'cmdline').read_bytes().split(b'\0')
                except OSError:
                    continue
                if (str(PROJECT/'synthesis/run.py').encode() in argv
                        and str(root).encode() in argv and build.name.encode() in argv):
                    live = True
                    break
            if not live:
                # Recheck completion after the process-exit/manifest-write race.
                if reusable(build) == 'synthesis_complete':
                    break
                raise RuntimeError(f'Interrupted build has no live synthesis wrapper: {build}')
            time.sleep(5)
        if reusable(build) != 'synthesis_complete':
            execute(command(root, flows, variant, platform)+['--reuse-rtl'],
                    OUT/'logs'/(build.name+'-run.log'))
        result = archive(build, OUT/'runs'/build.name, platform)
        assert result['status'] == 'synthesis_complete', result
    print('COMPLETE', build.name, json.dumps(result['resources']), flush=True)
    return build.name


def reuse_pifo(root, flows):
    """Compare every generated input byte, ignoring only the Git-hash comment."""
    reference = root/name(flows, 'pifo', 'quartus')
    if not (reference/'manifest.json').exists():
        execute(command(root, flows, 'pifo', 'quartus')+['--generate-only'],
                OUT/'logs'/(reference.name+'-generate-reference.log'))
    reusable(reference)  # A stale generated core must not validate a new source tree.
    current = json.loads((reference/'manifest.json').read_text())
    assert current['rtl_complete']
    normalized = {}
    previous_rtl = PIFO_RESULTS/'inputs'/reference.name/'rtl.tar.gz'
    def strip_git_comment(data):
        return re.sub(rb'(?m)^// Git hash  : [0-9a-f]+\r?\n', b'', data)
    with tarfile.open(previous_rtl, 'r:gz') as tar:
        old = json.loads((PIFO_RESULTS/'runs'/reference.name/'manifest.json').read_text())
        assert old['hardware'] == current['hardware']
        assert set(old['rtl_sha256']) == set(current['rtl_sha256'])
        for filename, expected in old['rtl_sha256'].items():
            a = tar.extractfile('rtl/'+filename).read()
            b = (reference/'rtl'/filename).read_bytes()
            assert hashlib.sha256(a).hexdigest() == expected
            assert hashlib.sha256(b).hexdigest() == current['rtl_sha256'][filename]
            assert strip_git_comment(a) == strip_git_comment(b), filename
            normalized[filename] = hashlib.sha256(strip_git_comment(a)).hexdigest()
    for platform in CONFIG['platforms']:
        source = PIFO_RESULTS/'runs'/name(flows,'pifo',platform)
        result = json.loads((source/'result.json').read_text())
        assert result['status'] == 'synthesis_complete'
        assert result['rtl_sha256'] == old['rtl_sha256']
        target = OUT/'runs'/source.name
        shutil.copytree(source, target, dirs_exist_ok=True)
        save(target/'reuse-verification.json', dict(status='passed', source=str(source),
            source_result_sha256=hashlib.sha256((source/'result.json').read_bytes()).hexdigest(),
            current_reference=str(reference), normalized_rtl_sha256=normalized,
            only_ignored_difference='SpinalHDL Git-hash header comment', synthesis_rerun=False))
    print('REUSED_PIFO', flows, 'both platforms; RTL equivalent', flush=True)


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
                if variant != 'pifo':
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
                assert h['lookup_backend'] == 'cam' and h['cam_entries_per_bank'] == 2*flows
            assert ordinary['source_sha256'] == replay['source_sha256']
            assert json.loads((OUT/'runs'/name(flows,'pifo',platform)/'reuse-verification.json').read_text())['status'] == 'passed'
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
        fingerprint = hashlib.sha256(json.dumps([config, comparisons, bram_comparisons],
                                                sort_keys=True).encode()).hexdigest()
        if complete and (previous.get('version'), previous.get('input_sha256')) != (FIGURES_VERSION, fingerprint):
            render(config, output)
            save(marker, dict(status='complete', version=FIGURES_VERSION, input_sha256=fingerprint))
    if budgets:
        write_csv(OUT/'pifo-component/resources.csv', budgets)
    return statuses


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-root', type=Path, default=DEFAULT_BUILD_ROOT)
    parser.add_argument('--collect-only', action='store_true')
    args = parser.parse_args()
    root = args.build_root.resolve()
    root.mkdir(parents=True, exist_ok=True)
    OUT.mkdir(parents=True, exist_ok=True)
    save(OUT/'experiment-config.json', CONFIG)
    if args.collect_only:
        collect(root)
        return
    for flows in FLOWS:
        reuse_pifo(root, flows)
    history_path = OUT/'execution-attempts.json'
    history = json.loads(history_path.read_text()) if history_path.exists() else []
    if (OUT/'execution.json').exists():
        history.append(json.loads((OUT/'execution.json').read_text()))
    save(history_path, history)
    save(OUT/'execution.json', dict(argv=sys.argv, build_root=str(root),
        started_utc=datetime.now(timezone.utc).isoformat(), source_commit=SOURCE_COMMIT,
        jobs=JOBS,
        synthesis_only=True, implementation_run=False))
    snapshot = OUT/'workflow'
    hashes = {}
    for folder in ('hw/spinal/rio','synthesis','hw/python'):
        for path in (PROJECT/folder).glob('*'):
            if path.is_file() and path.suffix in ('.scala','.py','.tcl'):
                target = snapshot/path.relative_to(PROJECT)
                target.parent.mkdir(parents=True,exist_ok=True)
                shutil.copy2(path,target)
                hashes[str(path.relative_to(PROJECT))] = hashlib.sha256(path.read_bytes()).hexdigest()
    save(snapshot/'source-sha256.json', hashes)
    (OUT/'source.patch').write_text(subprocess.check_output(['git','diff',SOURCE_COMMIT,'--','pifo-hardware'],cwd=PROJECT.parent,text=True))
    pools = {platform:ThreadPoolExecutor(max_workers=count) for platform,count in JOBS.items()}
    futures = []
    try:
        # Start the fixed case early; bounded pools keep both vendors moving.
        for flows in [1024,32,128,512]:
            for variant in ('replay','static'):
                qb = root/name(flows,variant,'quartus')
                if reusable(qb) is None:
                    execute(command(root,flows,variant,'quartus')+['--prepare-only'],
                            OUT/'logs'/(qb.name+'-prepare.log'))
                vb = root/name(flows,variant,'vivado')
                if reusable(vb) is None:
                    execute(command(root,flows,variant,'vivado')+['--rtl-from',str(qb),'--prepare-only'],
                            OUT/'logs'/(vb.name+'-prepare.log'))
                for platform in CONFIG['platforms']:
                    futures.append(pools[platform].submit(synthesize,root,flows,variant,platform))
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
    save(OUT/'completion.json', dict(status='synthesis_complete',runs=len(futures),reused_pifo_measurements=8,
        completed_utc=datetime.now(timezone.utc).isoformat(),implementation_run=False))
    print('CAM_SHARED_FIFO_EXPERIMENTS_COMPLETE', OUT, flush=True)


if __name__ == '__main__':
    main()
