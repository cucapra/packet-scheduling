#!/usr/bin/env python3
"""Vivado staged-CAM sweep: eight RIO cases and four matching native PIFOs."""
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
import fcntl
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile

from pifo_synthesis_results import PROJECT, RESOURCE_NAMES, archive
from pifo_shared_fifo_figures import write_csv, render
from pifo_bram_accounting import TARGETS, bram_pair
from run import vivado_root, vivado_board_info
from verify_pipelined_mesh_rtl import verify as verify_mesh

FLOWS = [32, 128, 512, 1024]
BOARD = 'kcu116'
OUT = PROJECT / f'experiment-results/cam-pipeline-{BOARD}'
VERSION = f'cam-pipeline-{BOARD}-v1'


def save(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + '.tmp')
    temporary.write_text(json.dumps(data, indent=2) + '\n')
    temporary.replace(path)


def name(flows, mode):
    return (f'pifo-v{flows}-c1024-{BOARD}' if mode == 'pifo'
            else f'pe5-v{flows}-c1024-q256-p128-{mode}-{BOARD}')


def command(root, flows, mode):
    cmd = [sys.executable, str(PROJECT / 'synthesis/run.py'), '--tool', 'vivado',
           '--board', BOARD, '--build-root', str(root), '--name', name(flows, mode),
           '--engines', '5', '--vpifos', str(flows), '--entries-per-pe', '1024',
           '--priority-bits', '8', '--control-queue-depth', '256', '--clock-mhz', '100',
           '--threads', '8', '--vivado-directive', 'RuntimeOptimized',
           '--vivado-allow-over-capacity', '--component', 'pifo' if mode == 'pifo' else 'rio',
           '--configuration', 'static' if mode == 'pifo' else mode,
           '--pifo-backend', 'house' if mode == 'pifo' else 'external']
    if mode != 'pifo':
        cmd += ['--lookup-backend', 'cam-pipelined', '--cam-entries-per-pe', str(2 * flows),
                '--cam-entries-per-stage', '128']
    return cmd


def prepare(root, flows, mode, rtl_from_root):
    cmd = command(root, flows, mode) + ['--generate-only']
    origin = None
    if rtl_from_root:
        prefix = (f'pifo-v{flows}-c1024-' if mode == 'pifo'
                  else f'pe5-v{flows}-c1024-q256-p128-{mode}-')
        candidates = [p for p in rtl_from_root.glob(prefix + '*')
                      if manifest(p).get('rtl_complete')]
        if len(candidates) != 1:
            raise ValueError(f'Expected one prepared RTL build for {prefix}: {candidates}')
        origin = candidates[0]
        cmd += ['--rtl-from', str(origin)]
    build = root / name(flows, mode)
    execute(cmd, OUT / 'logs' / (build.name + '-generate.log'))
    if origin:
        save(build / 'rtl-origin.json', dict(build=str(origin),
            manifest_sha256=hashlib.sha256((origin / 'manifest.json').read_bytes()).hexdigest(),
            manifest=manifest(origin), note='RTL reused; synthesis targets the selected board'))
    verify_inputs(build)


def execute(cmd, log):
    log.parent.mkdir(parents=True, exist_ok=True)
    print('RUN', json.dumps(cmd), flush=True)
    with log.open('w') as stream:
        subprocess.run(cmd, cwd=PROJECT, stdout=stream, stderr=subprocess.STDOUT, check=True)


def manifest(build):
    try:
        return json.loads((build / 'manifest.json').read_text())
    except (OSError, ValueError):
        return {}


def verify_inputs(build):
    m = manifest(build)
    assert m.get('rtl_complete'), build
    for path, digest in m['source_sha256'].items():
        assert hashlib.sha256((PROJECT / path).read_bytes()).hexdigest() == digest, path
    for path, digest in m['rtl_sha256'].items():
        assert hashlib.sha256((build / 'rtl' / path).read_bytes()).hexdigest() == digest, path
    assert m['board_part_matches_target'] and m['board_reference']['board_name'] == BOARD
    if m['hardware']['component'] == 'rio':
        rtl = (build / 'rtl/PifoMesh.v').read_text()
        assert 'BoundedCamMapper' not in rtl and 'cam_srl #' in rtl
        assert 'PipelinedCamMapper' in rtl and 'mapper response credit overflow' in rtl
        assert 'PIFO response credit overflow' in rtl
        save(build / 'rtl-validation.json', verify_mesh(build))
    return m


def archive_vivado(build, output):
    result = archive(build, output, 'vivado')
    if result['status'] == 'synthesis_complete':
        native = json.loads((output / 'resource-summary.json').read_text())['resources']
        for resource, row in [('distributed_ram_luts', 'LUT as Distributed RAM'),
                              ('srl_luts', 'LUT as Shift Register')]:
            # Vivado omits both detail rows when the design uses no LUT memory.
            result['resources'][resource] = native.get(row, {}).get('Used', 0)
        assert (result['resources']['distributed_ram_luts'] + result['resources']['srl_luts']
                == native['LUT as Memory']['Used']), 'Incomplete LUT memory breakdown'
        memory = native['LUT as Memory']
        result['capacity_checks'].append(dict(resource='lutram_luts', used=memory['Used'],
            available=memory['Available'], utilization_percent=100 * memory['Used'] / memory['Available'],
            exceeds_capacity=memory['Used'] > memory['Available']))
        save(output / 'result.json', result)
    return result


def synthesize(root, flows, mode):
    build = root / name(flows, mode)
    verify_inputs(build)
    with (build / '.synthesis.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        if manifest(build)['status'] != 'synthesis_complete':
            try:
                execute(command(root, flows, mode) + ['--reuse-rtl'], OUT / 'logs' / (build.name + '-synthesis.log'))
            except subprocess.CalledProcessError:
                archive_vivado(build, OUT / 'runs' / build.name)
                raise
        result = archive_vivado(build, OUT / 'runs' / build.name)
        assert result['status'] == 'synthesis_complete', result
    print('COMPLETE', build.name, json.dumps(result['resources']), flush=True)
    return build.name


def archive_completed(root):
    """Refresh parsed results without invoking synthesis, including after parser fixes."""
    for flows in FLOWS:
        for mode in ('static', 'replay', 'pifo'):
            build = root / name(flows, mode)
            if manifest(build).get('status') == 'synthesis_complete':
                verify_inputs(build)
                result = archive_vivado(build, OUT / 'runs' / build.name)
                assert result['status'] == 'synthesis_complete', result


def collect(root):
    board = vivado_board_info(vivado_root(None), BOARD)
    assert board, f'{BOARD} board definition is required'
    results, statuses = {}, []
    for flows in FLOWS:
        for mode in ('static', 'replay', 'pifo'):
            build = root / name(flows, mode)
            m = manifest(build)
            statuses.append(dict(vflows=flows, configuration=mode, platform='vivado',
                                 status=m.get('status', 'not_started'), build=str(build)))
            path = OUT / 'runs' / build.name / 'result.json'
            if path.exists():
                r = json.loads(path.read_text())
                if r['status'] == 'synthesis_complete':
                    results[flows, mode] = r
    save(OUT / 'run-status.json', statuses)
    label = {'kcu116': 'KCU116 XCKU5P', 'vcu118': 'VCU118 XCVU9P'}[BOARD]
    target = dict(TARGETS['vivado'], part=board['part'], label=label, available=None)
    if results:
        capacities = {next(c['available'] for c in r['capacity_checks'] if c['resource'] == 'bram36_tiles')
                      for r in results.values()}
        assert len(capacities) == 1
        target['available'] = capacities.pop()
    config = dict(schema=VERSION, source_commit=subprocess.check_output(
        ['git', 'rev-parse', 'HEAD'], cwd=PROJECT, text=True).strip(),
        source_identity='commit plus workflow/source-sha256.json and source.patch',
        configurations=['static', 'replay'], platforms=['vivado'], board_reference=board,
        hardware=dict(num_engines=5, vflows=FLOWS, entries_per_pe=1024, priority_bits=8,
                      control_queue_depth=256, pifo_backend='external', lookup_backend='cam-pipelined',
                      cam_entries_per_bank='2 * vflows', cam_entries_per_stage=128,
                      cam_implementation='alexforencich_srl_staged',
                      rank_dependency_interlock='one_inflight_request_per_vpifo'),
        bram_targets={'vivado': target}, clock_mhz=100, threads=8,
        vivado_directive='RuntimeOptimized', rio_synthesis_runs=8, pifo_synthesis_runs=4,
        implementation_run=False, plot=dict(dpi=180, version=VERSION),
        percent_denominator='ordinary_RIO_plus_five_matching_PIFOs')
    save(OUT / 'experiment-config.json', config)
    save(OUT / 'bram-targets.json', config['bram_targets'])
    totals, rio_rows, comparisons, capacity_rows, memory_rows, memory_comparisons, budgets = [], [], [], [], [], [], []
    for flows in FLOWS:
        if not all((flows, m) in results for m in ('static', 'replay', 'pifo')):
            continue
        ordinary, replay, pifo = (results[flows, m] for m in ('static', 'replay', 'pifo'))
        for key in ('part', 'tool_version', 'clock_target_mhz'):
            assert ordinary[key] == replay[key] == pifo[key]
        assert ordinary['part'] == board['part']
        assert ordinary['source_sha256'] == replay['source_sha256']
        for r in (ordinary, replay):
            h = r['hardware']
            assert h['num_engines'] == 5 and h['commit_queue_depth'] == 256
            assert h['global_flow_id_capacity'] == flows and h['shared_entries_per_pe'] == 1024
            assert h['lookup_backend'] == 'cam-pipelined' and h['cam_entries_per_bank'] == 2 * flows
            assert h['cam_entries_per_stage'] == 128 and h['mesh_lookup_backpressure']
        assert replay['hardware']['separate_replay_journal'] is False
        assert pifo['hardware']['enabled_push_ports'] == 1 and pifo['hardware']['token_bits'] == replay['hardware']['token_bits']
        source = f'../runs/{name(flows, "pifo")}/resource-summary.json'
        for resource, a in ordinary['resources'].items():
            b, one = replay['resources'][resource], pifo['resources'][resource]
            unit = RESOURCE_NAMES[resource][1]
            comparisons.append(dict(platform='vivado', vflows=flows, resource=resource, unit=unit,
                static_rio=a, replay_rio=b, pifo_one=one, pifo_count=5, pifo_total=5 * one,
                static=a + 5 * one, replay=b + 5 * one, absolute_change=b - a,
                percent_change=100 * (b - a) / (a + 5 * one) if a + 5 * one else '',
                percent_denominator=config['percent_denominator'], measurement_kind='sum_of_synthesized_components'))
            budgets.append(dict(platform='vivado', vflows=flows, resource=resource, one_pifo=one,
                                pifo_count=5, pifo_total=5 * one, source=source))
            for mode, value, result in [('static', a, ordinary), ('replay', b, replay)]:
                row = dict(platform='vivado', vflows=flows, configuration=mode, resource=resource,
                           unit=unit, value=value, source=f'../runs/{name(flows, mode)}/resource-summary.json')
                rio_rows.append(row)
                totals.append(dict(row, rio_only=value, pifo_one=one, pifo_count=5, pifo_total=5 * one,
                                   value=value + 5 * one, pifo_source=source,
                                   measurement_kind='sum_of_synthesized_components'))
                cap = next((c for c in result['capacity_checks'] if c['resource'] == resource), None)
                if cap:
                    capacity_rows.append(dict(platform='vivado', vflows=flows, configuration=mode,
                        resource=resource, used=value + 5 * one, available=cap['available'],
                        utilization_percent=100 * (value + 5 * one) / cap['available'],
                        exceeds_capacity=value + 5 * one > cap['available']))
        mem, change = bram_pair(flows, 'vivado', ordinary, replay, pifo,
            {m: f'../runs/{name(flows, m)}/resource-summary.json' for m in ('static', 'replay')}, source, target)
        memory_rows.extend(mem)
        memory_comparisons.append(change)
    for experiment, flows in [('r1-fixed', [1024]), ('r2-vflows', FLOWS)]:
        output = OUT / experiment
        cfg = dict(config, hardware=dict(config['hardware'], vflows=flows))
        save(output / 'experiment-config.json', cfg)
        selected_tables = {}
        for filename, rows in [('resources.csv', totals), ('rio-only-resources.csv', rio_rows),
                               ('comparison.csv', comparisons), ('device-capacity.csv', capacity_rows),
                               ('bram-resources.csv', memory_rows), ('bram-comparison.csv', memory_comparisons)]:
            selected = [r for r in rows if r['vflows'] in flows]
            selected_tables[filename] = selected
            if selected:
                write_csv(output / filename, selected)
        if all((f, m) in results for f in flows for m in ('static', 'replay', 'pifo')):
            fingerprint = hashlib.sha256(json.dumps([cfg, selected_tables], sort_keys=True).encode()).hexdigest()
            marker = output / 'figures-complete.json'
            previous = json.loads(marker.read_text()) if marker.exists() else {}
            if previous.get('input_sha256') != fingerprint:
                render(cfg, output)
                save(marker, dict(status='complete', version=VERSION, input_sha256=fingerprint))
    if budgets:
        write_csv(OUT / 'pifo-component/resources.csv', budgets)
    return statuses


def archive_inputs_and_workflow(root):
    inputs = []
    rtl_checks = []
    for flows in FLOWS:
        for mode in ('static', 'replay', 'pifo'):
            build = root / name(flows, mode)
            m = verify_inputs(build)
            target = OUT / 'inputs' / build.name
            target.mkdir(parents=True, exist_ok=True)
            shutil.copy2(build / 'manifest.json', target / 'manifest.json')
            if (build / 'rtl-origin.json').exists():
                shutil.copy2(build / 'rtl-origin.json', target / 'rtl-origin.json')
            if mode != 'pifo':
                rtl_checks.append(json.loads((build / 'rtl-validation.json').read_text()))
                shutil.copy2(build / 'rtl-validation.json', target / 'rtl-validation.json')
            with tarfile.open(target / 'rtl.tar.gz', 'w:gz') as tar:
                for filename in sorted(m['rtl_sha256']):
                    tar.add(build / 'rtl' / filename, arcname='rtl/' + filename)
            inputs.append(dict(vflows=flows, configuration=mode, archive=str((target / 'rtl.tar.gz').relative_to(OUT))))
    snapshot = OUT / 'workflow'
    paths = [p for folder in ('hw/spinal/rio', 'hw/verilog/vendor/verilog-cam', 'synthesis', 'hw/python')
             for p in (PROJECT / folder).rglob('*') if p.is_file()
             and not any(x in p.parts for x in ('.tools', '__pycache__', 'build', 'results'))
             and (p.suffix in ('.scala', '.v', '.sv', '.py', '.tcl') or p.name in ('COPYING', 'UPSTREAM.json', 'README.md'))]
    paths += [PROJECT / p for p in ('build.sbt', 'project/build.properties', 'project/plugins.sbt',
                                  'synthesis/repositories', 'hw/verilog/priority_encode_log.v')]
    hashes = {}
    for path in paths:
        target = snapshot / path.relative_to(PROJECT)
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, target)
        hashes[str(path.relative_to(PROJECT))] = hashlib.sha256(path.read_bytes()).hexdigest()
    save(snapshot / 'source-sha256.json', hashes)
    save(OUT / 'inputs/index.json', inputs)
    save(OUT / 'validation/rtl-validation.json', dict(status='passed', cases=rtl_checks))
    board = vivado_board_info(vivado_root(None), BOARD)
    shutil.copy2(board['definition'], OUT / f'{BOARD}-board.xml')
    base = json.loads((OUT / 'execution.json').read_text())['source_commit']
    (OUT / 'source.patch').write_text(subprocess.check_output(['git', 'diff', base, '--',
        'pifo-hardware/hw', 'pifo-hardware/synthesis', 'pifo-hardware/.gitignore'], cwd=PROJECT.parent, text=True))
    for path in root.glob('*.log'):
        target = OUT / 'logs' / path.name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(path, target)
    return board


def finalize(root):
    statuses = collect(root)
    assert len(statuses) == 12 and all(r['status'] == 'synthesis_complete' for r in statuses)
    board = archive_inputs_and_workflow(root)
    save(OUT / 'validation/license-status.json', dict(status='validated_by_successful_synthesis',
        target=board['part'], completed_cases=12, license_contents_recorded=False))
    save(OUT / 'validation/experiment-accounting.json', dict(status='passed', rio_runs=8,
        native_pifo_runs=4, same_native_target=board['part'], flow_sizes=FLOWS,
        same_controller_fifo_depth=256, separate_replay_journal=False, stage_entries=128,
        sources_and_rtl_hashes_checked=True, pifo_included_in_all_percentage_denominators=True,
        bram_counts_and_target_utilization_checked=True, implementation_run=False))
    save(OUT / 'completion.json', dict(status='synthesis_complete', rio_runs=8, pifo_runs=4,
        completed_utc=datetime.now(timezone.utc).isoformat(), implementation_run=False))


def main():
    global BOARD, OUT, VERSION
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--board', choices=('kcu116', 'vcu118'), default='kcu116')
    parser.add_argument('--build-root', type=Path)
    parser.add_argument('--output', type=Path)
    parser.add_argument('--rtl-from-root', type=Path,
                        help='Reuse verified RTL from another prepared staged-CAM sweep')
    parser.add_argument('--collect-only', action='store_true', help='Reparse completed native reports and refresh figures without synthesis')
    parser.add_argument('--finalize-only', action='store_true', help='Reparse and archive a completed sweep without synthesis')
    parser.add_argument('--generate-only', action='store_true', help='Prepare all twelve RTL inputs without synthesis')
    parser.add_argument('--retry-failed', action='store_true', help='Archive failed attempts and retry after their cause is fixed')
    parser.add_argument('--jobs', type=int, default=4)
    args = parser.parse_args()
    BOARD = args.board
    OUT = (args.output or PROJECT / f'experiment-results/cam-pipeline-{BOARD}').resolve()
    VERSION = f'cam-pipeline-{BOARD}-v1'
    root = (args.build_root or Path(f'/data/work/rio-synthesis/cam-pipeline-{BOARD}-20260909')).resolve()
    rtl_from_root = args.rtl_from_root.resolve() if args.rtl_from_root else None
    if args.jobs < 1:
        parser.error('--jobs must be positive')
    root.mkdir(parents=True, exist_ok=True)
    if args.collect_only:
        archive_completed(root)
        collect(root)
        return
    if args.finalize_only:
        archive_completed(root)
        finalize(root)
        return
    with (root / '.manager.lock').open('w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        save(OUT / 'execution.json', dict(argv=sys.argv, build_root=str(root), jobs=args.jobs,
            board=BOARD, rtl_from_root=str(rtl_from_root) if rtl_from_root else None,
            started_utc=datetime.now(timezone.utc).isoformat(), synthesis_only=True,
            source_commit=subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=PROJECT, text=True).strip()))
        if args.generate_only:
            for flows in reversed(FLOWS):
                for mode in ('replay', 'static', 'pifo'):
                    build = root / name(flows, mode)
                    if manifest(build).get('rtl_complete'):
                        verify_inputs(build)
                    else:
                        assert manifest(build).get('status', 'preparing') == 'preparing', build
                        prepare(root, flows, mode, rtl_from_root)
            collect(root)
            archive_inputs_and_workflow(root)
            save(OUT / 'preparation.json', dict(status='rtl_prepared', rio_cases=8, pifo_cases=4,
                generated_utc=datetime.now(timezone.utc).isoformat(), synthesis_complete=False))
            print('RTL_PREPARED', str(OUT), flush=True)
            return
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            pending = []
            for flows in reversed(FLOWS):
                for mode in ('replay', 'static', 'pifo'):
                    # Surface a licensing/tool failure before preparing the rest
                    # of a sweep that would encounter the same failure.
                    for future in pending:
                        if future.done():
                            future.result()
                    build = root / name(flows, mode)
                    m = manifest(build)
                    if m:
                        if args.retry_failed and m['status'].endswith('_failed'):
                            attempt = OUT / 'failed-attempts' / datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%SZ') / build.name
                            archive_vivado(build, attempt)
                        else:
                            assert m['status'] in ('generated', 'synthesis_complete'), (build, m['status'])
                        verify_inputs(build)
                    else:
                        prepare(root, flows, mode, rtl_from_root)
                    pending.append(pool.submit(synthesize, root, flows, mode))
            errors = []
            for future in as_completed(pending):
                try:
                    future.result()
                except Exception as error:
                    errors.append(str(error))
                    print('FAILED', str(error), flush=True)
                collect(root)
            if errors:
                raise RuntimeError(errors)
        finalize(root)
        print('SWEEP_COMPLETE', str(OUT), flush=True)


if __name__ == '__main__':
    main()
