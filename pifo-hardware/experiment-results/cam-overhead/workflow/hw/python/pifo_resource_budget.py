#!/usr/bin/env python3
"""Measure an unchanged PIFO component at each mesh width; budget five copies."""
import argparse
import csv
import gzip
from concurrent.futures import ThreadPoolExecutor, as_completed
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import sys
import time

from pifo_hardware_overhead import PROJECT, archive, resources
sys.path.insert(0, str(PROJECT / 'synthesis'))
from run import quartus_root, vivado_root, board_info, vivado_board_info, run


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def write_csv(path, data, fields):
    with path.open('w', newline='') as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator='\n')
        writer.writeheader(); writer.writerows(data)


def extract(source, config, flows):
    manifest = json.loads((source / 'manifest.json').read_text())
    h = manifest['hardware']
    for key, wanted in [('num_engines', config['num_engines']),
                        ('num_vpifos_per_pe', flows),
                        ('shared_entries_per_pe', config['entries_per_pe']),
                        ('priority_bits', config['priority_bits']), ('pifo_backend', 'house')]:
        assert h[key] == wanted, (source, key)
    rtl = source / 'rtl/PifoMesh.v'
    encoder = source / 'rtl/priority_encode_log.v'
    for path in (rtl, encoder):
        assert digest(path) == manifest['rtl_sha256'][path.name], path
    assert digest(PROJECT / 'hw/spinal/rio/ConcurrentPifoRTL.scala') == manifest['source_sha256']['hw/spinal/rio/ConcurrentPifoRTL.scala']
    snapshot = PROJECT/'experiment-results/hardware-overhead/diagnostics/whole-mesh-source-snapshot'
    old_core = snapshot/'hw/spinal/rio/PifoCore.scala'
    assert digest(old_core) == manifest['source_sha256']['hw/spinal/rio/PifoCore.scala']
    core_pattern = r'^abstract class PifoCore\b.*?^}'
    # The current file also contains the external boundary added after this
    # snapshot. The house core's inherited interface is exactly unchanged.
    assert re.search(core_pattern, old_core.read_text(), re.M|re.S).group() == re.search(
        core_pattern, (PROJECT/'hw/spinal/rio/PifoCore.scala').read_text(), re.M|re.S).group()
    assert digest(PROJECT/'hw/spinal/rio/Pifo.scala') == manifest['source_sha256']['hw/spinal/rio/Pifo.scala']
    matches = re.findall(r'^module ConcurrentPifoRTL\b.*?^endmodule', rtl.read_text(), re.M | re.S)
    assert len(matches) == 1
    body = matches[0]
    header = body.split(');', 1)[0]
    assert len(re.findall(r'^\s*(?:input|output)\s+wire\b', header, re.M)) == 20
    for name, width in [('io_push1_payload_priority', config['priority_bits']),
                        ('io_push1_payload_port', h['vpifo_id_bits']),
                        ('io_push1_payload_data', h['token_bits'])]:
        assert re.search(rf'\[{width - 1}:0\]\s+{name}\b', header), name
    for port in ('io_popPortEmpty', 'io_portDrained_valid', 'io_portDrained_payload'):
        assert port in header, port
    for field in ('priority', 'data', 'port'):
        indices = re.findall(rf'^\s*reg\s+\[\d+:0\]\s+pifoArray_(\d+)_{field}\s*;', body, re.M)
        assert set(map(int, indices)) == set(range(config['entries_per_pe'])), field
    assert '$readmemb' not in body and 'engineCAM' not in body
    assert config['enabled_push_ports'] == 1
    tied = re.findall(r"\.io_push2_valid\s*\(1'b0\s*\)", rtl.read_text())
    assert len(tied) == config['num_engines'], 'Expected the RIO push2 tie-off in every PE'
    ports = re.findall(r'\b(input|output)\s+wire\s*(\[\d+:0\])?\s*(\w+)', header)
    public = [(d,w,n) for d,w,n in ports if not n.startswith('io_push2_')]
    assert len(public) == 16
    wrapper = 'module PifoMesh (\n' + ',\n'.join(f'  {d} wire {w} {n}' for d,w,n in public) + '\n);\n'
    connections = [f'    .{n}({n})' for _,_,n in public]
    connections += [f"    .{n}('0)" for _,_,n in ports if n.startswith('io_push2_')]
    wrapper += '  ConcurrentPifoRTL core (\n' + ',\n'.join(connections) + '\n  );\nendmodule\n'
    isolated = wrapper + '\n' + body + '\n'
    assert re.search(r'^module ConcurrentPifoRTL\b.*?^endmodule', isolated, re.M|re.S).group() == body
    proof = {'status': 'passed', 'source_manifest_sha256': digest(source / 'manifest.json'),
             'source_rtl_sha256': digest(rtl), 'module_sha256': hashlib.sha256(body.encode()).hexdigest(),
             'core_rtl_unchanged': True, 'wrapper': 'PifoMesh; push2.valid tied low as in every RIO PE',
             'push2_payload_tied_zero': 'irrelevant while push2.valid is zero',
             'runtime_ports': 16, 'source_mesh_push2_tieoffs': len(tied),
             'storage_entries': config['entries_per_pe'],
             'empty_and_drain_logic_included': True, 'token_bits': h['token_bits'],
             'port_bits': h['vpifo_id_bits'], 'rank_bits': config['priority_bits']}
    return isolated, encoder, manifest, proof


def archive_component(build, target, platform, proof, encoder):
    result = archive(build, target, platform)
    (target/'extraction-validation.json').write_text(json.dumps(proof, indent=2)+'\n')
    rtl = (build/'rtl/PifoMesh.v').read_bytes()
    (target/'pifo-component.v.gz').write_bytes(gzip.compress(rtl, mtime=0))
    assert gzip.decompress((target/'pifo-component.v.gz').read_bytes()) == rtl
    (target/'pifo-component.v').unlink(missing_ok=True)
    shutil.copy2(encoder, target/encoder.name)
    readers = [Path(__file__), PROJECT/'hw/python/pifo_hardware_overhead.py',
               PROJECT/f'synthesis/summarize_{platform}.py']
    (target/'collection.json').write_text(json.dumps(dict(
        status=result['status'], implementation_run=False,
        canonical_rtl_archive='pifo-component.v.gz', compression='gzip; deterministic timestamp',
        reader_sha256={str(p.relative_to(PROJECT)):digest(p) for p in readers}),indent=2)+'\n')
    return result


def execute_case(config, flows, platform, sources, root, output, collect_only=False):
    source = sources / f'pe{config["num_engines"]}-v{flows}-c{config["entries_per_pe"]}-static-quartus'
    body, encoder, reference, proof = extract(source, config, flows)
    name = f'pifo-house-1push-v{flows}-c{config["entries_per_pe"]}-t{proof["token_bits"]}-{platform}'
    build = root / name
    target = output / 'runs' / name
    if collect_only and not (build/'manifest.json').exists():
        raise RuntimeError(f'No completed PIFO component to collect: {build}')
    if (build / 'manifest.json').exists():
        existing = json.loads((build / 'manifest.json').read_text())
        if existing['status'] == 'synthesis_running':
            raise RuntimeError(f'Refusing to overwrite running component: {build}')
        # The first RAM-free Quartus cases completed successfully but the old
        # reader rejected omitted memory rows. Recover only that known reader
        # error, with native success and explicit zero-memory evidence.
        if (platform == 'quartus' and existing['status'] == 'synthesis_failed'
                and existing.get('error') == "'Total block memory bits'"):
            _, parsed = resources(build, platform)
            assert parsed['zero_memory_usage']['block_memory_bits'] == 0
            assert 'Quartus Prime Synthesis was successful. 0 errors' in (build/'synthesis.log').read_text()
            existing['collection_recovery'] = dict(
                original_status=existing['status'], original_error=existing.pop('error'),
                reason='Vendor synthesis succeeded; the old reader did not recognize evidenced zero RAM.',
                reran_synthesis=False, resource_reader_sha256=digest(PROJECT/'synthesis/summarize_quartus.py'),
                native_report_sha256=digest(build/'output_files/pifo.syn.rpt'))
            existing['status'] = 'synthesis_complete'
            (build/'manifest.json').write_text(json.dumps(existing,indent=2)+'\n')
        if existing['status'] == 'synthesis_complete':
            assert existing['component_extraction'] == proof
            assert existing['rtl_sha256']['PifoMesh.v'] == hashlib.sha256(body.encode()).hexdigest()
            for file, expected in existing['rtl_sha256'].items():
                assert digest(build / 'rtl' / file) == expected
            result = archive_component(build, target, platform, proof, encoder)
            return dict(vflows=flows, **result)
        raise RuntimeError(f'Existing incomplete component requires inspection: {build}')
    (build / 'rtl').mkdir(parents=True, exist_ok=False)
    (build / 'rtl/PifoMesh.v').write_text(body)
    shutil.copy2(encoder, build / 'rtl' / encoder.name)
    tool = quartus_root(None) if platform == 'quartus' else vivado_root(None)
    board = board_info(tool) if platform == 'quartus' else vivado_board_info(tool, 'kcu116')
    env = os.environ.copy()
    env['PATH'] = str(tool / 'bin') + os.pathsep + env['PATH']
    if platform == 'quartus':
        env.update(QUARTUS_ROOTDIR=str(tool), LM_LICENSE_FILE='/data/work/quartus/licenses/LR-187458_License.dat')
    manifest = dict(status='preparing', tool=platform, part=board['part'], board_reference=board,
        scope='one unchanged house PIFO with RIO push2 tie-off; empty/drain included',
        clock_target_mhz=config['clock_mhz'], threads=config['threads'], implementation_run=False,
        synthesis_mode='virtual data pins' if platform == 'quartus' else 'out_of_context',
        vivado_directive=config['vivado_directive'], vivado_capacity_check='disabled_for_estimation',
        hardware=dict(component='house_pifo', instances=1, budget_instances=config['num_engines'],
                      global_flow_id_capacity=flows, shared_entries_per_pe=config['entries_per_pe'],
                      priority_bits=config['priority_bits'], token_bits=proof['token_bits'],
                      vpifo_id_bits=proof['port_bits'],enabled_push_ports=1,push2_valid=0),
        source_sha256=reference['source_sha256'], rtl_reused_from=str(source), component_extraction=proof,
        rtl_sha256={p.name:digest(p) for p in (build / 'rtl').iterdir()},
        workflow_sha256={str(Path(__file__).relative_to(PROJECT)):digest(Path(__file__)),
            **{f'synthesis/{file}':digest(PROJECT / 'synthesis' / file)
               for file in ('create_project.tcl','vivado_synth.tcl')}},
        timestamp_utc=datetime.now(timezone.utc).isoformat())
    started = time.monotonic()
    def save():
        manifest['elapsed_seconds'] = round(time.monotonic()-started, 3)
        (build / 'manifest.json').write_text(json.dumps(manifest, indent=2)+'\n')
    print(f'Starting PIFO component: {name}', flush=True)
    save()
    try:
        version = build / f'{platform}-version.txt'
        run([tool / ('bin/quartus_sh' if platform=='quartus' else 'bin/vivado'),
             '--version' if platform=='quartus' else '-version'], build, version, env)
        manifest['tool_version'] = version.read_text().strip()
        if platform == 'quartus':
            run([tool/'bin/quartus_sh', '-t', PROJECT/'synthesis/create_project.tcl', build,
                 board['part'], str(config['threads']), str(1000/config['clock_mhz'])],
                build, build/'project.log', env)
        manifest['status'] = 'synthesis_running'; save()
        command = ([tool/'bin/quartus_syn','pifo'] if platform=='quartus' else
                   [tool/'bin/vivado','-mode','batch','-nojournal','-log',build/'vivado.log',
                    '-source',PROJECT/'synthesis/vivado_synth.tcl','-tclargs',build,board['part'],
                    str(config['threads']),str(1000/config['clock_mhz']),'0',config['vivado_directive'],'1'])
        run(command, build, build/'synthesis.log', env)
        manifest['status'] = 'synthesis_complete'; save()
        values, _ = resources(build, platform)
        print(f'Completed {name}: {values}', flush=True)
    except Exception as error:
        manifest.update(status='synthesis_failed', error=str(error))
    finally:
        manifest['completed_utc'] = datetime.now(timezone.utc).isoformat(); save()
    result = archive_component(build, target, platform, proof, encoder)
    return dict(vflows=flows, **result)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--config', type=Path, default=PROJECT/'experiments/hardware-overhead/pifo-component.json')
    parser.add_argument('--source-root', type=Path, default=Path('/data/work/rio-synthesis/hardware-overhead'))
    parser.add_argument('--build-root', type=Path, default=Path('/data/work/rio-synthesis/pifo-component-single-push'))
    parser.add_argument('--collect-only', action='store_true', help='Verify and collect existing synthesis; never launch a vendor tool')
    args = parser.parse_args(); config = json.loads(args.config.read_text())
    output = PROJECT/config['output_dir']; output.mkdir(parents=True, exist_ok=True)
    shutil.copy2(args.config, output/'experiment-config.json')
    (output/'execution.json').write_text(json.dumps(dict(argv=sys.argv, source_root=str(args.source_root),
        build_root=str(args.build_root), jobs_per_vendor=config['jobs_per_vendor'],
        collect_only=args.collect_only, implementation=False), indent=2)+'\n')
    results=[]
    def publish():
        indexed={(r['platform'],r['vflows']):r for r in results}
        statuses=[indexed.get((p,f),dict(platform=p,vflows=f,status='pending',resources={}))
                  for p in config['platforms'] for f in config['vflows']]
        (output/'run-status.json').write_text(json.dumps(statuses,indent=2)+'\n')
        data=[dict(platform=r['platform'],vflows=r['vflows'],resource=k,one_pifo=v,
                   pifo_count=config['num_engines'],pifo_total=config['num_engines']*v,
                   measurement_kind='sum_of_isolated_components',
                   source=f'runs/{Path(r["build"]).name}/resource-summary.json')
              for r in sorted(results,key=lambda r:(r['platform'],r['vflows'])) for k,v in r['resources'].items()]
        write_csv(output/'resources.csv',data,['platform','vflows','resource','one_pifo','pifo_count','pifo_total','measurement_kind','source'])
    publish()
    with ThreadPoolExecutor(max_workers=config['jobs_per_vendor']['quartus']) as qp, \
            ThreadPoolExecutor(max_workers=config['jobs_per_vendor']['vivado']) as vp:
        pools={'quartus':qp,'vivado':vp}
        futures=[pools[p].submit(execute_case,config,f,p,args.source_root,args.build_root,output,args.collect_only)
                 for p in config['platforms'] for f in config['vflows']]
        for future in as_completed(futures):
            results.append(future.result());publish()
    if any(r['status'] != 'synthesis_complete' for r in results):
        raise SystemExit('Some PIFO component measurements failed; see run-status.json')


if __name__ == '__main__':
    main()
