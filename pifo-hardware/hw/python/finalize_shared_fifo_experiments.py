#!/usr/bin/env python3
"""Wait for the synthesis queue, then validate and archive reproducible inputs."""
import csv
from datetime import datetime, timezone
import hashlib
import json
import math
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import time

from pifo_synthesis_results import PROJECT
from pifo_shared_fifo_experiments import OUT, FLOWS, name, save, collect
from validate_shared_fifo_synthesis import main as audit

ROOT=Path('/data/work/rio-synthesis/shared-fifo-34e35d2')


def audit_bram():
    """Cross-check displayed BRAM counts against native primitives / RAM bits."""
    with (OUT/'r2-vflows/bram-resources.csv').open() as stream:
        rows = list(csv.DictReader(stream))
    values = {}
    for row in rows:
        flows, platform, variant = int(row['vflows']), row['platform'], row['configuration']
        amounts = []
        for component in (variant, 'pifo'):
            folder = OUT/'runs'/name(flows,component,platform)
            result = json.loads((folder/'result.json').read_text())
            if platform == 'quartus':
                assert result['part'] == 'AGFB014R24B2E2V'
                amounts.append(result['resources']['block_memory_bits']/20480)
                available = 7110
                assert row['measurement_kind'] == 'inferred_ram_bit_capacity_equivalent'
            else:
                native = json.loads((folder/'resource-summary.json').read_text())['mapped_memory']
                amounts.append(native['ramb36_primitives'] + native['ramb18_primitives']/2)
                available = next(c['available'] for c in result['capacity_checks']
                                 if c['resource'] == 'bram36_tiles')
                assert row['measurement_kind'] == 'mapped_bram36_tile_equivalent'
            assert float(row['available']) == available
        rio, one = amounts
        total = rio + 5*one
        for field, expected in [('rio_only',rio), ('pifo_one',one), ('pifo_total',5*one),
                                ('value',total), ('utilization_percent',100*total/available)]:
            assert math.isclose(float(row[field]),expected,rel_tol=1e-12,abs_tol=1e-9), (row,field)
        assert row['exceeds_capacity'] == str(total > available)
        values[flows,platform,variant] = total
    with (OUT/'r2-vflows/bram-comparison.csv').open() as stream:
        comparisons = list(csv.DictReader(stream))
    for row in comparisons:
        flows, platform = int(row['vflows']), row['platform']
        a, b = [values[flows,platform,v] for v in ('static','replay')]
        available = float(row['available'])
        for field, expected in [('static',a), ('replay',b), ('absolute_change',b-a),
                                ('static_utilization_percent',100*a/available),
                                ('replay_utilization_percent',100*b/available),
                                ('utilization_change_percentage_points',100*(b-a)/available)]:
            assert math.isclose(float(row[field]),expected,rel_tol=1e-12,abs_tol=1e-9), (row,field)
        if a:
            assert math.isclose(float(row['percent_change']),100*(b-a)/a,rel_tol=1e-12)
        else:
            assert row['percent_change'] == ''
    result = dict(status='passed', configuration_rows=len(rows), comparison_rows=len(comparisons),
        native_vivado_primitives_checked=True, quartus_bit_capacity_estimates_labeled=True,
        pifo_included_in_totals=True, target_percentage_distinct_from_overhead=True)
    save(OUT/'validation/bram-accounting.json',result)
    return result


def main():
    while not (OUT/'completion.json').exists():
        save(OUT/'validation/finalization-status.json', dict(status='waiting_for_synthesis',
            checked_utc=datetime.now(timezone.utc).isoformat()))
        time.sleep(20)
    statuses=collect(ROOT)
    assert len(statuses)==36 and all(r['status']=='synthesis_complete' for r in statuses)
    bram_validation = audit_bram()
    assert bram_validation['configuration_rows'] == 24
    assert bram_validation['comparison_rows'] == 12
    audit()
    summary=json.loads((OUT/'validation/memory-overhead.json').read_text())
    assert len(summary['cases'])==12
    with (OUT/'r2-vflows/comparison.csv').open() as stream:
        rows=list(csv.DictReader(stream))
    for row in rows:
        f=int(row['vflows']); p=row['platform']; resource=row['resource']
        result=[json.loads((OUT/'runs'/name(f,v,p)/'result.json').read_text())
                for v in ('static','replay','pifo')]
        a,b,one=[r['resources'][resource] for r in result]
        assert result[0]['source_sha256']==result[1]['source_sha256']==result[2]['source_sha256']
        for key,value in [('static_rio',a),('replay_rio',b),('pifo_one',one),('pifo_total',5*one),
                          ('static',a+5*one),('replay',b+5*one),('absolute_change',b-a)]:
            assert float(row[key])==value,(f,p,resource,key)
        if a+5*one:
            assert abs(float(row['percent_change'])-100*(b-a)/(a+5*one))<1e-10
        else:
            assert row['percent_change']==''
    inputs=[]
    for flows in FLOWS:
        for variant in ('static','replay','pifo'):
            build=ROOT/name(flows,variant,'quartus')
            m=json.loads((build/'manifest.json').read_text())
            vm=json.loads((ROOT/name(flows,variant,'vivado')/'manifest.json').read_text())
            assert m['rtl_sha256']==vm['rtl_sha256']
            target=OUT/'inputs'/build.name
            target.mkdir(parents=True,exist_ok=True)
            archive=target/'rtl.tar.gz'
            if not archive.exists():
                with tarfile.open(archive,'w:gz') as tar:
                    for filename,expected in m['rtl_sha256'].items():
                        src=build/'rtl'/filename
                        with src.open('rb') as stream:
                            actual=hashlib.file_digest(stream,'sha256').hexdigest()
                        assert actual==expected,src
                        tar.add(src,arcname='rtl/'+filename)
            shutil.copy2(build/'manifest.json',target/'manifest.json')
            inputs.append(dict(vflows=flows,configuration=variant,archive=str(archive.relative_to(OUT)),
                               canonical_rtl_identical_across_vendors=True))
    snapshot=OUT/'workflow'
    sources=[]
    for folder in ('hw/spinal/rio','synthesis','hw/python'):
        for path in (PROJECT/folder).rglob('*'):
            if any(x in path.parts for x in ('.tools','__pycache__','build','target')):
                continue
            if path.is_file() and path.suffix in ('.scala','.py','.tcl','.sv'):
                sources.append(path)
    sources += [PROJECT/p for p in ('build.sbt','project/build.properties','project/plugins.sbt',
                                    'synthesis/repositories','synthesis/README.md','hw/verilog/priority_encode_log.v')]
    hashes={}
    for path in sources:
        target=snapshot/path.relative_to(PROJECT)
        target.parent.mkdir(parents=True,exist_ok=True)
        shutil.copy2(path,target)
        hashes[str(path.relative_to(PROJECT))]=hashlib.sha256(path.read_bytes()).hexdigest()
    save(OUT/'workflow/source-sha256.json',hashes)
    save(OUT/'inputs/index.json',inputs)
    shutil.copy2(ROOT/'manager.log',OUT/'logs/synthesis-manager.log')
    save(OUT/'validation/experiment-accounting.json',dict(status='passed',runs=36,
        native_pairs=12,comparison_rows=len(rows),same_fifo_depth=256,
        pifo_included_in_all_percentage_denominators=True,
        bram_counts_and_target_utilization_checked=True,
        source_and_rtl_matched_across_platforms=True,
        all_extra_block_memory_is_mapper_banks=all(r['all_extra_ram_is_mapper_banks'] for r in summary['cases']),
        no_extra_logical_controller_storage=all(r['controller_logical_storage_delta_bits']==0 for r in summary['cases']),
        implementation_run=False))
    save(OUT/'validation/finalization-status.json',dict(status='passed',
        completed_utc=datetime.now(timezone.utc).isoformat()))
    print('SHARED_FIFO_FINALIZATION_PASS',OUT,flush=True)


if __name__=='__main__':
    try:
        main()
    except Exception as error:
        save(OUT/'validation/finalization-status.json',dict(status='failed',error=str(error)))
        raise
