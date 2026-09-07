#!/usr/bin/env python3
"""Check R1/R2 arithmetic against archived native Quartus and Vivado reports."""
import csv
import gzip
import hashlib
import json
from pathlib import Path
import sys

from pifo_hardware_overhead import PROJECT, resources
sys.path.insert(0, str(PROJECT/'synthesis'))
from check_replay_mapping import check

BASE = PROJECT/'experiment-results/hardware-overhead'


def read_csv(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    cache = {}
    def native(source, platform):
        source = source.resolve()
        if source not in cache:
            values, _ = resources(source.parent, platform)
            result = json.loads((source.parent/'result.json').read_text())
            assert result['status'] == 'synthesis_complete', source
            assert values == result['resources'], source
            hardware = result['hardware']
            if hardware.get('component') == 'house_pifo':
                manifest = json.loads((source.parent/'manifest.json').read_text())
                proof = manifest['component_extraction']
                assert proof['core_rtl_unchanged'] and proof['empty_and_drain_logic_included']
                assert proof['storage_entries'] == 1024 and proof['source_mesh_push2_tieoffs'] == 5
                assert hardware['enabled_push_ports'] == 1 and hardware['push2_valid'] == 0
                rtl = gzip.decompress((source.parent/'pifo-component.v.gz').read_bytes())
                assert hashlib.sha256(rtl).hexdigest() == manifest['rtl_sha256']['PifoMesh.v']
                assert digest(source.parent/'priority_encode_log.v') == manifest['rtl_sha256']['priority_encode_log.v']
            elif platform == 'quartus' and hardware['configuration'] == 'replay':
                assert check(source.parent, True)['copy_read_replicas'] == 0
            cache[source] = values
        return cache[source]

    comparisons = {}
    for name in ('r1-fixed', 'r2-vflows'):
        folder = BASE/name
        config = json.loads((folder/'experiment-config.json').read_text())
        assert config['configurations'] == ['static', 'replay']
        expected = {(p, f, c) for p in config['platforms']
                    for f in config['hardware']['vflows'] for c in config['configurations']}
        status = json.loads((folder/'run-status.json').read_text())
        assert {(r['platform'], r['vflows'], r['configuration']) for r in status} == expected
        assert len(status) == len(expected)
        for r in status:
            assert r['status'] == 'synthesis_complete'
            assert digest(folder/r['source']) == r['source_sha256']
        rows = read_csv(folder/'resources.csv')
        indexed = {}
        for r in rows:
            p, f, c, metric = r['platform'], int(r['vflows']), r['configuration'], r['resource']
            core = native(folder/r['source'], p)[metric]
            pifo = native(folder/r['pifo_source'], p)[metric]
            assert int(r['pifo_count']) == 5
            assert float(r['rio_only']) == core and float(r['pifo_one']) == pifo
            assert float(r['pifo_total']) == 5*pifo and float(r['value']) == core+5*pifo
            key = (p, f, c, metric)
            assert key not in indexed
            indexed[key] = r
        assert {(p, f, c) for p, f, c, _ in indexed} == expected
        data = read_csv(folder/'comparison.csv')
        assert len(data)*2 == len(indexed)
        for r in data:
            key = (r['platform'], int(r['vflows']))
            a = indexed[(*key, 'static', r['resource'])]
            b = indexed[(*key, 'replay', r['resource'])]
            ordinary, replay = float(a['value']), float(b['value'])
            assert float(r['static']) == ordinary and float(r['replay']) == replay
            assert float(r['absolute_change']) == replay-ordinary
            assert r['percent_denominator'] == 'ordinary_RIO_plus_five_PIFOs'
            assert r['measurement_kind'] == 'sum_of_synthesized_components'
            if ordinary:
                assert abs(float(r['percent_change'])-100*(replay-ordinary)/ordinary) < 1e-10
            else:
                assert r['percent_change'] == ''
        comparisons[name] = data
        assert not (folder/'report.md').exists()
    assert comparisons['r1-fixed'] == [r for r in comparisons['r2-vflows'] if r['vflows'] == '1024']
    assert len(cache) == 36, len(cache)  # 24 RIO measurements and 12 PIFO components.

    # Check the approximation against the completed ordinary 32-flow whole mesh.
    # This does not replace measured PIFO costs or alter any plotted point.
    whole = BASE/'whole-mesh/r2-vflows/runs/pe5-v32-c1024-static-quartus'
    whole_values, _ = resources(whole, 'quartus')
    estimated = {r['resource']: float(r['static']) for r in comparisons['r2-vflows']
                 if r['platform'] == 'quartus' and r['vflows'] == '32'}
    calibration = {k: {'whole_mesh': v, 'component_sum': estimated[k],
                       'difference': estimated[k]-v,
                       'percent_difference': 100*(estimated[k]-v)/v if v else None}
                   for k, v in whole_values.items()}
    summary = dict(status='passed', rio_measurements=24, pifo_measurements=12,
                   compared_resources=len(comparisons['r2-vflows']),
                   fixed_point_matches_sweep=True, percentage_denominator_includes_five_pifos=True,
                   quartus_replay_banks_and_journal_checked=True,
                   quartus_32flow_component_sum_calibration=calibration,
                   interpretation='Component totals are estimates; integrated synthesis optimization changes packing and logic cost.',
                   source_sha256={str(p.relative_to(BASE)): digest(p) for p in cache})
    out = BASE/'validation/experiment-accounting.json'
    out.write_text(json.dumps(summary, indent=2)+'\n')
    print('PASS: 24 RIO and 12 PIFO native results; totals, differences, percentages, and RAM ports.')


if __name__ == '__main__':
    main()
