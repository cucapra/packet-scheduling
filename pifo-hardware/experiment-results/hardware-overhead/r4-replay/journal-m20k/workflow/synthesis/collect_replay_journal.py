#!/usr/bin/env python3
"""Archive the journal-only M20K control and its full-core Quartus rerun."""
import argparse
import json
from pathlib import Path
import shutil
import sys

from check_replay_mapping import check

PROJECT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PROJECT / 'hw/python'))
from pifo_hardware_overhead import archive, count_text, write_csv


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--build-root', type=Path, default=Path('/data/work/rio-synthesis/journal-m20k'))
    parser.add_argument('--reference', type=Path, default=Path(
        '/data/work/rio-synthesis/hardware-replay/rio-replay-pe5-v1024-c1024-j16384-quartus'))
    parser.add_argument('--output', type=Path, default=PROJECT / 'experiment-results/hardware-overhead/r4-replay/journal-m20k')
    args = parser.parse_args()
    args.output.mkdir(parents=True, exist_ok=True)
    specs = [('isolated-auto', 'isolated_journal', args.build_root / 'isolated-auto'),
             ('isolated-m20k', 'isolated_journal', args.build_root / 'isolated-m20k'),
             ('full-auto', 'full_core', args.reference),
             ('full-m20k', 'full_core', args.build_root / 'rio-replay-pe5-v1024-c1024-j16384-m20k-quartus')]
    statuses = []
    rows = []
    for name, scope, build in specs:
        target = args.output / 'runs' / name
        result = archive(build, target, 'quartus')
        result.update(case=name, scope=scope)
        statuses.append(result)
        for extra in ['journal-probe-result.json']:
            if (build / extra).exists():
                shutil.copy2(build / extra, target / extra)
        if scope == 'isolated_journal' and (build / 'rtl/PifoMesh.v').exists():
            (target / 'rtl').mkdir(exist_ok=True)
            shutil.copy2(build / 'rtl/PifoMesh.v', target / 'rtl/PifoMesh.v')
        for resource, value in result['resources'].items():
            rows.append(dict(case=name, scope=scope, resource=resource, value=value,
                             source=f'runs/{name}/resource-summary.json'))
        if scope == 'full_core' and result['status'] == 'synthesis_complete':
            validation = check(build, require_log_ram=name == 'full-m20k')
            (target / 'memory-ports.json').write_text(json.dumps(validation, indent=2) + '\n')
    write_csv(args.output / 'resources.csv', rows, ['case', 'scope', 'resource', 'value', 'source'])
    (args.output / 'run-status.json').write_text(json.dumps(statuses, indent=2) + '\n')
    pairs = {result['case']: result for result in statuses}
    comparisons = []
    for scope, a, b in [('isolated_journal', 'isolated-auto', 'isolated-m20k'),
                        ('full_core', 'full-auto', 'full-m20k')]:
        if not pairs[a]['resources'] or not pairs[b]['resources']:
            continue
        for field in ['part', 'tool_version', 'clock_target_mhz', 'source_sha256']:
            assert pairs[a][field] == pairs[b][field], (scope, field)
        for resource, x in pairs[a]['resources'].items():
            y = pairs[b]['resources'][resource]
            comparisons.append(dict(scope=scope, resource=resource, auto_value=x, m20k_value=y,
                                    absolute_change=y-x, percent_change=100*(y-x)/x if x else ''))
    write_csv(args.output / 'comparison.csv', comparisons,
              ['scope', 'resource', 'auto_value', 'm20k_value', 'absolute_change', 'percent_change'])
    lines = ['# Quartus replay journal placement', '',
             'This control uses the same 16,384 × 40-bit instruction journal and the same Agilex 7 '
             'target, eight threads, Balanced synthesis, and 100 MHz constraint as R4. The full-core '
             'case retains five PEs and 1,024 IDs with PIFO cores excluded.', '',
             'The only full-core setting change assigns the journal array to M20K. Canonical RTL, '
             'compact MIF files, mapper tables, and read-during-write behavior are unchanged. '
             'The assignment is scoped to the journal module; it does not force other memories.', '',
             '| Scope | Resource | Automatic | Journal M20K | Difference | Change |',
             '|---|---|---:|---:|---:|---:|']
    for row in comparisons:
        pct = f"{row['percent_change']:+.2f}%" if row['percent_change'] != '' else 'N/A'
        lines.append(f"| {row['scope']} | {row['resource']} | {count_text(row['auto_value'])} | "
                     f"{count_text(row['m20k_value'])} | {count_text(row['absolute_change'], True)} | {pct} |")
    lines += ['', 'The isolated journal results are a mapping control and are not added to or '
              'subtracted from whole-core synthesis totals. The full-core comparison is measured '
              'by rerunning the complete unchanged RTL. These remain synthesis estimates; the dense '
              'tables exceed device memory capacity and no implementation or timing closure was run.', '',
              'The original automatic-mapping result is retained in the main R4 tables. This '
              'separate experiment reports an explicit journal placement setting. Vivado already '
              'maps the 1,024-ID replay journal to 18 BRAM36 tiles; its original result remains applicable.', '']
    incomplete = [r for r in statuses if r['status'] != 'synthesis_complete']
    for result in incomplete:
        lines += [f"- {result['case']}: `{result['status']}`; no completed resource count."]
    (args.output / 'report.md').write_text('\n'.join(lines) + '\n')
    print(f"Archived {len(rows)} resource rows; {len(incomplete)} incomplete cases.")
    if incomplete:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
