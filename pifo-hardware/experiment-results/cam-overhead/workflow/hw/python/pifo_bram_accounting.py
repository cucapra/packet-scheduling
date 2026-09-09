"""Keep target BRAM utilization separate from replay overhead percentages."""

TARGETS = {
    'quartus': dict(
        part='AGFB014R24B2E2V', label='Agilex 7 AGFB014',
        resource='m20k_equivalents', native_resource='block_memory_bits',
        native_units_per_block=20480, available=7110, unit='M20K equivalents',
        measurement_kind='inferred_ram_bit_capacity_equivalent',
        count_definition='inferred block RAM bits / 20480; packing is not modeled',
        capacity_source='https://docs.altera.com/api/khub/documents/T99La5fz4bf~McCaFB0bPw/content',
        capacity_source_version='2026.08.14, page 1, AGF 014 column',
        block_size_source='https://docs.altera.com/r/docs/683241/25.1.1/agilextm-7-embedded-memory-user-guide/agilextm-7-embedded-memory-features'),
    'vivado': dict(
        part='xcku5p-ffvb676-2-e', label='KCU116 XCKU5P',
        resource='bram36_tiles', native_resource='bram36_tiles',
        native_units_per_block=1, available=480, unit='BRAM36 equivalents',
        measurement_kind='mapped_bram36_tile_equivalent',
        count_definition='RAMB36 primitives + RAMB18 primitives / 2',
        capacity_source='native reports/utilization.rpt: Block RAM Tile / Available'),
}


def bram_pair(flows, platform, ordinary, replay, pifo, sources, pifo_source):
    target = TARGETS[platform]
    assert all(r['part'] == target['part'] for r in (ordinary, replay, pifo))
    available = target['available']
    if platform == 'vivado':
        # Check the device denominator against every native report, including PIFO.
        for result in (ordinary, replay, pifo):
            native = next(c for c in result['capacity_checks'] if c['resource'] == 'bram36_tiles')
            assert native['available'] == available
    divisor = target['native_units_per_block']
    a, b, one = [r['resources'][target['native_resource']] / divisor
                 for r in (ordinary, replay, pifo)]
    common = dict(platform=platform, part=target['part'], vflows=flows,
        resource=target['resource'], unit=target['unit'], pifo_one=one,
        pifo_count=5, pifo_total=5*one, available=available,
        measurement_kind=target['measurement_kind'], count_definition=target['count_definition'],
        capacity_source=target['capacity_source'], pifo_source=pifo_source)
    rows = []
    for variant, value in [('static', a), ('replay', b)]:
        total = value + 5*one
        rows.append(dict(common, configuration=variant, rio_only=value, value=total,
            utilization_percent=100*total/available, exceeds_capacity=total > available,
            source=sources[variant]))
    comparison = dict(common, static_rio=a, replay_rio=b, static=a+5*one, replay=b+5*one,
        absolute_change=b-a, percent_change=100*(b-a)/(a+5*one) if a+5*one else '',
        percent_denominator='ordinary_RIO_plus_five_matching_PIFOs',
        static_utilization_percent=100*(a+5*one)/available,
        replay_utilization_percent=100*(b+5*one)/available,
        utilization_change_percentage_points=100*(b-a)/available,
        static_source=sources['static'], replay_source=sources['replay'])
    return rows, comparison
