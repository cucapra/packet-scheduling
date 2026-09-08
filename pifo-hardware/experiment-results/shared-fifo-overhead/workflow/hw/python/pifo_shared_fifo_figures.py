"""Render the fixed table and vFlow sweep using repository figure conventions."""
import csv
from pifo_bram_accounting import TARGETS
from pifo_synthesis_results import RESOURCE_NAMES, count_text

FIGURES_VERSION = 'shared-fifo-bram-capacity-v2'


def write_csv(path, data, fields=None):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open('w', newline='') as stream:
        fields = fields or list(dict.fromkeys(key for row in data for key in row))
        writer = csv.DictWriter(stream, fieldnames=fields, lineterminator='\n')
        writer.writeheader()
        writer.writerows(data)


def read_csv(path):
    with path.open(newline='') as stream:
        return list(csv.DictReader(stream))


def render(config, output):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.ticker import ScalarFormatter, FuncFormatter
    plt.rcParams.update({'font.family':'DejaVu Sans', 'font.size':11, 'svg.fonttype':'none',
        'svg.hashsalt':FIGURES_VERSION, 'axes.spines.top':False, 'axes.spines.right':False})
    h = config['hardware']
    platforms = config['platforms']
    def read_selected(filename):
        return [r for r in read_csv(output/filename)
                if int(r['vflows']) in h['vflows'] and r['platform'] in platforms]
    data = read_selected('resources.csv')
    comparison = read_selected('comparison.csv')
    bram = read_selected('bram-resources.csv')
    bram_comparison = read_selected('bram-comparison.csv')
    capacity = {(r['platform'], r['resource'], r['configuration']): r
                for r in read_selected('device-capacity.csv')}
    groups = {'logic':{'quartus':'logic_alms', 'vivado':'logic_luts'},
              'registers':{'quartus':'registers', 'vivado':'registers'},
              'memory':{p:t['resource'] for p,t in TARGETS.items()}}
    variants = [('static','Ordinary + PIFOs','#52687a','s'),
                ('replay','Replay + PIFOs','#007e87','o')]
    memory_note = 'Quartus: inferred RAM bits / 20,480 (M20K capacity estimate; packing not modeled). Vivado: RAMB36 + RAMB18 / 2.'
    preview = 'Preview · ' if config.get('preview') else ''
    setup = f'{h["num_engines"]} PEs · {h["entries_per_pe"]:,} PIFO entries per PE'

    def save(fig, folder, selected):
        write_csv(folder/'data.csv', selected)
        for suffix in ('png','svg'):
            fig.savefig(folder/f'figure.{suffix}', dpi=config['plot']['dpi'], bbox_inches='tight',
                        metadata={'Date':None} if suffix == 'svg' else None)
        svg = folder/'figure.svg'
        svg.write_text('\n'.join(s.rstrip() for s in svg.read_text().splitlines())+'\n')
        plt.close(fig)

    if len(h['vflows']) == 1:
        selected = [r for r in comparison if r['resource'] in
                    ('logic_alms','logic_aluts','logic_luts','registers')]
        selected += bram_comparison
        order = ['logic_alms','logic_aluts','logic_luts','registers','m20k_equivalents','bram36_tiles']
        selected.sort(key=lambda r:(platforms.index(r['platform']), order.index(r['resource'])))
        for row in selected:
            for variant in ('static','replay'):
                cap = capacity.get((row['platform'],row['resource'],variant), {})
                row.setdefault(variant+'_utilization_percent', cap.get('utilization_percent',''))
                row.setdefault('available', cap.get('available',''))
        def percent(value, signed=False):
            return format(float(value), '+,.2f' if signed else ',.2f')+'%' if value != '' else '—'
        cells = [[r['platform'].capitalize(), RESOURCE_NAMES[r['resource']][0],
                  count_text(float(r['static'])), count_text(float(r['replay'])),
                  count_text(float(r['absolute_change']),True), percent(r['percent_change'],True),
                  percent(r['static_utilization_percent']), percent(r['replay_utilization_percent'])]
                 for r in selected]
        fig, ax = plt.subplots(figsize=(17,5.6))
        ax.axis('off')
        table = ax.table(cellText=cells,
            colLabels=['Platform','Resource','Ordinary + PIFOs','Replay + PIFOs','Absolute change',
                       'Overhead\n% ordinary','Ordinary\n% target','Replay\n% target'],
            colWidths=[.08,.21,.135,.135,.13,.09,.11,.11], cellLoc='right', loc='center')
        table.auto_set_font_size(False)
        table.set_fontsize(10.5)
        table.scale(1,2)
        for (r,c), cell in table.get_celld().items():
            cell.set_edgecolor('#d3dce2')
            if r == 0:
                cell.set_facecolor('#17384a')
                cell.set_text_props(color='white',weight='bold')
                cell.set_height(cell.get_height()*1.35)
            elif r % 2 == 0:
                cell.set_facecolor('#eef3f6')
            if c < 2:
                cell.set_text_props(ha='left')
        fig.suptitle(f'{preview}RIO shared-FIFO replay overhead\n{h["num_engines"]} PEs · '
                     f'{h["vflows"][0]:,} vFlows · {h["entries_per_pe"]:,} PIFO entries per PE',fontweight='bold')
        fig.text(.02,.12,'Totals = RIO synthesis + 5 × measured PIFO. Overhead = difference / ordinary total. '
                 '% target = total / FPGA resource capacity.',fontsize=10)
        fig.text(.02,.078,'BRAM targets: Agilex 7 AGFB014 = 7,110 M20Ks; KCU116 XCKU5P = 480 BRAM36 tiles. '
                 'Values above 100% exceed capacity; no placement or routing.',fontsize=10)
        fig.text(.02,.035,memory_note+'  — = capacity not reported.',fontsize=9)
        save(fig,output/'figures/resource-table',selected)
        return

    def axis(ax):
        ax.set_xscale('log',base=2)
        ax.set_xticks(h['vflows'])
        ax.xaxis.set_major_formatter(ScalarFormatter())
        ax.set_xlabel('vFlow / virtual-PIFO ID capacity')
        ax.grid(True,alpha=.2)
        ax.set_xlim(h['vflows'][0]/1.14,h['vflows'][-1]*1.14)

    def axes_figure():
        fig, axes = plt.subplots(1,len(platforms),figsize=(7*len(platforms),5.6),squeeze=False)
        fig.subplots_adjust(left=.08,right=.97,bottom=.19,top=.76,wspace=.34)
        return fig,axes[0]

    for group, metrics in [*groups.items(), ('bram-utilization',groups['memory'])]:
        is_memory = group in ('memory','bram-utilization')
        selected = [r for r in (bram if is_memory else data) if r['resource'] == metrics[r['platform']]]
        fig, axes = axes_figure()
        for ax, platform in zip(axes,platforms):
            key = 'utilization_percent' if group == 'bram-utilization' else 'value'
            for variant, label, color, marker in variants:
                points = {int(r['vflows']):float(r[key]) for r in selected
                          if r['platform'] == platform and r['configuration'] == variant}
                assert set(points) == set(h['vflows'])
                ax.plot(h['vflows'],[points[f] for f in h['vflows']],color=color,marker=marker,label=label,linewidth=2)
            axis(ax)
            ax.set_title(platform.capitalize())
            ax.set_ylabel(RESOURCE_NAMES[metrics[platform]][0])
            if is_memory:
                target = TARGETS[platform]
                ax.set_title(f'{platform.capitalize()} · {target["label"]}\n'
                             f'Target: {target["available"]:,} {target["unit"]}')
                ax.set_yscale('log')
                limit = 100 if group == 'bram-utilization' else target['available']
                ax.axhline(limit,color='#a15d18',linestyle='--',linewidth=1,label='100% target capacity')
                if group == 'bram-utilization':
                    ax.set_ylabel('BRAM / target BRAM capacity [%] · log scale')
                    ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:f'{x:,.0f}%'))
                else:
                    ax.set_ylabel(RESOURCE_NAMES[metrics[platform]][0]+' · log scale')
                    ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:count_text(x)))
            else:
                ax.set_ylim(bottom=0)
                ax.yaxis.set_major_formatter(FuncFormatter(lambda x,_:f'{x:,.0f}'))
            ax.legend(frameon=False,fontsize=10)
        title = 'BRAM utilization' if group == 'bram-utilization' else ('BRAM count' if group == 'memory' else group)
        fig.suptitle(f'{preview}RIO {title} versus vFlows · {setup}\n'
                     'Includes five measured PIFOs; sum of synthesis components',fontweight='bold')
        if is_memory:
            fig.text(.02,.06,memory_note,fontsize=9)
            fig.text(.02,.02,'Values above the dashed line exceed target BRAM capacity. No placement or routing.',fontsize=9)
        save(fig,output/'figures'/group,selected)

    fig, axes = axes_figure()
    selected = []
    for ax, platform in zip(axes,platforms):
        for (group,metrics), color in zip(groups.items(),['#007e87','#a15d18','#7854a3']):
            points = [r for r in (bram_comparison if group == 'memory' else comparison)
                      if r['platform'] == platform and r['resource'] == metrics[platform]]
            selected += points
            ax.plot([int(r['vflows']) for r in points],[float(r['percent_change']) for r in points],
                    label='BRAM' if group == 'memory' else group.capitalize(),color=color,marker='o')
        axis(ax)
        ax.set_title(platform.capitalize())
        ax.axhline(0,color='#999999',linewidth=.8)
        ax.set_ylabel('(Replay − ordinary) / (ordinary + PIFOs) [%]')
        ax.legend(frameon=False)
    fig.suptitle(f'{preview}Shared-FIFO replay overhead · PIFO cost included in the denominator\n'
                 f'{setup} · sum of synthesis components',fontweight='bold')
    fig.text(.02,.06,memory_note,fontsize=9)
    save(fig,output/'figures/overhead-percent',selected)
