# Shared-FIFO replay synthesis

This workspace is based on `pifo-hardware` commit `34e35d2`. Replay uses the
unchanged shared controller FIFO and remains the default. The controller FIFO
depth defaults to 256 for both replay and ordinary tables.

The synthesis adapter adds an ordinary-table baseline and an external PIFO
boundary. The ordinary baseline writes one mapper bank directly, retains the
command FIFO, drain guard, and routing, and consumes commits without action.
The original replay bank-selection/write logic is preserved. Numeric all-zero
initialization avoids constructing millions of Scala literal objects; it does
not change memory contents, widths, depths, or ports.

## Run the experiments

From `pifo-hardware`, with Python and matplotlib available:

```bash
python hw/python/pifo_shared_fifo_experiments.py
python hw/python/pifo_shared_fifo_experiments.py --collect-only
```

The fixed setup is 5 PEs, 1,024 vFlows, 1,024 PIFO entries per PE, 8-bit ranks,
and a 256-entry controller FIFO. The sweep uses 32, 64, 128, 256, 512, and 1,024
vFlows. Every configuration is synthesized afresh. One matching house PIFO is
also synthesized per width and vendor, including accepted-push notifications.

Results follow `experiment-results/shared-fifo-overhead/{r1-fixed,r2-vflows}`:
configuration JSON, resource/comparison CSVs, and `figures/*/{data.csv,figure.svg,figure.png}`.
Native tool evidence and source snapshots are kept under `runs/` and `workflow/`.
Large RTL, vendor databases, and checkpoints remain in
`/data/work/rio-synthesis/shared-fifo-34e35d2/`.

For each native resource, both totals include five measured PIFOs:

```text
ordinary total = ordinary RIO + 5 × one PIFO
replay total   = replay RIO   + 5 × one PIFO
change (%)    = 100 × (replay RIO − ordinary RIO) / ordinary total
target (%)    = 100 × total BRAM / target BRAM capacity
```

`bram-resources.csv` and `bram-comparison.csv` record BRAM counts and target
utilization separately from overhead. Quartus counts are inferred RAM bits /
20,480, labeled M20K bit-capacity equivalents, with no block packing estimate.
AGFB014 has 7,110 M20Ks according to the
[F-Series product table](https://docs.altera.com/api/khub/documents/T99La5fz4bf~McCaFB0bPw/content).
Vivado counts are mapped RAMB36 + RAMB18 / 2; the native XCKU5P reports provide
the 480-tile capacity. `hw/python/pifo_bram_accounting.py` preserves the capacity
sources and checks the target part. Finalization verifies the Vivado counts
against native primitives and checks both percentage denominators. The fixed
table and sweep figures include these counts and percentages, retaining values
above 100% when estimates exceed capacity.

These are sums of synthesis components. Dense tables at 1,024 vFlows exceed
the selected devices' memory capacity; these runs do not establish fit or timing.

## Detected vendor setup

| Setting | Quartus | Vivado |
|---|---|---|
| Installation | `/data/work/quartus/quartus` | `/data/work/vivado/2025.2/Vivado` |
| Version | Pro 25.3.1 Build 100 | 2025.2 |
| Installed board definition | Agilex F-Series Development Kit | KCU116 |
| Part | AGFB014R24B2E2V | xcku5p-ffvb676-2-e |
| Synthesis settings | Balanced, virtual data pins | Out of context, rebuilt hierarchy, RuntimeOptimized |
| Clock target / threads | 100 MHz / 8 | 100 MHz / 8 |

Quartus uses `/data/work/quartus/licenses/LR-187458_License.dat`. License
contents are not stored in experiment evidence. The Quartus frontend reads a
verified equivalent compact zero-MIF view; canonical generated RTL is retained.
Vivado uses its installed license configuration and the previously validated
process-local capacity-check hook for oversized resource estimates. Neither flow
runs placement, routing, or bitstream generation.

Individual builds can be reproduced with `synthesis/run.py`; the exact commands
are recorded in `logs/`. `--prepare-only` generates the project, `--generate-only`
generates RTL only, and `--rtl-from` / `--reuse-rtl` require matching source,
parameter, and generated-file hashes. `--component pifo` measures one house PIFO
with the same single enabled push port as a PE.
