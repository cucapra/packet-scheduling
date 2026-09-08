# RIO hardware overhead experiments

The current implementation defaults to **shared-FIFO replay with 256 controller
entries**. Both platforms and all six flow counts have completed synthesis. See
the [current results](../../experiment-results/shared-fifo-overhead/README.md),
[synthesis workflow](../../synthesis/README.md), and [replay protocol](../../REPLAY.md).
The fixed table and sweep include BRAM counts, target utilization, and five
matching PIFOs in the overhead denominator.

```bash
python hw/python/pifo_shared_fifo_experiments.py --collect-only
```

## Historical separate-journal experiments

The saved comparison is ordinary single-bank tables versus controller replay
from `d5a10e8`, with a 16,384-entry separate journal placed in M20K on Quartus.
Ordinary tables retain the command controller and consume commits as no-ops.
The definitions and results below describe that separate-journal implementation;
the shared-FIFO measurements are stored in the current results linked above.

## Experiments

1. **R1: fixed setup.** Five PEs, 1,024 vFlows, 1,024 shared PIFO entries per PE,
   and eight-bit ranks. A table shows ordinary and replay resource totals,
   absolute difference, and percentage change on both platforms.
2. **R2: vFlow sweep.** Keep five PEs and 1,024 PIFO entries per PE; sweep
   32, 64, 128, 256, 512, and 1,024 IDs. Plot total logic, registers, memory,
   and replay overhead. Each point includes PIFOs with matching port/token widths.
3. **Implementation exploration.** One 131,072 × 10-bit lookup with one- versus
   eight-cycle reads, both accepting one request per cycle. Keep resource counts
   and absolute differences. This isolated pipeline is not integrated into RIO;
   it has no scheduler-overhead percentage or measured frequency improvement.

## PIFO accounting

RIO is synthesized with external PIFO ports. A separate synthesis measures one
unchanged house PIFO at each flow-dependent width, including sorting, storage,
occupancy, empty detection, and drain signaling. Push2 is disabled exactly as in
the RIO PE; inactive payloads are tied to zero. Both versions budget five
identical copies. For each vendor resource independently:

```text
P = 5 × measured resource of one matching PIFO
ordinary_total = ordinary_RIO + P
replay_total = replay_RIO + P
absolute_change = replay_RIO − ordinary_RIO
percent_change = 100 × absolute_change / ordinary_total
```

A zero denominator is N/A. The PIFO contribution can be zero for a particular
resource when synthesis confirms that mapping; the house PIFO stores entries in
registers. Raw payload bits are never substituted for mapped FPGA resources.
Component sums are estimates because integrated optimization can change costs.
The 32-flow whole-minus-RIO residual is not used as a PIFO scaling model.

## Setup and output

Both installed tools use a 100 MHz constraint and eight threads: Quartus Pro
25.3.1 / Agilex 7 AGFB014R24B2E2V / Balanced / virtual data pins; Vivado 2025.2 /
xcku5p-ffvb676-2-e / out-of-context / rebuilt hierarchy / RuntimeOptimized.
Vivado's documented capacity-gate hook allows oversized synthesis estimates.
No implementation is run. The requested 1,024-flow setup is legal RTL, but its
current dense 8 × vFlows² table depth exceeds device memory capacity. Native
ALM/LUT units differ; Quartus inferred RAM bits are not fitted M20K counts.

Outputs follow the repository layout: experiment-config.json, execution.json,
resources.csv, comparison.csv, run-status.json, device-capacity.csv, and
figures/<name>/{data.csv,figure.svg,figure.png}. Percentages include PIFOs.
The rio-only-resources.csv preserves the direct core counts; pifo-component/
contains measured per-PIFO costs, extraction checks, manifests, and vendor logs.
Each PIFO run stores its exact generated RTL as `pifo-component.v.gz`; decompress
it to `rtl/PifoMesh.v` to use the saved project setup. The collector verifies its
uncompressed hash against the synthesis manifest.
Previous raw synthesis and protocol-validation evidence remains available for
reproduction; primary figures contain only ordinary and replay designs.

From pifo-hardware:

```bash
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py --collect-only
.venv/bin/python hw/python/pifo_hardware_overhead_r1.py --collect-only
.venv/bin/python hw/python/pifo_lookup_pipeline.py --render-only

# Verify the arithmetic directly against the archived vendor reports.
.venv/bin/python hw/python/validate_hardware_experiments.py

# Regenerate just the two requested experiment figures from saved CSVs.
.venv/bin/python hw/python/pifo_hardware_overhead_r1.py --render-only
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py --render-only
```

Collection and rendering reuse verified archived ordinary and replay measurements.
Fresh synthesis of these historical experiment definitions requires their
matching source/workflow at `d5a10e8`; current runners reject this incompatible
request before modifying results. The current shared-FIFO experiment uses the
same 256-entry control FIFO capacity for both designs in its separate definition.
The PIFO extractor checks original RTL hashes, unchanged house-PIFO source,
all used runtime ports, table capacity, exact widths, and the push2 tie-off in
every source PE. The extracted core is unchanged; a wrapper reproduces its
integration connections. Full tool setup remains in synthesis/README.md, and
controller protocol details remain in REPLAY.md.
