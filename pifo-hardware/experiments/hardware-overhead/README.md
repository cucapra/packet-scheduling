# RIO hardware overhead experiments

The primary comparison is ordinary single-bank tables versus controller replay.
Replay is the default RTL and synthesis configuration. Ordinary tables retain
the command controller and consume commits as no-ops. Replay records mapper
updates, swaps both banks, and replays into the shadow before the next commit.
Its global journal holds 16,384 updates; drivers must respect available credits.
Quartus assigns only the journal array to M20K in every replay measurement.

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
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py
.venv/bin/python hw/python/pifo_hardware_overhead_r1.py --collect-only
.venv/bin/python hw/python/pifo_lookup_pipeline.py --render-only

# Verify the arithmetic directly against the archived vendor reports.
.venv/bin/python hw/python/validate_hardware_experiments.py

# Regenerate just the two requested experiment figures from saved CSVs.
.venv/bin/python hw/python/pifo_hardware_overhead_r1.py --render-only
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py --render-only
```

The runners reuse verified archived ordinary measurements and completed replay
references, synthesize missing replay sizes, and measure the PIFO component grid.
The PIFO extractor checks original RTL hashes, unchanged house-PIFO source,
all used runtime ports, table capacity, exact widths, and the push2 tie-off in
every source PE. The extracted core is unchanged; a wrapper reproduces its
integration connections. Full tool setup remains in synthesis/README.md, and
controller protocol details remain in REPLAY.md.
