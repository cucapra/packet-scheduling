# Shared-FIFO replay synthesis

Portable bounded CAM lookups with shared-FIFO replay are the defaults in the
hardware configuration, RTL generator, and synthesis CLI. The controller FIFO
depth defaults to 256 for both replay and ordinary tables. Dense lookup tables
remain available with `--lookup-backend dense`.

The synthesis adapter adds an ordinary-table baseline and an external PIFO
boundary. The ordinary baseline writes one mapper bank directly, retains the
command FIFO, drain guard, and routing, and consumes commits without action.
The original replay bank-selection/write logic is preserved. Numeric all-zero
initialization avoids constructing millions of Scala literal objects; it does
not change memory contents, widths, depths, or ports.

## Run the experiments

From `pifo-hardware`, using the installed Python environment with matplotlib:

```bash
.venv/bin/python hw/python/pifo_shared_fifo_experiments.py
.venv/bin/python hw/python/pifo_shared_fifo_experiments.py --collect-only
.venv/bin/python hw/python/finalize_shared_fifo_experiments.py
```

The fixed setup is 5 PEs, 1,024 vFlows, 1,024 PIFO entries per PE, 8-bit ranks,
and a 256-entry controller FIFO. The sweep uses **32, 128, 512, and 1,024 vFlows**,
with `2 × vFlows` entries per CAM bank: four sizes × static/replay × two vendors
= **16 RIO synthesis runs**. Reruns reuse verified completed results. Matching
house-PIFO measurements are reused from the previous sweep after checking every
generated RTL input; only the nonfunctional Git-hash header comment may differ.
The original PIFO reports, manifests, RTL, and reuse proofs remain in the results.
The manager uses up to four Quartus and four Vivado jobs concurrently, with
eight threads per job. Completed jobs require matching source and generated-RTL
hashes before reuse. Incomplete builds must be inspected before a retry.

Results follow `experiment-results/cam-overhead/{r1-fixed,r2-vflows}`:
configuration JSON, resource/comparison CSVs, and `figures/*/{data.csv,figure.svg,figure.png}`.
Native tool evidence and source snapshots are kept under `runs/` and `workflow/`.
Large RTL, vendor databases, and checkpoints remain in
`/data/work/rio-synthesis/cam-shared-fifo-20260909/`.
The earlier dense-table sweep remains in `experiment-results/shared-fifo-overhead/`.

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

## Default bounded CAM backend

`--lookup-backend cam` replaces the large `(vPIFO, flow token)` flow-state and
post-mapper tables with portable exact-match CAMs. The pre-mapper remains a
small dense RAM. CAM lookups and replay are the defaults for the experiment
workflow and direct hardware elaboration. Individual CAM builds use
separate `cam-replay` / `cam-replay-vivado` default directories.

Each CAM stores tags and valid bits in registers, with a single-read/single-write
synchronous value RAM. Lookups take one cycle in RTL. Tag comparisons and match
encoding are combinational, so clock frequency and FPGA resource use still need
synthesis and implementation validation. Replay has two complete post-mapper CAM
banks; flow-state CAMs remain single-bank. The controller still reuses its
256-entry FIFO for replay.

`--cam-entries-per-pe` bounds distinct nonzero `(vPIFO, flow token)` pairs **per
table bank**, independently of the key address space and PIFO packet capacity.
It defaults to twice `--vpifos` to allow two tree contexts per flow; workloads
with more coexisting pairs must select a larger bound. Missing keys return zero.
Writing zero deletes an entry. Software must retire obsolete pairs and delete
before inserting when a bank is full. A full bank continues to accept updates
to existing keys, but holds a new nonzero-key write and asserts the corresponding
`camCapacityBlocked` PE bit. A later commit cannot overtake that write. A blocked
write at the head of the shared FIFO cannot be fixed by queuing a deletion behind
it; capacity must be checked before submission, or the design must be reset.

Generate the full 5 PE / 1,024 vFlow / 1,024 entries-per-PE CAM design, without
running synthesis:

```bash
python synthesis/run.py --tool vivado --board vcu118 \
  --engines 5 --vpifos 1024 --entries-per-pe 1024 \
  --configuration replay --lookup-backend cam --cam-entries-per-pe 2048 \
  --control-queue-depth 256 --pifo-backend external \
  --name cam-pe5-v1024-c1024-q256 --generate-only
```

The manifest separates CAM tag/valid registers from value-RAM storage. These
logical bit counts are not synthesized BRAM or LUT counts. Recorded dense-table
resource results remain under `experiment-results/shared-fifo-overhead/`.

The local Vivado 2025.2 catalog contains `xilinx.com:ip:cam:5.0`, supporting
Virtex UltraScale+. Its hardware-managed exact-match mode is selected using
`CAM_MODE=BCAM` and `UPDATE_MODE=HARDWARE` (CBCAM). The catalog probe on
2026-09-09 reported **IP license not found / Needs Purchase**, requiring
`hcam_base@2025.11` or `hcam_advanced@2025.11`. This portable backend does not
instantiate AMD's licensed IP. Recheck the installed catalog without synthesis:

```bash
vivado -mode batch -source synthesis/probe_cam_ip.tcl \
  -tclargs /tmp/rio-cam-catalog
```

AMD references: [CAM product page](https://www.amd.com/en/products/adaptive-socs-and-fpgas/intellectual-property/ef-di-cam.html),
[licensing](https://docs.amd.com/r/en-US/pg317-bcam/Licensing-and-Ordering),
[hardware update interface](https://docs.amd.com/r/en-US/pg317-bcam/Hardware-Update-Interface-Ports-CBCAM-only).
Vendor CBCAM updates use a multi-cycle ready/valid protocol and would require
an adapter and lookup/backpressure changes before integration.

Run correctness checks with:

```bash
sbt "runMain rio.sim.BoundedCamMapperSim" "runMain rio.sim.BoundedCamIntegrationSim"
sbt "runMain rio.sim.TransactionalConfigSim cam" "runMain rio.sim.SharedReplayDriverSim cam" \
    "runMain rio.sim.DrainGuardSim cam" "runMain rio.sim.FrontUnderflowRewriteSim cam" \
    "runMain rio.sim.ControlIngressRateSim cam"
```

## Optional pipelined CAM

`hw/spinal/rio/PipelinedCamMapper.scala` is a separate experimental module built
around unmodified [Alex Forencich SRL CAM RTL](https://github.com/alexforencich/verilog-cam/tree/69002598f12d7418d44bbaa88ea0be3bf3b14e6c),
vendored with its MIT license and file hashes under `hw/verilog/vendor/verilog-cam`.
Upstream registers the match vector; our wrapper provides the depth pipeline,
using 128 or 256 entries per stage. For 2,048 entries, these choices give 17 or
9 cycles minimum lookup latency, including the synchronous value read. An
unstalled lookup stream accepts one request per cycle after initialization.

Value banks retain one read and one write port. Replay uses the existing shared
controller FIFO, with no separate journal. Static writes pause lookup admission;
replay writes wait for old-bank lookups to finish while active-bank reads continue.
Inserts/deletes include the upstream multi-cycle SRL update; existing-key value
updates avoid rewriting the index. Output backpressure uses a small register FIFO.

This module has Stream request/response interfaces. It is not selected by the
current mesh/default synthesis sweep: mesh callers need to honor backpressure
and align packet metadata for its variable latency. The existing results measure
`BoundedCamMapper`, not this prototype. SRL index storage differs from the default
registered-tag CAM; resource and timing improvements have not been measured.

Run correctness checks and generate a self-contained standalone RTL directory:

```bash
sbt "runMain rio.sim.PipelinedCamMapperSim" "runMain rio.sim.PipelinedCamReplayIntegrationSim"
sbt "runMain rio.GeneratePipelinedCam /tmp/rio-cam-2048-p128 2048 128 replay"
python synthesis/verify_pipelined_cam_rtl.py /tmp/rio-cam-2048-p128 \
  --capacity 2048 --entries-per-stage 128 --replay
```

The generator copies the two upstream RTL dependencies, license, provenance and
`rtl-files.f`. These commands do not invoke Quartus or Vivado.
