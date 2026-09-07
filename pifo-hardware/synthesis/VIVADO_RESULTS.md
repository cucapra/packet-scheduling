# Vivado resource estimate: complete fixed PIFO mesh

**Vivado synthesis succeeded:** **198,425 LUTs**, **42,085 flip-flops**,
**8 RAMB36E2**, **2 URAM288**, and **0 DSPs** on
**Kintex UltraScale+ `xcku5p-ffvb676-2-e`**. This uses **91.46%** of the
device's LUT capacity at synthesis. Placement, routing, and timing closure
were not run, so a physical fit at this utilization is not established.

The measurement uses Vivado's **RuntimeOptimized** synthesis directive. It
performs fewer timing/RTL optimizations than the default directive; these
counts describe this particular synthesis run. See
[AMD UG901, synthesis settings](https://docs.amd.com/r/2025.2-English/ug901-vivado-synthesis/Using-Synthesis-Settings).

## Hardware and environment

The input is byte-for-byte identical to the RTL and initialization files in
the successful [Agilex 7 synthesis](RESULTS.md). All hardware source hashes
also match. The fixed configuration is:

| Parameter | Value |
| --- | ---: |
| PEs | 2 |
| vPIFO IDs per PE | 32; ID 0 is null |
| Global flow-ID capacity | 32 |
| Shared sorted entries per PE | 1,024 |
| Total sorted entries | 2,048 |
| Rank width | 8 bits |
| Stored sorted-entry width | 20 bits |
| Declared flow/brain state width | 32 bits |
| Configuration/commit queue depth | 4 |
| Crossbar | 3 ports, 8-entry input FIFOs |

This includes the existing configuration/commit controller, local PE
control, both complete PEs, mappers, brains, front rewrite tables, sorters,
and crossbar. All runtime inputs remain exposed. The unfinished hardware
admission/dequeue controller, simulator request controller, packet payload
queues, and MAC/PCIe/DDR/board shell are outside this RTL boundary.

- Tool: **Vivado 2025.2, Build 6299465**, detected at
  `/data/work/vivado/2025.2/Vivado`.
- Board reference: **KCU116**, version **1.5**, from the installed
  `data/xhub/boards/XilinxBoardStore/boards/Xilinx/kcu116/1.5/board.xml`.
  Vivado's device query confirmed that the target part itself is installed.
- This device is supported by Vivado 2025.2 Standard Edition without an
  external license, as documented in
  [AMD UG973, supported devices](https://docs.amd.com/r/2025.2-English/ug973-vivado-release-notes-install-license/Supported-Devices).
  The actual synthesis license checkout succeeded.
- Mode: `out_of_context`, hierarchy `rebuilt`, `RuntimeOptimized`, maximum
  8 configured general threads; **100 MHz** clock constraint, reset false path.
- Completed **September 6, 2026, 19:37:43 EDT**. `synth_design` took **6:44**;
  the runner including startup/checkpoint/reports took **7:29**. Peak reported
  memory through checkpoint generation was **5,797 MB**.

## Resource usage

| Resource | Used | Available | Utilization |
| --- | ---: | ---: | ---: |
| Total CLB LUTs | **198,425** | 216,960 | **91.46%** |
| LUTs as logic | 198,169 | 216,960 | 91.34% |
| LUTs as distributed RAM, included in total LUTs | 256 | 99,840 | 0.26% |
| Flip-flops | **42,085** | 433,920 | **9.70%** |
| RAMB36E2 / block-RAM tiles | **8** | 480 | **1.67%** |
| RAMB18 | 0 | 960 | 0% |
| URAM288 | **2** | 64 | **3.13%** |
| DSPs | **0** | 1,824 | 0% |
| CARRY8 | 4,098 | 27,120 | 15.11% |
| F7 / F8 muxes | 4,604 / 2,160 | 108,480 / 54,240 | 4.24% / 3.98% |
| Latches / unresolved blackboxes | **0 / 0** | — | — |

Use Vivado's reported total LUT count, which accounts for LUT combining.
Summing the individual LUT1–LUT6 primitive counts would give a different
number. The report also notes that later implementation optimizations can
change the LUT count. Out-of-context synthesis reports no bonded I/O buffers
or board clock-network resources for this core.

## Component breakdown

| Component | Total LUTs | LUTRAMs, included | Flip-flops | BRAM36 | URAM |
| --- | ---: | ---: | ---: | ---: | ---: |
| PE 0, including local control | 99,004 | 100 | 20,980 | 4 | 1 |
| PE 1, including local control | 98,992 | 100 | 20,981 | 4 | 1 |
| Crossbar and input FIFOs | 122 | 24 | 56 | 0 | 0 |
| Mesh control, configuration queue, and remainder, by subtraction | 307 | 32 | 68 | 0 | 0 |
| **Total** | **198,425** | **256** | **42,085** | **8** | **2** |

The last component row is an arithmetic residual, not a separately
synthesized controller. The displayed `io_controlRequest_fifo` row reports
293 LUTs and 58 FFs; `(PifoMesh)` reports another 2 FFs. The displayed direct
children do not sum exactly to the top-level report: 14 LUTs and 8 FFs remain
unattributed by those rows. The residual includes that difference. Use the
top-level resource report for the whole-core budget.

The sorters alone report **98,061 / 98,056 LUTs** and **20,548 / 20,549 FFs**
for PE 0 / PE 1. Their reported LUT counts total **98.84%** of the whole mesh's
LUT count. The original sorted-entry storage is 40,960 register bits.

## Memory mapping and Quartus comparison

- Each PE's `engineCAM` flow-state table maps **4096 × 32 bits** into one
  URAM288. Both URAMs together have **589,824 allocated bits**, with
  **262,144 used bits** in the memory report. Quartus reduced each corresponding
  state word to 8 bits in its optimized memory mapping.
- Post-dequeue mappings use two transactional banks per PE. Each bank is
  replicated for its two read paths, producing **8 RAMB36E2** instances of
  **4096 × 7 bits**. The report counts **229,376 used bits** in
  **294,912 allocated bits**, including the replicas and block padding.
- Small tables and FIFOs use **256 LUTs as distributed RAM**. The sorter
  entries remain registers.

The BRAM/URAM report therefore totals **491,520 used bits** in
**884,736 allocated bits (108 KiB)**, excluding distributed RAM and registers.
These are mapped-memory quantities, not additional logical PIFO entries.

| Same RTL, different target/mapping | Quartus / Agilex 7 | Vivado / Kintex UltraScale+ |
| --- | ---: | ---: |
| Logic | 90,077 estimated ALMs; 141,022 ALUTs | 198,425 LUTs, including 256 LUTRAMs |
| Registers / FFs | 42,356 | 42,085 |
| Memory | 297,440 block-memory bits; physical M20K count not reported | 8 BRAM36 + 2 URAM + 256 LUTRAMs |
| DSPs | 0 | 0 |

Intel ALMs/ALUTs and AMD LUTs use different packing and mapping rules. The
devices also have different capacities, and the runs use different synthesis
optimization settings. These measurements do not establish a vendor area or
performance advantage. Both identify the register-based sorters as the
dominant logic cost.

## Validation, warnings, and saved workflow

The full flow completed with **zero errors and zero critical warnings**.
Its four optimization warnings concern unpipelined UltraRAM; the elaboration
log additionally contains unused-signal and encoder-width-trimming warnings.
No pipeline stages were added to the scheduler. The clock remains a synthesis
constraint, with no verified maximum frequency or packet-throughput claim.

Validation checked matching hardware/source/RTL hashes against the Quartus
baseline, successful completion evidence, no unresolved blackboxes, zero
latch primitives, and agreement between utilization and FF/BRAM/URAM/DSP
primitive counts. Both tool preparation flows passed. The resource parsers
reject failed, cancelled, and incomplete runs, and RTL reuse rejects a size
mismatch. The scheduler RTL was not changed for Vivado.

From `pifo-hardware`, run the measured configuration from a fresh checkout with:

```bash
python3 synthesis/run.py --tool vivado --name baseline-vivado-runtime \
  --pifo-backend house --vivado-directive RuntimeOptimized
python3 synthesis/summarize_vivado.py synthesis/build/baseline-vivado-runtime \
  synthesis/build/baseline-vivado-runtime/vivado-summary.json
```

To share RTL with a freshly generated Quartus build, add
`--rtl-from synthesis/build/baseline`. Earlier pre-backend manifests must be
regenerated before passing the current source and manifest checks. The
[workflow guide](README.md) also records environment discovery,
size options, licenses, preparation, and both vendor commands.

Preserved evidence:
[utilization](results/baseline-vivado-runtime/reports/utilization.rpt),
[hierarchy](results/baseline-vivado-runtime/reports/utilization-hierarchy.rpt),
[RAM mapping](results/baseline-vivado-runtime/reports/ram-utilization.rpt),
[structured summary](results/baseline-vivado-runtime/vivado-summary.json),
[manifest](results/baseline-vivado-runtime/manifest.json), and
[complete log](results/baseline-vivado-runtime/synthesis.log).
The synthesized checkpoint remains in the ignored
`synthesis/build/baseline-vivado-runtime/pifo_synth.dcp`.

Earlier diagnostics are kept separately: VCU118 lacked its synthesis license,
ZCU106 lacked installed device files, and the default KCU116 synthesis pass
was stopped after **28:31**, still in timing optimization, once the successful
RuntimeOptimized result was verified. None of those attempts is reported as
a completed resource measurement.
