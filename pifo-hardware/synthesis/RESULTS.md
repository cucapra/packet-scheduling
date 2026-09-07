# Full PIFO mesh: fixed-size resource analysis

**Quartus synthesis succeeded:** the complete baseline mesh needs an estimated
**90,077 ALMs (18.49% of AGFB014R24B2E2V)**, **42,356 registers**, and
**297,440 block-memory bits**, with **0 DSP blocks**. These are synthesis
estimates; placement, routing, and timing closure were not run.

The identical RTL was also successfully synthesized with Vivado 2025.2 on
Kintex UltraScale+: **198,425 LUTs, 42,085 FFs, 8 BRAM36, and 2 URAM**, using
the RuntimeOptimized directive. See the [Vivado report](VIVADO_RESULTS.md) for
the device, memory mapping, component breakdown, and comparison limitations.

## Machine and board reference

Measured on September 6, 2026, using this repository's house-PIFO baseline.

- Quartus Prime Pro 25.3.1, Build 100, installed at
  `/data/work/quartus/quartus`.
- Installed board definition:
  `common/devkits/agilex_f_series_development_kit/agilex_f_series_development_kit.devkit_info`.
  It identifies the Agilex F-Series FPGA Development Kit and part
  **AGFB014R24B2E2V**. The runner reads the part from this definition.
- Installed platform reference:
  `common/devplatforms/agilex_f_series_bts_config/agilex_f_series_bts_config.qar`.
  Its restored `platform_setup.tcl` independently specifies the same part. The
  board support reference is BTS Config, originally for Quartus 22.4; it is not
  an installed OpenCL/oneAPI BSP or a packet-processing NIC shell.
- License: `/data/work/quartus/licenses/LR-187458_License.dat`.
  FlexNet verified the correct node and a `quartus_pro` expiration of
  **December 5, 2026**. This resolved the earlier expired-license failures.
- Command: `quartus_syn pifo`, Balanced optimization, 8 configured processors.
  Completed at **18:33:49 EDT**, September 6, 2026, in **9 minutes 57 seconds**;
  peak virtual memory was 3,571 MB. Quartus reported **0 errors, 3 warnings**
  and a separate Reset Release IP critical warning.

The original [Quartus report](results/baseline/quartus-synthesis.rpt),
[console output](results/baseline/quartus-synthesis.txt),
[structured resource tables](results/baseline/quartus-summary.json), and
[size/source manifest](results/baseline/manifest.json) are preserved.

## Quartus synthesis results

| Resource | Complete mesh |
| --- | ---: |
| Estimated ALMs needed | **90,077 / 487,200 (18.49%)** |
| Combinational ALUTs | **141,022** |
| Dedicated logic registers | **42,356** |
| Block-memory bits after synthesis | **297,440 (36.31 KiB)** |
| MLAB memory bits reported | **0** |
| DSP blocks | **0** |
| Inferred RAM instances in RAM Summary | **25**, all type `AUTO` |
| Top-level I/O signals reported | 90; data ports are virtual pins |

Quartus reports block-memory **bits**, not a final M20K allocation, in this
synthesis flow. Dividing by 20,480 would ignore RAM shape, fragmentation, and
packing; no physical M20K count is claimed. Similarly, the 100 MHz SDC clock is
a synthesis target, not a measured achievable frequency.

The hierarchy report gives ALUTs and registers per component; its ALM estimate
is for the whole mesh. These rows do not overlap and sum to the whole design:

| Component | Combinational ALUTs | Registers | Block-memory bits |
| --- | ---: | ---: | ---: |
| PE 0, including local control | 70,426 | 21,059 | 148,672 |
| PE 1, including local control | 70,398 | 21,059 | 148,672 |
| Crossbar and its input FIFOs | 163 | 224 | 0 |
| Mesh configuration/commit controller, queue, and routing | 35 | 14 | 96 |
| **Total** | **141,022** | **42,356** | **297,440** |

The mesh-controller row is the total minus both PEs and the crossbar. It does
not represent all control logic: mapper synchronization and PE/brain control
are already included in the PE rows. Within each PE, the principal costs are:

| Component inside PE | PE 0 ALUTs | PE 1 ALUTs | Registers per PE | Memory bits per PE |
| --- | ---: | ---: | ---: | ---: |
| Sorted PIFO and priority encoders | 69,737 | 69,712 | 20,519 | 0 |
| Brain, state tables, and its stream queues | 223 | 223 | 176 | 33,344 |
| Post-dequeue mapper with bank synchronization | 60 | 60 | 33 | 114,688 |
| Pre-enqueue mapper with bank synchronization | 31 | 31 | 15 | 640 |
| Front rewrite table | 271 | 271 | 288 | 0 |

The remaining PE resources are stream/control glue. The two sorting blocks
account for **98.88% of the mesh's combinational ALUTs**: the sorter, rather
than the configuration controller, determines this baseline's logic cost.
The small PE-to-PE logic difference is present in Quartus's optimized report.

## Why the memory result differs from the generic audit

Quartus both removes unused bits and duplicates memories for additional read
ports. Its RAM Summary shows these concrete changes:

- Each 4,096 × 32 flow-state table is reduced to 32,768 bits, equivalent to
  4,096 × 8; only the eight rank bits are consumed by the current policies.
  The synthesis implementation shape shown is 2,048 × 16.
- Each 32 × 32 brain-state table is reduced to 32 × 8.
- Each transactional mapper bank is duplicated for its two read ports. The
  post mapper therefore has four 28,672-bit RAMs per PE, totaling 114,688 bits.
  The pre mapper similarly has four 160-bit RAMs per PE.
- Small stream FIFOs become logic/registers. The configuration queue's RAM
  payload is reduced to 24 bits per entry, totaling 96 bits.

| Surviving RAM category | Whole-mesh implementation bits |
| --- | ---: |
| Duplicated post-dequeue mapper banks | 229,376 |
| Flow state after trimming to eight bits | 65,536 |
| Duplicated pre-enqueue mapper banks | 1,280 |
| Brain-state tables | 512 |
| Last virtual-time tables | 512 |
| Brain policy tables | 128 |
| Configuration ingress queue | 96 |
| **Total** | **297,440** |

## Synthesis checks and limits

All source files and memory initializations were read successfully. The
post-synthesis Design Assistant reports zero combinational loops and zero
inferred-latch violations. Its remaining failed rule concerns registers not
reachable from Reset Release IP; the core-only top does not instantiate that
board-integration IP. The elaboration report also flags asynchronous clears
on RAM control signals. The [elaboration DRC](results/baseline/drc-partitioned.rpt)
and [post-synthesis DRC](results/baseline/drc-synthesized.rpt) preserve these
diagnostics. These issues must be handled during board integration; this run
does not validate configuration/reset sequencing or timing.

The synthesized design is comfortably below the device's ALM capacity on this
estimate, but no claim is made about routed fit, operating frequency, or power.

## Fixed hardware boundary

| Parameter | Baseline |
| --- | ---: |
| Processing engines | 2 |
| vPIFO IDs per PE | 32, of which vPIFO 0 is the null sink |
| Global input flow-ID capacity | 32 IDs, 0–31 |
| Shared sorted entries per PE | 1,024 |
| Total scheduler entries | 2,048 |
| Rank width | 8 bits |
| PE-qualified token width | 7 bits: 2 engine + 5 flow/vPIFO |
| Stored entry width | 20 bits: 5 port + 8 rank + 7 token |
| Declared brain / flow state widths | 32 / 32 bits |
| Configuration ingress queue | 4 instructions, 49 bits each |
| Crossbar | 3 ports, 8-entry input FIFOs |

This includes the hardware configuration/commit queue, distribution and bank
synchronization, all PE controllers, mapper banks, front rewrites, brains,
sorting arrays, crossbar, and stream buffering. Every external control and data
port is exposed; no test configuration is tied off to obtain a smaller design.

The insertion/dequeue request controller in `rio.sim`, per-flow packet queues,
packet payload/descriptor storage, and link-prefetch behavior are simulation
code, not part of `PifoMesh`. They are not included. The repository still lists
a hardware admission/dequeue controller as unfinished. Thus these results cover
the full **existing synthesizable scheduler**, not a complete packet-buffering
SmartNIC. A request can occupy several scheduler entries, so 2,048 entries do
not necessarily mean 2,048 distinct buffered packets. `fifoDepth = 32` is a
capacity multiplier, not an enforced quota of 32 packets per virtual PIFO.

`EngineConfig.numFlows` returns 64 here, but that helper does not size the
external global flow namespace. `PifoMessage.vPifoId` is five bits and the
simulation controller broadcasts the same global flow ID to each PE. The dense
tables use the full seven-bit PE-qualified token address space, including
unused engine encodings.

## Earlier generic RTL audit, for comparison

Yosys 0.33 read the generated SystemVerilog, elaborated the encoder, flattened
the mesh, converted processes, optimized constants and widths, and collected
memories without mapping them to an FPGA. `check -assert` reported **0 problems**;
no latches or unresolved blackboxes remained.

| Resource | Whole mesh |
| --- | ---: |
| Explicit flip-flop bits | **41,902** |
| Of those: sorted-entry arrays | **40,960** |
| Of those: other registered state/control/pipelines | **942** |
| Logical memory bits | **381,220** (46.54 KiB) |
| Logical memory arrays | **36** |

The [machine-readable audit](results/baseline/generic-summary.json) lists each
memory, its dimensions, and read/write port counts. Small FIFOs may become
registers or MLABs during Quartus synthesis, and memory port implementation may
require replication. Consequently the explicit flip-flop count and logical
memory count are not final FPGA utilization.

| Logical memory category | Whole mesh bits |
| --- | ---: |
| Flow-state tables: 2 × 4,096 × 32 | 262,144 |
| Post-dequeue mapper: 2 PEs × 2 banks × 4,096 × 7 | 114,688 |
| Pre-enqueue mapper: 2 PEs × 2 banks × 32 × 5 | 640 |
| Brain policy tables | 128 |
| Last virtual-time tables | 512 |
| Brain-state tables | 2,048 |
| PE pipeline FIFOs | 696 |
| Mesh control queue | 196 |
| Crossbar input FIFOs | 168 |
| **Total** | **381,220** |

The sorted arrays account for about **97.8% of explicit register bits**, and
the dense flow-state plus post-mapper tables account for **98.9% of logical
memory bits**. The remaining 942 explicit register bits include front rewrite
state, pipeline state, counters, and control. The Quartus tables above supersede
these generic counts for the synthesis estimate.

## How size affects resources

Let `E` be PEs, `V` the power-of-two vPIFO/flow-ID capacity, `C` shared entries
per PE, and `P` rank bits. Define:

```text
b = log2(V)
e = ceil(log2(E + 1))
t = b + e
A = 2^(b + t) = 2^e × V²

Sorted-entry register bits = E × C × (b + P + t)
Flow-state memory bits    = E × A × 32
Post-mapper memory bits   = E × 2 × A × t
```

These are structural formulas for the current design, before FPGA mapping.
The following comparisons hold `C = 1,024` entries per PE and `P = 8` fixed;
only the first row has been audited through generated RTL.

| PEs | Flow/vPIFO IDs | Sorted-entry register bits | Flow-state + post-mapper bits |
| ---: | ---: | ---: | ---: |
| 2 | 32 | 40,960 | 376,832 |
| 4 | 32 | 86,016 | 1,572,864 |
| 2 | 64 | 45,056 | 1,572,864 |
| 2 | 128 | 49,152 | 6,553,600 |
| 2 | 256 | 53,248 | 27,262,976 |

Increasing PIFO space mainly increases the sorted registers and their
comparison/shift network. Increasing maximum flows grows dense table storage
quadratically even when PIFO entry space is fixed. PE-count increases also
widen the token and can double the table address space at an engine-ID width
boundary, as in the 2-to-4 PE comparison.

The sorting implementation accesses and shifts many entries simultaneously;
its capacity is held in flip-flops, not a simple block-RAM FIFO. Its mux,
comparator, and priority-encoder logic can dominate ALM use beyond the storage
cost. Generic cell counts must not be interpreted as ALMs.

The Quartus baseline confirms memory duplication for the transactional
mapper's two read ports and removal of unused upper state bits. Therefore the
structural formulas above describe the declared design, not a calibrated
prediction of block-memory bits at other sizes. Those configurations require
their own synthesis measurements.

For a larger-flow implementation, the useful design changes would be to
separate the global flow namespace from vPIFO/token IDs, replace dense
`(vPIFO, token)` tables with storage sized to the supported associations, and
reconsider the register sorter as entry capacity grows. No such architectural
changes were made for this baseline.

The distinction between synthesis estimates and device utilization is described
in Altera's [synthesis summary report documentation](https://www.intel.com/content/www/us/en/programmable/quartushelp/22.4/report/rpt/rpt_file_analysis_summary.htm).
The generic passes are documented in the [Yosys command reference](https://yosyshq.readthedocs.io/projects/yosys/en/latest/cmd-cmd.html).

## Reproduce the Quartus estimate

From `pifo-hardware` on this machine:

```bash
python3 synthesis/run.py --tool quartus --pifo-backend house \
  --license /data/work/quartus/licenses/LR-187458_License.dat
```

To reuse a build made from the current source and manifest format, add
`--reuse-rtl`. Earlier pre-backend manifests must first be regenerated. The flow ends after
`quartus_syn`; place, route, timing closure, and programming are not required.
See [README.md](README.md) for size options and the generic-audit command.
