# Replay RAM breakdown

All values come from completed synthesis reports with PIFO cores excluded. Components sum exactly to each run's reported top-level RAM count. The CSV includes every completed R4 point; the tables below show 1,024 IDs.

## Quartus: mapped block-memory bits

| Component | Ordinary | Read/copy | Replay |
|---|---:|---:|---:|
| Post-mapper banks, all PEs | 545,259,520 | 2,181,038,080 | 1,090,519,040 |
| Pre-mapper banks, all PEs | 0 | 0 | 0 |
| Unbanked engineCAM tables, all PEs | 335,544,320 | 335,544,320 | 335,544,320 |
| Shared instruction log | 0 | 0 | 0 |
| Other inferred memories | 792 | 792 | 792 |

## Vivado: BRAM36 tile equivalents

| Component | Ordinary | Read/copy | Replay |
|---|---:|---:|---:|
| Post-mapper banks, all PEs | 15,360 | 61,440 | 30,720 |
| Pre-mapper banks, all PEs | 2.5 | 10 | 5 |
| Unbanked engineCAM tables, all PEs | 20,480 | 20,480 | 20,480 |
| Shared instruction log | 0 | 0 | 18 |
| Other inferred memories | 7.5 | 7.5 | 7.5 |

Quartus counts inferred implementation bits, including width pruning and RAM replicas; these are not a fitted M20K allocation. Vivado counts mapped block RAM primitives (RAMB18 counts as half a tile); allocated bits and URAM counts are also in the CSV. LUT RAM is outside this block-memory breakdown.

A zero block-memory count does not mean a component is absent: any logic or register implementation is included in the whole-design resource totals. For example, Quartus implements the 1,024-ID ordinary pre-mappers with logic and registers.

The replay log is included in the totals. Rebuilt hierarchy can assign shared logic to the FIFO that drives it, so hierarchical LUT counts are not used here as standalone controller costs. Whole-design LUT differences remain the logic comparison.

Quartus, 1,024 IDs: the 655,360-bit journal maps to logic and registers. Its hierarchy reports 655,446 registers and zero RAM bits. This cost is included in the whole-design logic/register totals; the two post-mapper banks per PE remain simple dual-port RAMs.

The completed [journal-only M20K control](journal-m20k/report.md) preserves all 22 other RAM instances and adds 655360 journal RAM bits. Its total is 1,426,719,512 RAM bits. The table above retains original automatic placement.
