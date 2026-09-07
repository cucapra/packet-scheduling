# Replay RAM breakdown

All values come from completed synthesis reports with PIFO cores excluded. Components sum exactly to each run's reported top-level RAM count. The CSV includes every completed R4 point; the tables below show 1,024 IDs.

## Quartus: mapped block-memory bits

| Component | Ordinary | Read/copy | Replay |
|---|---:|---:|---:|
| Post-mapper banks, all PEs | 545,259,520 | Incomplete | Incomplete |
| Pre-mapper banks, all PEs | 0 | Incomplete | Incomplete |
| Unbanked engineCAM tables, all PEs | 335,544,320 | Incomplete | Incomplete |
| Shared instruction log | 0 | Incomplete | Incomplete |
| Other inferred memories | 792 | Incomplete | Incomplete |

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

Incomplete measurements:

- quartus, 1024 IDs, dynamic: `synthesis_running`.
- quartus, 1024 IDs, replay: `synthesis_running`.
