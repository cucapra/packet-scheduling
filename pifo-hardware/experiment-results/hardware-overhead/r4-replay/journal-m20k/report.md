# Quartus replay journal placement

This control uses the same 16,384 × 40-bit instruction journal and the same Agilex 7 target, eight threads, Balanced synthesis, and 100 MHz constraint as R4. The full-core case retains five PEs and 1,024 IDs with PIFO cores excluded.

The only full-core setting change assigns the journal array to M20K. Canonical RTL, compact MIF files, mapper tables, and read-during-write behavior are unchanged. The assignment is scoped to the journal module; it does not force other memories.

| Scope | Resource | Automatic | Journal M20K | Difference | Change |
|---|---|---:|---:|---:|---:|
| isolated_journal | logic_alms | 490 | 490 | +0 | +0.00% |
| isolated_journal | logic_aluts | 172 | 172 | +0 | +0.00% |
| isolated_journal | registers | 49 | 49 | +0 | +0.00% |
| isolated_journal | block_memory_bits | 655,360 | 655,360 | +0 | +0.00% |
| isolated_journal | mlab_memory_bits | 0 | 0 | +0 | N/A |
| isolated_journal | dsp_blocks | 0 | 0 | +0 | N/A |
| full_core | logic_alms | 479,742 | 355,832 | -123,910 | -25.83% |
| full_core | logic_aluts | 681,729 | 446,357 | -235,372 | -34.53% |
| full_core | registers | 923,812 | 268,415 | -655,397 | -70.94% |
| full_core | block_memory_bits | 1,426,064,152 | 1,426,719,512 | +655,360 | +0.05% |
| full_core | mlab_memory_bits | 0 | 0 | +0 | N/A |
| full_core | dsp_blocks | 0 | 0 | +0 | N/A |

## Full-core comparison with ordinary and read/copy tables

These references are the completed, preserved R4 Quartus measurements. Static uses one ordinary bank; dynamic is the original read/copy implementation. The read/copy MLAB count is unreported and is omitted rather than treated as zero.

| Baseline | Resource | Baseline count | Replay, journal M20K | Difference | Change |
|---|---|---:|---:|---:|---:|
| static | logic_alms | 249,166 | 355,832 | +106,666 | +42.81% |
| static | logic_aluts | 257,723 | 446,357 | +188,634 | +73.19% |
| static | registers | 145,293 | 268,415 | +123,122 | +84.74% |
| static | block_memory_bits | 880,804,632 | 1,426,719,512 | +545,914,880 | +61.98% |
| static | mlab_memory_bits | 0 | 0 | +0 | N/A |
| static | dsp_blocks | 0 | 0 | +0 | N/A |
| dynamic | logic_alms | 526,738 | 355,832 | -170,906 | -32.45% |
| dynamic | logic_aluts | 665,003 | 446,357 | -218,646 | -32.88% |
| dynamic | registers | 268,960 | 268,415 | -545 | -0.20% |
| dynamic | block_memory_bits | 2,516,583,192 | 1,426,719,512 | -1,089,863,680 | -43.31% |
| dynamic | dsp_blocks | 0 | 0 | +0 | N/A |

The isolated journal results are a mapping control and are not added to or subtracted from whole-core synthesis totals. The full-core comparison is measured by rerunning the complete unchanged RTL. These remain synthesis estimates; the dense tables exceed device memory capacity and no implementation or timing closure was run.

The original automatic-mapping result is retained in the main R4 tables. This separate experiment reports an explicit journal placement setting. Vivado already maps the 1,024-ID replay journal to 18 BRAM36 tiles; its original result remains applicable.

