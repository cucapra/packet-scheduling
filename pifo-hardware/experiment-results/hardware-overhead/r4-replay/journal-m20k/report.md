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

The isolated journal results are a mapping control and are not added to or subtracted from whole-core synthesis totals. The full-core comparison is measured by rerunning the complete unchanged RTL. These remain synthesis estimates; the dense tables exceed device memory capacity and no implementation or timing closure was run.

The original automatic-mapping result is retained in the main R4 tables. This separate experiment reports an explicit journal placement setting. Vivado already maps the 1,024-ID replay journal to 18 BRAM36 tiles; its original result remains applicable.

- full-m20k: `synthesis_running`; no completed resource count.
