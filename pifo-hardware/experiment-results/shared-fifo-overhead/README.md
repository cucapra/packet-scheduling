# Shared-FIFO replay experiment results

Paired synthesis of ordinary tables and replay based on `34e35d2`, with 256
controller FIFO entries in both variants. No separate replay journal is present.

- `r1-fixed/`: 5 PEs, 1,024 vFlows, 1,024 PIFO entries per PE; absolute and
  percentage resource differences for Quartus and Vivado.
- `r2-vflows/`: 32–1,024 vFlows with fixed PE count and PIFO capacity.
- `pifo-component/`: newly measured matching house PIFO costs.
- `runs/`, `logs/`, `workflow/`: native resource reports, commands, and source.
- `run-status.json`: distinguishes completed measurements from pending runs.

Totals add five measured PIFOs to each RIO-only result. Overhead percentages divide by
ordinary RIO plus those PIFOs. Completed figures are created only when every
required measurement for that experiment is available.

The fixed table includes BRAM counts and ordinary/replay percentages of target
capacity. The sweep includes BRAM count (`figures/memory/`) and target utilization
(`figures/bram-utilization/`) plots, each with a 100% capacity line.
`bram-resources.csv` and `bram-comparison.csv` retain counts, target capacities,
utilization percentages, and absolute/percentage overhead; `bram-targets.json`
records the denominator sources. Target utilization is `100 × total / available`.

Quartus uses **M20K bit-capacity equivalents**: inferred block RAM bits / 20,480,
against the AGFB014's 7,110 M20Ks. These are capacity estimates; physical block
packing is not modeled. Capacity and block size come from the
[Agilex 7 F-Series product table](https://docs.altera.com/api/khub/documents/T99La5fz4bf~McCaFB0bPw/content)
and [embedded memory guide](https://docs.altera.com/r/docs/683241/25.1.1/agilextm-7-embedded-memory-user-guide/agilextm-7-embedded-memory-features).
Vivado uses mapped **BRAM36 equivalents** (`RAMB36 + RAMB18 / 2`), against the
480 tiles reported for XCKU5P in each native utilization report. MLAB, eSRAM,
LUTRAM, and UltraRAM capacity are excluded from these BRAM denominators.

These are synthesis component estimates, with no placement or routing. The
dense 1,024-flow setup exceeds device memory capacity. Quartus reports inferred
RAM bits; Vivado reports mapped BRAM allocation. Native logic units differ.

[Reproduction and tool setup](../../synthesis/README.md)
