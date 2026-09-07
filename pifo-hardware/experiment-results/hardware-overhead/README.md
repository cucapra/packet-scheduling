# Hardware overhead evidence

The primary experiments synthesize RIO with explicit external PIFO interfaces.
They include all five PEs, mapper/brain logic, crossbar, and configuration
controller; PIFO sorting, entry storage, occupancy, and drain detection are
excluded and require a separate budget. Every report distinguishes completed
synthesis from failed, cancelled, or still-running work.

| Case | Measurement | Artifacts |
|---|---|---|
| R1 | Static versus atomic read/copy, 5 PEs / 1,024 IDs / 1,024 PIFO entries per PE reserved | [Report](r1-fixed/report.md), [CSV](r1-fixed/comparison.csv) |
| R2 | Static versus atomic read/copy, 32–1,024 IDs | [Report](r2-vflows/report.md), [logic](r2-vflows/figures/logic/figure.svg), [memory](r2-vflows/figures/memory/figure.svg) |
| R3 | One- versus eight-cycle isolated lookup | [Report](r3-lookup-pipeline/report.md), [table](r3-lookup-pipeline/figures/resource-table/figure.svg) |
| R4 | Controller instruction replay versus static and read/copy, 32/128/1,024 IDs | [Report](r4-replay/report.md), [CSV](r4-replay/comparison.csv), [memory](r4-replay/figures/memory/figure.svg) |

R4 is the implementation proposed after identifying read-port replication in
the original atomic mapper. It uses a shared 16,384-entry instruction log and
two mapper banks with one read and one write port each. The complete log and
controller cost is included. See the
[protocol and capacity contract](../../experiments/hardware-overhead/REPLAY.md).
The [RAM breakdown](r4-replay/memory-breakdown.md) isolates mapper banks,
unbanked engineCAM tables, and the journal, and reconciles them with each
completed synthesis total.

Both vendors use the installed tools and board definitions recorded in each
manifest: Quartus Pro 25.3.1 / Agilex 7 AGFB014R24B2E2V and Vivado 2025.2 /
Kintex UltraScale+ xcku5p-ffvb676-2-e. Constraints target 100 MHz; implementation
is not run. Oversized Vivado estimates explicitly skip the pre-mapping capacity
gate, retaining the actual target capacities in their utilization reports.

## Provenance and validation

- `workflow/` records the workflow before adding replay;
  `diagnostics/read-copy-source-snapshot/` records the exact R1/R2 RTL sources.
- `r4-replay/workflow/` records the replay RTL and workflow.
- `r3-lookup-pipeline/workflow/` records the isolated lookup generator used by
  its four completed runs; shared vendor scripts are in `workflow/`.
- `validation/` contains packet/configuration simulations, compact-MIF controls,
  PIFO boundary checks, and reference RTL equivalence evidence.
- `r4-replay/validation/` checks Quartus's simple-dual-port mapper/log inference
  and absence of the additional copy-read RAM replicas.
- The replay controller test exercises 29 commits and 52 staged/replayed
  updates, checking full bank contents after synchronized batches. It covers
  full-log credits, a queued next-epoch update, FIFO wraparound, and overlapping
  packet lookups. The separate full-mesh packet test checks atomic visibility.
- `whole-mesh/` retains earlier house-PIFO measurements and cancelled attempts.
  `diagnostics/whole-mesh-source-snapshot/` holds their RTL sources.
- `diagnostics/` retains failed capacity checks, interrupted long elaboration,
  workflow probes, and scheduler changes; these are not final resource counts.

`pifo-storage-budget.csv` is a logical payload calculation, not mapped hardware.
`pifo-residual-diagnostic.csv` subtracts isolated RIO from whole-mesh measurements
at 32 IDs and includes changes in optimization across the boundary. It is not a
standalone PIFO measurement and is not extrapolated to the larger cases.

Quartus ALMs/ALUTs and Vivado LUTs are distinct units. Quartus block-memory bits
are synthesis estimates; Vivado BRAM/URAM values are mapped primitive capacities.
Dense flow/token tables retain their current quadratic sizing. A synthesis
result alone does not demonstrate a physical fit or frequency improvement.
