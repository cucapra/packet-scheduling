# Hardware overhead experiment results

| Experiment | Figures | Data |
|---|---|---|
| R1: 5 PEs / 1,024 vFlows / 1,024 PIFO entries per PE | [Resource table](r1-fixed/figures/resource-table/figure.svg) | [Absolute and percentage differences](r1-fixed/comparison.csv) |
| R2: vFlow sweep, fixed PEs and PIFO capacity | [Logic](r2-vflows/figures/logic/figure.svg), [registers](r2-vflows/figures/registers/figure.svg), [memory](r2-vflows/figures/memory/figure.svg), [overhead](r2-vflows/figures/overhead-percent/figure.svg) | [Totals](r2-vflows/resources.csv), [differences](r2-vflows/comparison.csv) |
| Lookup pipeline exploration | [One- versus eight-cycle lookup](r3-lookup-pipeline/figures/resource-table/figure.svg) | [Component differences](r3-lookup-pipeline/comparison.csv) |

R1/R2 compare ordinary tables with **separate-journal** controller replay on
Quartus and Vivado, using the implementation saved at `d5a10e8`. The current
shared FIFO replay change has [correctness validation](validation/shared-control-fifo/validation.json)
only; these resource results have not been regenerated for it.
Both totals include five separately measured house PIFOs at matching widths.
Percentage change divides by ordinary RIO plus those five PIFOs. The
[PIFO budget](pifo-component/resources.csv) records every native resource cost.

These are sums of synthesis components, not implemented scheduler utilization.
The current dense 1,024-flow RTL exceeds device memory capacity. Each figure
also has PNG and CSV files. Configurations, source references, vendor logs, and
validation evidence are retained alongside the measured data.

[Accounting validation](validation/experiment-accounting.json) checks every
source count and percentage, and records the difference between component sums
and the archived 32-flow whole-mesh Quartus measurement.

[Experiment definitions and workflow](../../experiments/hardware-overhead/README.md)
