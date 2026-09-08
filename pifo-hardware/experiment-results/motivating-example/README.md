# Motivating example: explicit FIFO-leaf rerun

These artifacts were generated together on 2026-09-08 by one invocation of
`pifo_motivation_all.py`, from the working tree based on merge commit `1cf88bd`.
The runner used `rio.sim.EvaluationRequestSimulatorCli`, Verilator 5.039 and
Temurin JDK 17.0.20.1. No result below is carried over from the earlier
root-terminated topology.

Every flow now terminates at a hardware FIFO PIFO:

| Policy | Flow | Pop path |
| --- | --- | --- |
| p1 / p2a | zoom, gmail, spotify when present | SP root (PE 1) → dedicated FIFO (PE 3) → output |
| p2b | zoom | SP root (PE 1) → dedicated FIFO (PE 3) → output |
| p2b | gmail, spotify | SP root (PE 1) → shared RR (PE 3) → dedicated FIFO (PE 2) → output |

R4's old gmail FIFO and replacement RR occupy different vPIFOs on PE 3 so the
local underflow rewrite can switch the confined subtree; R2–R4 otherwise use
the same declared target tree. The simulator's per-flow packet-metadata depth
is 4096. The lossless source gate remains a separate unbounded model, so this
setting is not presented as a finite global-buffer proof.

## Measurements

All four runs replay the same 2,136-packet trace and complete every packet with
zero drops and no within-flow reorderings.

| Run | Install instructions | Commit accepted | Old tree/subtree drained | Ready for next commit | Post-start zoom max delay |
| --- | ---: | ---: | ---: | ---: | ---: |
| R1 additive | 7 | 2012 | not required | 2023 | 20 |
| R2 stop the world | 25 | 2039 | captured at 2012 | 3040 | 1055 |
| R3 whole-tree replace | 26 | 2031 | 2633 | 2659 | 613 |
| R4 confined replace | 16 | 2018 | 3033 | 3041 | 21 |

The R3-versus-R4 zoom-delay gap is **592 cycles**. The prior folded-leaf
artifacts measured 416 versus 17 cycles, a 399-cycle gap; the explicit FIFO
model therefore widens the observed gap by 193 cycles.

R3 drains for 602 cycles after command acceptance; R4's affected subtree
drains for 1,015 cycles while zoom remains on its unchanged path. R2's
lossless 1,024-cycle stop retains 203 admitted packets, reaches a measured peak
of 483 outstanding packets, and creates a 1,044-cycle output gap.

The machine-checked summary is [validation.txt](validation.txt). Per-run raw
request, admission, packet-outcome and reconfiguration CSVs sit beside the
compiled transaction programs. The comparison figures are [R2–R4 packet
delay](comparisons/r2-r4-delay-scatter/figure.png) and [R3/R4
throughput](comparisons/r3-r4-throughput/figure.png).
