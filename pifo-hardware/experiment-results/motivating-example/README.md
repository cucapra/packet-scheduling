# Motivating example: explicit FIFO-leaf rerun

These artifacts were generated together on 2026-09-09 by one invocation of
`pifo_motivation_all.py`, from the working tree based on `d1d44a0`, after fixing
the dequeue driver's rising-edge cycle-counter race.
The runner used `rio.sim.EvaluationRequestSimulatorCli`, Verilator 5.039 and
Temurin JDK 17.0.20.1. All four hardware cases completed before the comparison
was rendered from their new CSVs.

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

During cycles 200–1700, each run serves 500 packets: 100% of the 16-byte/cycle
link, split 40% zoom and 60% gmail. The earlier driver served only 450 packets
in that interval because it sometimes read a stale cycle counter and waited
an extra cycle. The dequeue check now runs on falling edges, after the
rising-edge observer updates the counter. The initial empty-pipeline ramp
and the plot's 240-cycle Hann smoothing remain visible.

| Run | Install instructions | Commit accepted | Old tree/subtree drained | Ready for next commit | Post-start zoom max delay |
| --- | ---: | ---: | ---: | ---: | ---: |
| R1 additive | 7 | 2013 | not required | 2024 | 22 |
| R2 stop the world | 25 | 2040 | captured at 2012 | 3040 | 1050 |
| R3 whole-tree replace | 26 | 2032 | 2431 | 2457 | 411 |
| R4 confined replace | 16 | 2026 | 2696 | 2704 | 23 |

The R3-versus-R4 zoom-delay gap is **388 cycles**. The previous 592-cycle
explicit-leaf result and 399-cycle folded-leaf result used the faulty driver
and are superseded. They do not establish that explicit FIFO leaves widen
the gap: the driver bug also inflated the pre-transition backlog.

R3 drains for 399 cycles after command acceptance; R4's affected subtree
drains for 670 cycles while zoom remains on its unchanged path. R2's
lossless 1,024-cycle stop retains 137 admitted packets, reaches a measured peak
of 416 outstanding packets, and creates a 1,041-cycle output gap.

The machine-checked summary is [validation.txt](validation.txt). Per-run raw
request, admission, packet-outcome and reconfiguration CSVs sit beside the
compiled transaction programs. The comparison figures are [R2–R4 packet
delay](comparisons/r2-r4-delay-scatter/figure.png) and [R3/R4
throughput](comparisons/r3-r4-throughput/figure.png).
