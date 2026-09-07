# Large-tree experiment results

All runs replay the identical source trace. All four transitioning runs finish with zero drops and zero per-flow reorderings.
Control keeps p1; its four unadmitted flows are recorded as unserved, not dropped.

## First service after the request (cycles)

| Mechanism | Video | Chat | Game | Vr | Peak stop buffer (packets) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Rio: localized edits | 96 | 210 | 111 | 135 | no global stop |
| Whole-tree: prefill SP | 1582 | 3290 | 1585 | 1609 | 495 |
| Whole-tree: copy + prefill | 2593 | 4889 | 2596 | 2620 | 781 |
| Stop-the-world reset | 2591 | 2948 | 2603 | 2627 | 1058 |

Chat is below Video in a real SP tenant: first Chat service includes draining Video's new-policy backlog, not just the old-tree drain.

## Timings and instructions

`commit` is instruction acceptance, `applied` is hardware publication. `empty/captured` is old PIFO empty except for reset, where it is the retained-state snapshot. `finish` is the main configuration package; cleanup completion is separate.

| Run | Start | Commit | Applied | Empty/captured | Finish | Cleanup finished | Main / guarded instructions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| rio | 2001 | 2060 | 2063 | 4111 | 4112 | 6175 | 51 / 11 |
| prefill | 2001 | 2247 | 2575 | 3556 | 4624 | 8751 | 201 / 17 |
| relocate | 2001 | 3258 | 3586 | 4567 | 5635 | 9772 | 234 / 17 |
| reset | 2001 | 2173 | 2176 | 2019 | 4559 | 10046 | 152 / 11 |

There is one config ingress instruction per cycle, not five instantaneous hardware edits. Raw `controller-instructions.csv` distinguishes queue acceptance, dispatch, commit publication and copy completion.

## Departing backlog and unchanged-path witnesses

Measured backlog at t1 is identical in every run; none of it is waiting at the input gate:

| Flow | Packets at t1 | Rio p95 absolute delay difference from control |
| --- | ---: | ---: |
| Zoom | 1 | 11 |
| Voip | 45 | 376 |
| Legacy1 | 86 | 604 |
| Legacy2 | 53 | 371 |
| Gmail | 12 | 426 |
| Ssh | 1 | 15 |
| Http | 12 | 487 |
| Https | 1 | 12 |
| Backup | 79 | 2702 |
| Sync | 40 | 1590 |
| Video | 0 | not served by control |
| Chat | 0 | not served by control |
| Game | 0 | not served by control |
| Vr | 0 | not served by control |

The stronger prediction that unchanged paths have unchanged delay is **not supported**: Gmail and Http show a transient increase even in Rio. Work and web already have nonzero backlog at t1. Reweighting siblings and admitting new tenants changes their shared-root service; these observations are reported, not treated as a successful indistinguishability test.
Their absolute root weight stays 3, but total configured sibling weight changes from 15 to 21 including the retiring legacy arm, then 17. Nominal all-backlogged fractions therefore change from 3/15 to 3/21 to 3/17; actual service also depends on frozen ranks and empty queues.
The measured pre-t1 link utilization, including startup, is 0.909, not the 1.00 assumed in the ideal backlog arithmetic.

Difference test: paired p95 absolute delay difference > 10 cycles, packets outstanding/generated from t1 through 18000; new flows count as changed.

Changed flow counts: rio=14/14, prefill=14/14, relocate=14/14, reset=14/14.

Relocation copies 975 PIFO entries across three PEs; copied entries are scheduler tokens, not three copies of packet payload buffers.
The lossless reset's 512-cycle teardown and 513-cycle install budgets are model parameters forming a minimum stop, not extra controller instructions. Actual bank synchronization and replay can make it longer.

## Realtime recovery

Realtime's new nominal weight share is 6/17, but its source offers only 0.20. The following is settling to the offered-load steady throughput, not a saturated-share measurement:

| Run | Pre-t1 realtime packets all served | Sustained 0.20 throughput from cycle |
| --- | ---: | ---: |
| control | 3503 | 4403 |
| rio | 3575 | 5125 |
| prefill | 3063 | 5963 |
| relocate | 4074 | 9224 |
| reset | 6112 | 16012 |

## Figures and raw data

- [A: first service](figures/first-service/figure.png)
- [B: unchanged-path delays](figures/untouched-delay/figure.png)
- [A-copy: prefill versus relocation](figures/first-service-copy/figure.png)
- [B-copy: prefill versus relocation delays](figures/untouched-delay-copy/figure.png)

Each figure has SVG and data.csv siblings. Each run directory contains the full generated/admitted/completed timestamps, controller trace and packet outcomes. The complete machine-readable report is [measurements.json](measurements.json).
