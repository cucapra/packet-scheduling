# Large-tree experiment results

All runs replay the identical source trace. All four transitioning runs finish with zero drops and zero per-flow reorderings.
Control keeps p1; its four unadmitted flows are recorded as unserved, not dropped.

## First service after the request (cycles)

| Mechanism | Video | Chat | Game | Vr | Peak stop buffer (packets) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Rio: localized edits | 96 | 210 | 111 | 135 | no global stop |
| Whole-tree: prefill SP | 1583 | 3356 | 1586 | 1610 | 495 |
| Whole-tree: copy + prefill | 2594 | 4889 | 2597 | 2621 | 781 |
| Stop-the-world reset | 1078 | 1381 | 1081 | 1099 | 624 |

Chat is below Video in a real SP tenant: first Chat service includes draining Video's new-policy backlog, not just the old-tree drain.

## Timings and instructions

`commit` is instruction acceptance, `applied` is hardware publication. `empty/captured` is old PIFO empty except for reset, where it is the retained-state snapshot. `install_finish_cycle` is each commit's own replay readiness; `finish_cycle` includes its linked cleanup. Final configuration readiness is the last cleanup/reclamation commit, not the end of packet traffic.

| Run | Start | Commit | Applied | Empty/captured | Finish | Cleanup finished | Main / guarded instructions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| rio | 2001 | 2060 | 2063 | 4111 | 4143 | 4143 | 51 / 20 |
| prefill | 2001 | 2215 | 2575 | 3557 | 3695 | 3739 | 201 / 120 |
| relocate | 2001 | 2248 | 3586 | 4568 | 4706 | 4750 | 234 / 120 |
| reset | 2001 | 2173 | 2176 | 2019 | 6433 | 6433 | 152 / 20 |

| Run | Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| rio | C1 | 2001 | 2060 | 2063 | 2093 | 51 | 62 | 30 |
| rio | C2 | 2093 | 2114 | 4135 | 4143 | 20 | 2042 | 8 |
| prefill | C1 | 2001 | 2215 | 2575 | 2704 | 201 | 574 | 129 |
| prefill | C2 | 2704 | 2797 | 3652 | 3695 | 92 | 948 | 43 |
| prefill | C3 | 3695 | 3724 | 3726 | 3739 | 28 | 31 | 13 |
| relocate | C1 | 2001 | 2248 | 3586 | 3745 | 234 | 1585 | 159 |
| relocate | C2 | 3745 | 3838 | 4663 | 4706 | 92 | 918 | 43 |
| relocate | C3 | 4706 | 4735 | 4737 | 4750 | 28 | 31 | 13 |
| reset | C1 | 2001 | 2173 | 2176 | 2260 | 152 | 175 | 84 |
| reset | C2 | 3044 | 3064 | 6425 | 6433 | 20 | 3381 | 8 |

There is one config ingress instruction per cycle, not five instantaneous hardware edits. Raw `controller-instructions.csv` distinguishes queue acceptance, dispatch, commit publication and copy completion.

## Departing backlog and unchanged-path witnesses

Measured backlog at t1 is identical in every run; none of it is waiting at the input gate:

| Flow | Packets at t1 | Rio p95 absolute delay difference from control |
| --- | ---: | ---: |
| Zoom | 1 | 11 |
| Voip | 45 | 377 |
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

Untouched-path witnesses exceeding the stated paired-delay threshold: Gmail, Ssh, Http, Https. Identical internal paths do not by themselves guarantee identical service at a shared root whose sibling weights and load change. The table reports the measured backlogs and differences rather than assuming the ideal arithmetic.
Their absolute root weight stays 3, but total configured sibling weight changes from 15 to 21 including the retiring legacy arm, then 17. Nominal all-backlogged fractions therefore change from 3/15 to 3/21 to 3/17; actual service also depends on frozen ranks and empty queues.
The measured pre-t1 link utilization, including startup, is 0.909, not the 1.00 assumed in the ideal backlog arithmetic.

Difference test: paired p95 absolute delay difference > 10 cycles, packets outstanding/generated from t1 through 18000; new flows count as changed.

Changed flow counts: rio=14/14, prefill=14/14, relocate=14/14, reset=14/14.

Relocation moves 14 occupied virtual PIFOs across three PEs, containing 975 scheduler tokens for 325 buffered packets. The dedicated brain-bypassing read/inject datapath exists only in the evaluation image; payload buffers are not copied.
The lossless reset's 512-cycle teardown and 513-cycle install budgets are model parameters forming a minimum stop, not extra controller instructions. Actual bank synchronization and replay can make it longer.

## Realtime recovery

Realtime's new nominal weight share is 6/17, but its source offers only 0.20. The following is settling to the offered-load steady throughput, not a saturated-share measurement:

| Run | Pre-t1 realtime packets all served | Sustained 0.20 throughput from cycle |
| --- | ---: | ---: |
| control | 3503 | 4403 |
| rio | 3575 | 5125 |
| prefill | 3064 | 6064 |
| relocate | 4075 | 9225 |
| reset | 4536 | 6986 |

## Figures and raw data

- [A: first service](figures/first-service/figure.png)
- [B: unchanged-path delays](figures/untouched-delay/figure.png)
- [A-copy: prefill versus relocation](figures/first-service-copy/figure.png)
- [B-copy: prefill versus relocation delays](figures/untouched-delay-copy/figure.png)

Each figure has SVG, data.csv, complete packets.csv, commits.csv and a self-contained plot.py. Copy the folder elsewhere and run `python plot.py` using only Matplotlib and those local CSVs. Timeline panels show start, acceptance, replay readiness and the shared old-tree drain for every commit; blue/amber/green backgrounds distinguish installation, cleanup and reclamation. Each run directory contains the full generated/admitted/completed timestamps, controller trace and packet outcomes. The complete machine-readable report is [measurements.json](measurements.json).
