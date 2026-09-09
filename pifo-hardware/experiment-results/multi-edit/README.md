# Large-tree experiment results

All six runs replay the identical source trace. All four transitioning runs finish with zero drops and zero per-flow reorderings.
The p1 control leaves the four arriving flows unadmitted. The p2 control starts and stays in p2, so the two legacy flows absent from p2 are unadmitted. Both cases are recorded as unserved, not dropped.

## First service after the request (cycles)

| Mechanism | Video | Chat | Game | Vr | Peak stop buffer (packets) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Control: p2 | 35 | 47 | 38 | 50 | no global stop |
| Rio: localized edits | 106 | 247 | 109 | 139 | no global stop |
| Whole-tree: prefill SP | 1372 | 2800 | 1375 | 1399 | 424 |
| Whole-tree: copy + prefill | 2227 | 4522 | 2230 | 2254 | 669 |
| Stop-the-world reset | 1075 | 1585 | 1078 | 1090 | 571 |

Chat is below Video in a real SP tenant: first Chat service includes draining Video's new-policy backlog, not just the old-tree drain.

## Timings and instructions

`commit` is instruction acceptance, `applied` is hardware publication. `empty/captured` is old PIFO empty except for reset, where it is the retained-state snapshot. `install_finish_cycle` is each commit's own replay readiness; `finish_cycle` includes its linked cleanup. Final configuration readiness is the last cleanup/reclamation commit, not the end of packet traffic.

| Run | Start | Commit | Applied | Empty/captured | Finish | Cleanup finished | Main / guarded instructions |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| rio | 2001 | 2061 | 2064 | 3884 | 3916 | 3916 | 51 / 20 |
| prefill | 2001 | 2213 | 2521 | 3346 | 3484 | 3528 | 201 / 120 |
| relocate | 2001 | 2246 | 3376 | 4201 | 4339 | 4383 | 234 / 120 |
| reset | 2001 | 2171 | 2173 | 2016 | 6202 | 6202 | 152 / 20 |

| Run | Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| rio | C1 | 2001 | 2061 | 2065 | 2094 | 51 | 64 | 29 |
| rio | C2 | 2094 | 2115 | 3908 | 3916 | 20 | 1814 | 8 |
| prefill | C1 | 2001 | 2213 | 2521 | 2650 | 201 | 520 | 129 |
| prefill | C2 | 2650 | 2743 | 3441 | 3484 | 92 | 791 | 43 |
| prefill | C3 | 3484 | 3513 | 3515 | 3528 | 28 | 31 | 13 |
| relocate | C1 | 2001 | 2246 | 3376 | 3535 | 234 | 1375 | 159 |
| relocate | C2 | 3535 | 3628 | 4296 | 4339 | 92 | 761 | 43 |
| relocate | C3 | 4339 | 4367 | 4371 | 4383 | 28 | 32 | 12 |
| reset | C1 | 2001 | 2171 | 2173 | 2258 | 152 | 172 | 85 |
| reset | C2 | 3041 | 3061 | 6194 | 6202 | 20 | 3153 | 8 |

There is one config ingress instruction per cycle, not five instantaneous hardware edits. Raw `controller-instructions.csv` distinguishes queue acceptance, dispatch, commit publication and copy completion.

## Departing backlog and unchanged-path witnesses

The four transitioning runs start from p1 and have the same measured backlog at t1 as the p1 control; none of it is waiting at the input gate. The steady-p2 control intentionally has a different pre-t1 policy state.

| Flow | Packets at t1 | Rio p95 difference vs p1 | Rio p95 difference vs p2 |
| --- | ---: | ---: | ---: |
| Zoom | 0 | 12 | 8 |
| Voip | 35 | 327 | 1200 |
| Legacy1 | 79 | 648 | not jointly served |
| Legacy2 | 46 | 417 | not jointly served |
| Gmail | 1 | 135 | 138 |
| Ssh | 1 | 18 | 16 |
| Http | 2 | 150 | 150 |
| Https | 1 | 11 | 15 |
| Backup | 76 | 1845 | 3041 |
| Sync | 36 | 1128 | 1930 |
| Video | 0 | not jointly served | 12 |
| Chat | 0 | not jointly served | 244 |
| Game | 0 | not jointly served | 153 |
| Vr | 0 | not jointly served | 27 |

Untouched-path witnesses exceeding the stated paired-delay threshold versus p1: Gmail, Ssh, Http, Https; versus steady p2: Gmail, Ssh, Http, Https. Identical internal paths do not by themselves guarantee identical service at a shared root whose sibling weights and load change. The table reports both controls rather than assuming either is the sole counterfactual.
Their absolute root weight stays 3, but total configured sibling weight changes from 15 to 21 including the retiring legacy arm, then 17. Nominal all-backlogged fractions therefore change from 3/15 to 3/21 to 3/17; actual service also depends on frozen ranks and empty queues.
The measured pre-t1 link utilization, including startup, is 0.989, not the 1.00 assumed in the ideal backlog arithmetic.

Difference test: paired p95 absolute delay difference > 10 cycles, packets outstanding/generated from t1 through 18000; admission differences count as changed.

Changed flow counts versus p1: rio=14/14, prefill=14/14, relocate=14/14, reset=14/14.
Changed flow counts versus p2: rio=13/14, prefill=14/14, relocate=14/14, reset=14/14.

Relocation moves 14 occupied virtual PIFOs across three PEs, containing 819 scheduler tokens for 273 buffered packets. The dedicated brain-bypassing read/inject datapath exists only in the evaluation image; payload buffers are not copied.
The lossless reset's 512-cycle teardown and 513-cycle install budgets are model parameters forming a minimum stop, not extra controller instructions. Actual bank synchronization and replay can make it longer.

## Realtime recovery

Realtime's new nominal weight share is 6/17, but its source offers only 0.20. The following is settling to the offered-load steady throughput, not a saturated-share measurement:

| Run | Pre-t1 realtime packets all served | Sustained 0.20 throughput from cycle |
| --- | ---: | ---: |
| control | 2949 | 3849 |
| control-p2 | 2002 | 2302 |
| rio | 3159 | 4509 |
| prefill | 2862 | 5362 |
| relocate | 3717 | 7367 |
| reset | 4476 | 6676 |

## Figures and raw data

- [A: first service](figures/first-service/figure.png)
- [B: unchanged-path delays](figures/untouched-delay/figure.png)
- [A-copy: prefill versus relocation](figures/first-service-copy/figure.png)
- [B-copy: prefill versus relocation delays](figures/untouched-delay-copy/figure.png)

Each figure has SVG, data.csv, complete packets.csv, commits.csv and a self-contained plot.py. Copy the folder elsewhere and run `python plot.py` using only Matplotlib and those local CSVs. Timeline panels show start, acceptance, replay readiness and the shared old-tree drain for every commit; blue/amber/green backgrounds distinguish installation, cleanup and reclamation. Each run directory contains the full generated/admitted/completed timestamps, controller trace and packet outcomes. The complete machine-readable report is [measurements.json](measurements.json).
