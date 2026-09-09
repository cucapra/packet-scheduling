# Designated-survivor experiment

Measured RTL runs, with identical CBR traffic offered during all stops. Packet delay starts at generation, including time waiting at the input gate. All completed runs have zero packet drops and zero per-flow reorderings.

Every flow now terminates in its own hardware FIFO node. Push inserts a token into that FIFO as well as each policy node on the path. Pop traverses the policy nodes and performs a separate FIFO pop before packet completion; no policy node routes directly to a packet output.

p1 paths are `root (PE 1) → per-flow FIFO (PE 3)`. Under p2b, zoom keeps that shape; gmail and spotify use `root (PE 1) → RR (PE 2) → per-flow FIFO (PE 3)`. Old/new FIFO versions have distinct vPIFO IDs. The reserved wrapper uses PE 4; Strict* leaves it unused. Compiled physical paths are recorded in each transactions.plan.json.

Copy + prefill relocates the frozen old root PE 1 -> 2 and FIFO PE 3 -> 4, then creates the wrapper at the original root on PE 1. The new tree is freshly installed at PEs 2/3/4. Teardown publishes its root and reclaims the wrapper, as in the reserved baseline. This measures old-tree descent, not live-survivor ascent or brain-state migration; the latter protocol remains unsupported. All three runs have the same four-PE hardware shape and identical initial tree and traffic.

Both hardware variants use the current shared command FIFO (256 entries) and immediate mapper-write replay. Four PEs widen post-mapper addresses, but replay does not scan the full 512-entry bank. Cleanup waits on GuardDrain for every retired FIFO and uses ordinary invalidation writes and commits. The evaluation-only prefill/copy hardware is absent from the production image.

## Birth: measured stop cycles

| Pre-phase | Backlog at t1 | Mechanism | Total stop | Actual prefill N | Entry writes | Copy cycles | Fixed overhead | Hardware gate stop | Peak stop buffer |
| ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | Strict* link | 0 | 0 | 0 | 0 | 0 | 0 | — |
| 0 | 0 | Reserved-PE Strict wrapper | 83 | 1 | 1 | 0 | 82 | 71 | 24 |
| 0 | 0 | Copy + prefill Strict wrapper | 106 | 1 | 1 | 4 | 101 | 94 | 31 |
| 1000 | 74 | Strict* link | 0 | 0 | 0 | 0 | 0 | 0 | — |
| 1000 | 74 | Reserved-PE Strict wrapper | 153 | 70 | 70 | 0 | 83 | 140 | 113 |
| 1000 | 74 | Copy + prefill Strict wrapper | 314 | 70 | 70 | 142 | 102 | 301 | 154 |
| 2000 | 141 | Strict* link | 0 | 0 | 0 | 0 | 0 | 0 | — |
| 2000 | 141 | Reserved-PE Strict wrapper | 220 | 137 | 137 | 0 | 83 | 207 | 197 |
| 2000 | 141 | Copy + prefill Strict wrapper | 515 | 137 | 137 | 276 | 102 | 502 | 276 |
| 2500 | 174 | Strict* link | 0 | 0 | 0 | 0 | 0 | 0 | — |
| 2500 | 174 | Reserved-PE Strict wrapper | 253 | 170 | 170 | 0 | 83 | 240 | 238 |
| 2500 | 174 | Copy + prefill Strict wrapper | 614 | 170 | 170 | 342 | 102 | 601 | 334 |
| 5000 | 341 | Strict* link | 0 | 0 | 0 | 0 | 0 | 0 | — |
| 5000 | 341 | Reserved-PE Strict wrapper | 420 | 337 | 337 | 0 | 83 | 407 | 452 |
| 5000 | 341 | Copy + prefill Strict wrapper | 1115 | 337 | 337 | 676 | 102 | 1102 | 636 |

The headline stop includes driver quiescence before the RTL StopWorld gate, configuration, copy where used, prefill and commit publication. Hardware gate width is listed separately. Entry writes count actual ready/valid insertions, not an estimate. Copy cycles sum dispatch-to-completion intervals. Fixed overhead is the measured total minus prefill write cycles and copy-command cycles.
Strict* has no global pop stop and one designate instruction, but compiling/installing the new tree still costs instructions and a short input-only commit barrier. Zero is not a claim that the entire tree change is instantaneous.

## Death: root change and nonempty wrapper reclamation

Costs start when both the old root is empty and the creation package is ready for another transaction; the drain wait itself is excluded. `finish` includes mapper-bank synchronization, not a packet outage.

| Pre-phase | Mechanism | Root change | Reclaimed | All config finished | Tokens at detach | Tokens discarded | Packets served later |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | Reserved-PE Strict wrapper | 23 | 39 | 55 | 22 | 21 | 2119 |
| 0 | Copy + prefill Strict wrapper | 23 | 39 | 55 | 27 | 26 | 2113 |
| 1000 | Reserved-PE Strict wrapper | 23 | 39 | 55 | 95 | 96 | 2125 |
| 1000 | Copy + prefill Strict wrapper | 23 | 39 | 55 | 139 | 138 | 2125 |
| 2000 | Reserved-PE Strict wrapper | 23 | 39 | 55 | 167 | 166 | 2125 |
| 2000 | Copy + prefill Strict wrapper | 23 | 39 | 55 | 246 | 246 | 2125 |
| 2500 | Reserved-PE Strict wrapper | 23 | 39 | 55 | 202 | 202 | 2125 |
| 2500 | Copy + prefill Strict wrapper | 23 | 39 | 55 | 299 | 298 | 2125 |
| 5000 | Reserved-PE Strict wrapper | 23 | 39 | 55 | 380 | 381 | 2390 |
| 5000 | Copy + prefill Strict wrapper | 23 | 39 | 55 | 566 | 566 | 2390 |

UpdateRoot publishes the survivor as the port root. ClearPifoEngine then clears the detached wrapper's occupancy counters in one hardware cycle; its redundant tokens are discarded, not copied, and the survivor's packet queues remain intact. Old memory bits need not be individually erased. This substrate therefore supports nonempty death with constant-time logical reclamation; death is not an O(backlog) token-deletion loop. No additional global stop occurs at teardown.

## Drain and instruction accounting

| Pre-phase | Mechanism | Commit applied | Old root N at publication | Old root empty | Post-commit drain | Install / cleanup / reclaim inst | Categories |
| ---: | --- | ---: | ---: | ---: | ---: | --- | --- |
| 0 | Strict* link | 35 | 0 | 36 | 1 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 0 | Reserved-PE Strict wrapper | 83 | 1 | 92 | 9 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 0 | Copy + prefill Strict wrapper | 106 | 1 | 115 | 9 | 58 / 20 / 11 | {'rest': 44, 'copy': 2, 'wrapper_creation': 11, 'prefill': 1} |
| 1000 | Strict* link | 1036 | 65 | 1231 | 195 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 1000 | Reserved-PE Strict wrapper | 1153 | 70 | 1369 | 216 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 1000 | Copy + prefill Strict wrapper | 1314 | 70 | 1530 | 216 | 58 / 20 / 11 | {'rest': 44, 'copy': 2, 'wrapper_creation': 11, 'prefill': 1} |
| 2000 | Strict* link | 2035 | 132 | 2431 | 396 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 2000 | Reserved-PE Strict wrapper | 2220 | 137 | 2637 | 417 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 2000 | Copy + prefill Strict wrapper | 2515 | 137 | 2932 | 417 | 58 / 20 / 11 | {'rest': 44, 'copy': 2, 'wrapper_creation': 11, 'prefill': 1} |
| 2500 | Strict* link | 2536 | 165 | 3031 | 495 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 2500 | Reserved-PE Strict wrapper | 2753 | 170 | 3269 | 516 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 2500 | Copy + prefill Strict wrapper | 3114 | 170 | 3630 | 516 | 58 / 20 / 11 | {'rest': 44, 'copy': 2, 'wrapper_creation': 11, 'prefill': 1} |
| 5000 | Strict* link | 5035 | 332 | 6031 | 996 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 5000 | Reserved-PE Strict wrapper | 5420 | 337 | 6437 | 1017 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 5000 | Copy + prefill Strict wrapper | 6115 | 337 | 7132 | 1017 | 58 / 20 / 11 | {'rest': 44, 'copy': 2, 'wrapper_creation': 11, 'prefill': 1} |

`start` is transaction execution start; `commit` in reconfiguration-events.csv is instruction acceptance, not publication; controller-instructions.csv records actual publication. `drained` means the final old-root token popped (lower-level traversals may still be in flight). A row's `install_finish_cycle` is that commit's own replay-ready time; `finish_cycle` includes its associated cleanup. The last reclamation row marks final readiness. All commits are drawn independently with a shared old-tree drain, not a fictional second/third drain.
Hardware gate register transitions are observed on the following rising-edge sample; both edges have the same offset, so their width is unaffected. The driver stop ends at commit publication, not at completion of mapper-bank synchronization.

| Pre-phase | Run | Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |
| ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | link | C1 | 0 | 32 | 35 | 52 | 26 | 35 | 17 |
| 0 | link | C2 | 52 | 69 | 71 | 78 | 16 | 19 | 7 |
| 0 | reserved | C1 | 0 | 48 | 83 | 112 | 39 | 83 | 29 |
| 0 | reserved | C2 | 112 | 132 | 136 | 146 | 20 | 24 | 10 |
| 0 | reserved | C3 | 146 | 158 | 160 | 167 | 11 | 14 | 7 |
| 0 | copy | C1 | 0 | 66 | 106 | 155 | 58 | 106 | 49 |
| 0 | copy | C2 | 155 | 175 | 178 | 189 | 20 | 23 | 11 |
| 0 | copy | C3 | 189 | 200 | 204 | 210 | 11 | 15 | 6 |
| 1000 | link | C1 | 1000 | 1034 | 1036 | 1053 | 26 | 36 | 17 |
| 1000 | link | C2 | 1053 | 1069 | 1250 | 1257 | 16 | 197 | 7 |
| 1000 | reserved | C1 | 1000 | 1049 | 1153 | 1182 | 39 | 153 | 29 |
| 1000 | reserved | C2 | 1182 | 1203 | 1392 | 1403 | 20 | 210 | 11 |
| 1000 | reserved | C3 | 1403 | 1415 | 1417 | 1424 | 11 | 14 | 7 |
| 1000 | copy | C1 | 1000 | 1067 | 1314 | 1363 | 58 | 314 | 49 |
| 1000 | copy | C2 | 1363 | 1383 | 1553 | 1564 | 20 | 190 | 11 |
| 1000 | copy | C3 | 1564 | 1575 | 1578 | 1585 | 11 | 14 | 7 |
| 2000 | link | C1 | 2000 | 2032 | 2035 | 2052 | 26 | 35 | 17 |
| 2000 | link | C2 | 2052 | 2068 | 2450 | 2457 | 16 | 398 | 7 |
| 2000 | reserved | C1 | 2000 | 2048 | 2220 | 2249 | 39 | 220 | 29 |
| 2000 | reserved | C2 | 2249 | 2270 | 2660 | 2671 | 20 | 411 | 11 |
| 2000 | reserved | C3 | 2671 | 2682 | 2685 | 2692 | 11 | 14 | 7 |
| 2000 | copy | C1 | 2000 | 2068 | 2515 | 2564 | 58 | 515 | 49 |
| 2000 | copy | C2 | 2564 | 2585 | 2955 | 2966 | 20 | 391 | 11 |
| 2000 | copy | C3 | 2966 | 2977 | 2980 | 2987 | 11 | 14 | 7 |
| 2500 | link | C1 | 2500 | 2534 | 2536 | 2553 | 26 | 36 | 17 |
| 2500 | link | C2 | 2553 | 2569 | 3050 | 3057 | 16 | 497 | 7 |
| 2500 | reserved | C1 | 2500 | 2549 | 2753 | 2782 | 39 | 253 | 29 |
| 2500 | reserved | C2 | 2782 | 2802 | 3292 | 3303 | 20 | 510 | 11 |
| 2500 | reserved | C3 | 3303 | 3314 | 3317 | 3324 | 11 | 14 | 7 |
| 2500 | copy | C1 | 2500 | 2567 | 3114 | 3163 | 58 | 614 | 49 |
| 2500 | copy | C2 | 3163 | 3183 | 3653 | 3664 | 20 | 490 | 11 |
| 2500 | copy | C3 | 3664 | 3675 | 3678 | 3685 | 11 | 14 | 7 |
| 5000 | link | C1 | 5000 | 5032 | 5035 | 5052 | 26 | 35 | 17 |
| 5000 | link | C2 | 5052 | 5068 | 6050 | 6057 | 16 | 998 | 7 |
| 5000 | reserved | C1 | 5000 | 5048 | 5420 | 5449 | 39 | 420 | 29 |
| 5000 | reserved | C2 | 5449 | 5470 | 6460 | 6471 | 20 | 1011 | 11 |
| 5000 | reserved | C3 | 6471 | 6482 | 6485 | 6492 | 11 | 14 | 7 |
| 5000 | copy | C1 | 5000 | 5068 | 6115 | 6164 | 58 | 1115 | 49 |
| 5000 | copy | C2 | 6164 | 6185 | 7155 | 7166 | 20 | 991 | 11 |
| 5000 | copy | C3 | 7166 | 7177 | 7180 | 7187 | 11 | 14 | 7 |

In Figure A, zoom's peak delay is 411 versus 647 cycles: +236, compared with the 220-cycle stop. The post-commit old-root drains are 396 and 417 cycles.
Those drains start with 132 and 137 tokens respectively: Strict* kept serving the old policy while its commands arrived. The difference in token counts and wrapper traversal latency explains why the drain widths need not be exactly equal.
At t1=2000, the measured backlog is 141 packets and pre-transition link utilization is 0.990. These values are recomputed from the current packet traces.

Copy + prefill stops for 515 cycles: 137 prefill writes + 276 copy cycles + 102 other cycles. It moves 2 occupied PIFOs containing 274 scheduler tokens for 137 buffered packets. Its zoom peak delay is 942 cycles, with a 417-cycle post-publication old-root drain.

| Pre-phase | Copied occupied PIFOs | Copied scheduler tokens | Buffered packets | Copy cycles |
| ---: | ---: | ---: | ---: | ---: |
| 0 | 2 | 2 | 1 | 4 |
| 1000 | 2 | 140 | 70 | 142 |
| 2000 | 2 | 274 | 137 | 276 |
| 2500 | 2 | 340 | 170 | 342 |
| 5000 | 2 | 674 | 337 | 676 |

| Pre-phase | Mechanism | Last packet pop | All configuration finished |
| ---: | --- | ---: | ---: |
| 0 | Strict* link | 8026 | 78 |
| 0 | Reserved-PE Strict wrapper | 8026 | 167 |
| 0 | Copy + prefill Strict wrapper | 8025 | 210 |
| 1000 | Strict* link | 9025 | 1257 |
| 1000 | Reserved-PE Strict wrapper | 9025 | 1424 |
| 1000 | Copy + prefill Strict wrapper | 9026 | 1585 |
| 2000 | Strict* link | 10026 | 2457 |
| 2000 | Reserved-PE Strict wrapper | 10025 | 2692 |
| 2000 | Copy + prefill Strict wrapper | 10025 | 2987 |
| 2500 | Strict* link | 10525 | 3057 |
| 2500 | Reserved-PE Strict wrapper | 10525 | 3324 |
| 2500 | Copy + prefill Strict wrapper | 10526 | 3685 |
| 5000 | Strict* link | 14019 | 6057 |
| 5000 | Reserved-PE Strict wrapper | 14019 | 6492 |
| 5000 | Copy + prefill Strict wrapper | 14339 | 7187 |

## Scope and capacity

4096 packet metadata FIFO entries per flow; each flow also has a real hardware FIFO PIFO (PE 3 before transition; PE 4 after copy). The 1024 scheduler-token slots on each PE are shared across its virtual PIFOs, including old/new FIFO versions. The source gate queue is unbounded and measured, not claimed as hardware RAM.

The pre-phases remain 0, 1000, 2500 and 5000 cycles, plus the 2000-cycle Figure A run. Rates, sizes, seeds, durations and FIFO topology are unchanged. Figure B uses the measured backlog from the current run. See [rerun provenance](../evaluation-merge-rerun.md) for simulator changes.
The 1000/2000 points were not run: this experiment's 1024-token PE cannot contain 2000 root tokens. Growing that sorted-register RTL and rerunning is required; silently leaving excess packets at the door would not measure a 2000-token prefill.

At the zero-pre-phase point, packets may already be entering as the stop starts. The hardware snapshot/prefill count is recorded separately from backlog immediately before t1.

One permanently reserved PE is needed per concurrent materialized wrapper. The Strict* run leaves that PE unused to keep the physical simulation shape identical. Reservation avoids descent and ascent but does not avoid the N birth writes.

Copy + prefill relocates the frozen old root PE 1 -> 2 and FIFO PE 3 -> 4, then creates the wrapper at the original root on PE 1. The new tree is freshly installed at PEs 2/3/4. Teardown publishes its root and reclaims the wrapper, as in the reserved baseline. This measures old-tree descent, not live-survivor ascent or brain-state migration; the latter protocol remains unsupported.

## Figures and raw files

- [Figure A: zoom delay](figures/zoom-delay/figure.png) ([SVG](figures/zoom-delay/figure.svg), [plotted CSV](figures/zoom-delay/data.csv), [standalone plotter](figures/zoom-delay/plot.py)).
- [Figure B: stop versus backlog](figures/prefill-stop/figure.png) ([SVG](figures/prefill-stop/figure.svg), [plotted CSV](figures/prefill-stop/data.csv), [standalone plotter](figures/prefill-stop/plot.py)).

Each figure folder also contains complete packets.csv and commits.csv. Copy that folder elsewhere and run `python plot.py`; only Matplotlib and the local CSVs are needed. Blue/amber/green backgrounds distinguish install, cleanup/collapse, and wrapper reclamation commits. Delay axes have vertical time markers only.

- [Measurements](measurements.json). Each pre-N directory contains its traffic input; link/, reserved/ and copy/ contain direct transactions, compiler accounting, requests.csv, packet-outcomes.csv, request-results.csv, reconfiguration-events.csv, controller-instructions.csv and maintenance-events.csv.

Per-flow packet metadata FIFO order is checked end-to-end; the RTL tokens carry flow IDs, not unique packet IDs. These measurements are not a gate-level timing-closure result.
