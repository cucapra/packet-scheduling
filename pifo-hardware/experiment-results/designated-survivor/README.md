# Designated-survivor experiment

Measured RTL runs, with identical CBR traffic offered during all stops. Packet delay starts at generation, including time waiting at the input gate. All completed runs have zero packet drops and zero per-flow reorderings.

Every flow now terminates in its own hardware FIFO node. Push inserts a token into that FIFO as well as each policy node on the path. Pop traverses the policy nodes and performs a separate FIFO pop before packet completion; no policy node routes directly to a packet output.

p1 paths are `root (PE 1) → per-flow FIFO (PE 3)`. Under p2b, zoom keeps that shape; gmail and spotify use `root (PE 1) → RR (PE 2) → per-flow FIFO (PE 3)`. Old/new FIFO versions have distinct vPIFO IDs. The reserved wrapper uses PE 4; Strict* leaves it unused. Compiled physical paths are recorded in each transactions.plan.json.

Both hardware variants use the current shared command FIFO (256 entries) and immediate mapper-write replay. Four PEs widen post-mapper addresses, but replay does not scan the full 512-entry bank. Cleanup waits on GuardDrain for every retired FIFO and uses ordinary invalidation writes and commits. The evaluation-only prefill/copy hardware is absent from the production image.

## Birth: measured stop cycles

| Pre-phase | Backlog at t1 | Strict* stop | Reserved stop | Actual prefill N | Entry writes | Fixed overhead | Hardware gate stop | Peak stop buffer |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 83 | 1 | 1 | 82 | 71 | 24 |
| 1000 | 107 | 0 | 185 | 103 | 103 | 82 | 173 | 155 |
| 2000 | 207 | 0 | 284 | 203 | 203 | 81 | 273 | 280 |
| 2500 | 257 | 0 | 335 | 253 | 253 | 82 | 323 | 345 |
| 5000 | 507 | 0 | 584 | 503 | 503 | 81 | 573 | 660 |

The headline stop includes driver quiescence before the RTL StopWorld gate, configuration, prefill and commit publication. Hardware gate width is listed separately. Entry writes count actual ready/valid insertions, not an estimate. Fixed overhead is the measured total minus those write cycles.
Strict* has no global pop stop and one designate instruction, but compiling/installing the new tree still costs instructions and a short input-only commit barrier. Zero is not a claim that the entire tree change is instantaneous.

## Death: root change and nonempty wrapper reclamation

Costs start when both the old root is empty and the creation package is ready for another transaction; the drain wait itself is excluded. `finish` includes mapper-bank synchronization, not a packet outage.

| Pre-phase | Root change | Reclaimed | All config finished | Tokens at detach | Tokens discarded | Packets served later |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 23 | 39 | 55 | 22 | 21 | 2120 |
| 1000 | 23 | 39 | 55 | 132 | 131 | 2126 |
| 2000 | 23 | 39 | 55 | 239 | 239 | 2126 |
| 2500 | 23 | 39 | 55 | 292 | 291 | 2126 |
| 5000 | 23 | 39 | 55 | 559 | 559 | 2391 |

UpdateRoot publishes the survivor as the port root. ClearPifoEngine then clears the detached wrapper's occupancy counters in one hardware cycle; its redundant tokens are discarded, not copied, and the survivor's packet queues remain intact. Old memory bits need not be individually erased. This substrate therefore supports nonempty death with constant-time logical reclamation; death is not an O(backlog) token-deletion loop. No additional global stop occurs at teardown.

## Drain and instruction accounting

| Pre-phase | Mechanism | Commit applied | Old root N at publication | Old root empty | Post-commit drain | Install / cleanup / reclaim inst | Categories |
| ---: | --- | ---: | ---: | ---: | ---: | --- | --- |
| 0 | Strict* link | 36 | 0 | 36 | 0 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 0 | Reserved-PE Strict wrapper | 83 | 1 | 92 | 9 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 1000 | Strict* link | 1034 | 100 | 1333 | 299 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 1000 | Reserved-PE Strict wrapper | 1185 | 103 | 1501 | 316 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 2000 | Strict* link | 2034 | 200 | 2633 | 599 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 2000 | Reserved-PE Strict wrapper | 2284 | 203 | 2900 | 616 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 2500 | Strict* link | 2534 | 250 | 3283 | 749 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 2500 | Reserved-PE Strict wrapper | 2835 | 253 | 3601 | 766 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |
| 5000 | Strict* link | 5034 | 500 | 6533 | 1499 | 26 / 16 / 0 | {'rest': 25, 'designate': 1} |
| 5000 | Reserved-PE Strict wrapper | 5584 | 503 | 7100 | 1516 | 39 / 20 / 11 | {'rest': 27, 'wrapper_creation': 11, 'prefill': 1} |

`start` is transaction execution start; `commit` in reconfiguration-events.csv is instruction acceptance, not publication; controller-instructions.csv records actual publication. `drained` means the final old-root token popped (lower-level traversals may still be in flight). A row's `install_finish_cycle` is that commit's own replay-ready time; `finish_cycle` includes its associated cleanup. The last reclamation row marks final readiness. All commits are drawn independently with a shared old-tree drain, not a fictional second/third drain.
Hardware gate register transitions are observed on the following rising-edge sample; both edges have the same offset, so their width is unaffected. The driver stop ends at commit publication, not at completion of mapper-bank synchronization.

| Pre-phase | Run | Commit | Start | Accepted | Published | Ready for next commit | Instructions | Cycles to publish | Bank replay cycles |
| ---: | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | link | C1 | 0 | 33 | 36 | 53 | 26 | 36 | 17 |
| 0 | link | C2 | 53 | 69 | 72 | 79 | 16 | 19 | 7 |
| 0 | reserved | C1 | 0 | 47 | 83 | 112 | 39 | 83 | 29 |
| 0 | reserved | C2 | 112 | 133 | 135 | 146 | 20 | 23 | 11 |
| 0 | reserved | C3 | 146 | 158 | 160 | 167 | 11 | 14 | 7 |
| 1000 | link | C1 | 1000 | 1031 | 1034 | 1051 | 26 | 34 | 17 |
| 1000 | link | C2 | 1051 | 1068 | 1352 | 1359 | 16 | 301 | 7 |
| 1000 | reserved | C1 | 1000 | 1047 | 1185 | 1214 | 39 | 185 | 29 |
| 1000 | reserved | C2 | 1214 | 1235 | 1524 | 1535 | 20 | 310 | 11 |
| 1000 | reserved | C3 | 1535 | 1547 | 1549 | 1556 | 11 | 14 | 7 |
| 2000 | link | C1 | 2000 | 2031 | 2034 | 2051 | 26 | 34 | 17 |
| 2000 | link | C2 | 2051 | 2068 | 2652 | 2659 | 16 | 601 | 7 |
| 2000 | reserved | C1 | 2000 | 2046 | 2284 | 2313 | 39 | 284 | 29 |
| 2000 | reserved | C2 | 2313 | 2334 | 2923 | 2934 | 20 | 610 | 11 |
| 2000 | reserved | C3 | 2934 | 2945 | 2948 | 2955 | 11 | 14 | 7 |
| 2500 | link | C1 | 2500 | 2531 | 2534 | 2551 | 26 | 34 | 17 |
| 2500 | link | C2 | 2551 | 2568 | 3302 | 3309 | 16 | 751 | 7 |
| 2500 | reserved | C1 | 2500 | 2547 | 2835 | 2864 | 39 | 335 | 29 |
| 2500 | reserved | C2 | 2864 | 2885 | 3624 | 3635 | 20 | 760 | 11 |
| 2500 | reserved | C3 | 3635 | 3647 | 3649 | 3656 | 11 | 14 | 7 |
| 5000 | link | C1 | 5000 | 5031 | 5034 | 5051 | 26 | 34 | 17 |
| 5000 | link | C2 | 5051 | 5068 | 6552 | 6559 | 16 | 1501 | 7 |
| 5000 | reserved | C1 | 5000 | 5046 | 5584 | 5613 | 39 | 584 | 29 |
| 5000 | reserved | C2 | 5613 | 5634 | 7123 | 7134 | 20 | 1510 | 11 |
| 5000 | reserved | C3 | 7134 | 7145 | 7148 | 7155 | 11 | 14 | 7 |

In Figure A, zoom's peak delay is 613 versus 910 cycles: +297, compared with the 284-cycle stop. The post-commit old-root drains are 599 and 616 cycles.
Those drains start with 200 and 203 tokens respectively: Strict* kept serving the old policy while its commands arrived. The difference in token counts and wrapper traversal latency explains why the drain widths need not be exactly equal.
At t1=2000, the measured backlog is 207 packets and pre-transition link utilization is 0.891. These values are recomputed for the FIFO-layer topology, not carried forward from the direct-policy-leaf experiment.

| Pre-phase | Mechanism | Last packet pop | All configuration finished |
| ---: | --- | ---: | ---: |
| 0 | Strict* link | 8026 | 79 |
| 0 | Reserved-PE Strict wrapper | 8026 | 167 |
| 1000 | Strict* link | 9026 | 1359 |
| 1000 | Reserved-PE Strict wrapper | 9026 | 1556 |
| 2000 | Strict* link | 10026 | 2659 |
| 2000 | Reserved-PE Strict wrapper | 10026 | 2955 |
| 2500 | Strict* link | 10526 | 3309 |
| 2500 | Reserved-PE Strict wrapper | 10526 | 3656 |
| 5000 | Strict* link | 14018 | 6559 |
| 5000 | Reserved-PE Strict wrapper | 14314 | 7155 |

## Scope and capacity

4096 packet metadata FIFO entries per flow; each flow also has a real hardware FIFO PIFO on PE 3. The 1024 scheduler-token slots on each PE are shared across its virtual PIFOs, including old/new FIFO versions. The source gate queue is unbounded and measured, not claimed as hardware RAM.

The pre-phases remain 0, 1000, 2500 and 5000 cycles, plus the 2000-cycle Figure A run. Rates, sizes, seeds and durations are unchanged from the previous experiment so the FIFO-layer change is isolated. Figure B uses the measured backlog for this topology, not a nominal target or the previous measurements.
The 1000/2000 points were not run: this experiment's 1024-token PE cannot contain 2000 root tokens. Growing that sorted-register RTL and rerunning is required; silently leaving excess packets at the door would not measure a 2000-token prefill.

At the zero-pre-phase point, packets may already be entering as the stop starts. The hardware snapshot/prefill count is recorded separately from backlog immediately before t1.

One permanently reserved PE is needed per concurrent materialized wrapper. The Strict* run leaves that PE unused to keep the physical simulation shape identical. Reservation avoids descent and ascent but does not avoid the N birth writes.

Not run: the existing copy datapath supports frozen drain-only relocation, not a complete live-survivor ascent/brain-state migration protocol.

## Figures and raw files

- [Figure A: zoom delay](figures/zoom-delay/figure.png) ([SVG](figures/zoom-delay/figure.svg), [plotted CSV](figures/zoom-delay/data.csv), [standalone plotter](figures/zoom-delay/plot.py)).
- [Figure B: stop versus backlog](figures/prefill-stop/figure.png) ([SVG](figures/prefill-stop/figure.svg), [plotted CSV](figures/prefill-stop/data.csv), [standalone plotter](figures/prefill-stop/plot.py)).

Each figure folder also contains complete packets.csv and commits.csv. Copy that folder elsewhere and run `python plot.py`; only Matplotlib and the local CSVs are needed. Blue/amber/green backgrounds distinguish install, cleanup/collapse, and wrapper reclamation commits. Delay axes have vertical time markers only.

- [Measurements](measurements.json). Each pre-N directory contains its traffic input; link/ and reserved/ contain direct transactions, compiler accounting, requests.csv, packet-outcomes.csv, request-results.csv, reconfiguration-events.csv, controller-instructions.csv and maintenance-events.csv.

Per-flow packet metadata FIFO order is checked end-to-end; the RTL tokens carry flow IDs, not unique packet IDs. These measurements are not a gate-level timing-closure result.
