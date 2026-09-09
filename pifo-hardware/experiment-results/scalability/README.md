# Tree-size scalability results

40 of 40 hardware runs have measured packet outcomes. Compiled plans alone are not counted as runs.

See [experiment specification](../../experiments/scalability/README.md) for fixed hardware, traffic, baseline budgets and metric definitions, and [run provenance](run-notes.md) for this batch's source and validation record.

## Comparison with baselines

Endpoints below are m=2 → m=32 (4 → 64 existing flows). Every flow has a distinct hardware FIFO leaf. Instruction counts exclude initial-policy setup; completion includes post-install cleanup. Added delay is the maximum paired difference for Tenant_02_A against the same point's steady-p2 control.

| Request | Run | Main instructions | Guarded/cleanup instructions | Completion cycles | Main replay cycles | Peak added delay cycles |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| add | rio | 21 → 21 | 0 → 0 | 44 → 45 | 12 → 12 | 0 → 24 |
| add | prefill | 99 → 1099 | 57 → 687 | 524 → 2959 | 63 → 723 | 221 → 1242 |
| add | reset | 64 → 704 | 1 → 1 | 1046 → 1206 | 36 → 396 | 1037 → 1221 |
| add | control-p2 | 0 → 0 | 0 → 0 | 0 → 0 | 0 → 0 | 0 → 0 |
| reweight | rio | 2 → 2 | 0 → 0 | 14 → 14 | 1 → 1 | 3 → 3 |
| reweight | prefill | 73 → 1073 | 51 → 681 | 488 → 2907 | 47 → 707 | 432 → 1198 |
| reweight | reset | 44 → 684 | 1 → 1 | 1046 → 1173 | 25 → 385 | 1986 → 1177 |
| reweight | control-p2 | 0 → 0 | 0 → 0 | 0 → 0 | 0 → 0 | 0 → 0 |

Read the full five-point data below: endpoint arrows do not imply monotonic intermediate values. In particular, reset retains the original 512+513-cycle model budget. Its stop can remain near that floor until actual rebuilding dominates; a linear instruction curve does not imply a linear stop curve. Paired delay also includes scheduling-state differences from the already-p2 control and need not increase monotonically.

[R-add figure](figures/add/figure.svg), [R-reweight figure](figures/reweight/figure.svg), [separate bank-replay figure](figures/bank-replay/figure.svg). Each figure folder includes standalone plot code and CSVs.

All 146,712 generated packets completed, with zero drops and zero per-flow reorderings.

## All measured points

| Request | m | Run | Main / cleanup instructions | To publication | Main bank replay | To completion | Global stop | Peak stop buffer | Peak added witness delay |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| add | 2 | rio | 21 / 0 | 32 | 12 | 44 | 0 | 0 | 0 |
| add | 2 | prefill | 99 / 57 | 217 | 63 | 524 | 217 | 150 | 221 |
| add | 2 | reset | 64 / 1 | 86 | 36 | 1046 | 1041 | 452 | 1037 |
| add | 2 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| add | 4 | rio | 21 / 0 | 32 | 12 | 44 | 0 | 0 | -3 |
| add | 4 | prefill | 165 / 99 | 283 | 107 | 649 | 283 | 174 | 284 |
| add | 4 | reset | 106 / 1 | 128 | 60 | 1046 | 1041 | 453 | 1046 |
| add | 4 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| add | 8 | rio | 21 / 0 | 32 | 12 | 44 | 0 | 0 | -3 |
| add | 8 | prefill | 299 / 183 | 416 | 195 | 905 | 416 | 223 | 417 |
| add | 8 | reset | 192 / 1 | 213 | 108 | 1047 | 1042 | 453 | 1026 |
| add | 8 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| add | 16 | rio | 21 / 0 | 33 | 12 | 45 | 0 | 0 | 15 |
| add | 16 | prefill | 565 / 351 | 683 | 371 | 1577 | 683 | 321 | 699 |
| add | 16 | reset | 362 / 1 | 384 | 204 | 1047 | 1042 | 453 | 1047 |
| add | 16 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| add | 32 | rio | 21 / 0 | 33 | 12 | 45 | 0 | 0 | 24 |
| add | 32 | prefill | 1099 / 687 | 1217 | 723 | 2959 | 1217 | 517 | 1242 |
| add | 32 | reset | 704 / 1 | 726 | 396 | 1206 | 1201 | 511 | 1221 |
| add | 32 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| reweight | 2 | rio | 2 / 0 | 13 | 1 | 14 | 0 | 0 | 3 |
| reweight | 2 | prefill | 73 / 51 | 191 | 47 | 488 | 191 | 141 | 432 |
| reweight | 2 | reset | 44 / 1 | 66 | 25 | 1046 | 1042 | 453 | 1986 |
| reweight | 2 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| reweight | 4 | rio | 2 / 0 | 13 | 1 | 14 | 0 | 0 | 0 |
| reweight | 4 | prefill | 139 / 93 | 257 | 91 | 613 | 257 | 165 | 357 |
| reweight | 4 | reset | 86 / 1 | 108 | 49 | 1046 | 1042 | 453 | 1413 |
| reweight | 4 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| reweight | 8 | rio | 2 / 0 | 13 | 1 | 14 | 0 | 0 | 6 |
| reweight | 8 | prefill | 273 / 177 | 390 | 179 | 869 | 390 | 214 | 460 |
| reweight | 8 | reset | 172 / 1 | 193 | 97 | 1046 | 1041 | 452 | 1188 |
| reweight | 8 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| reweight | 16 | rio | 2 / 0 | 13 | 1 | 14 | 0 | 0 | 9 |
| reweight | 16 | prefill | 539 / 345 | 657 | 355 | 1525 | 657 | 312 | 715 |
| reweight | 16 | reset | 342 / 1 | 364 | 193 | 1047 | 1043 | 453 | 1108 |
| reweight | 16 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| reweight | 32 | rio | 2 / 0 | 13 | 1 | 14 | 0 | 0 | 3 |
| reweight | 32 | prefill | 1073 / 681 | 1191 | 707 | 2907 | 1191 | 507 | 1198 |
| reweight | 32 | reset | 684 / 1 | 706 | 385 | 1173 | 1169 | 499 | 1177 |
| reweight | 32 | control-p2 | 0 / 0 | 0 | 0 | 0 | 0 | 0 | 0 |

All listed runs pass complete packet coverage, no-drop and per-flow FIFO checks. Timing values are simulated cycles, not wall-clock build times. The untouched witness is Tenant_02_A; added delay is reported only when the same point's target-policy control is available. Negative peaks mean every compared packet completed earlier than in the control.

[Measurements CSV](measurements.csv) includes measured offered load, utilization, backlog, source-side waiting, mapper-write counts and trace hashes. [Commit CSV](commits.csv) separates every publication and bank replay. Each request/m-N/run directory retains raw packet, instruction and event CSVs, plus complete named-flow delay pairs.

## Requested R-reweight pilot checkpoint

The m=2 and m=16 instruction/timing rows are identical.
ChangeMeta issues no mapper writes: the recorded bank-ready interval is fixed readiness bookkeeping, not a scan of the flow table. This pilot does not measure R-add's mapper-write replay or either baseline.
