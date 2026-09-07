# Shared-FIFO replay experiment rerun

Rerun on 2026-09-07 after fast-forwarding `pifo-hardware` to
`34e35d235735314f87c06f6c7b0c3dce62ec97a6` (shared-FIFO mapper replay,
256 controller entries). All six simulations completed and all 14 current
figures were regenerated. **The large-tree phase verifier fails; this is not
an all-green experiment run.**

The hardware, experiment configurations, seeds, and generated input traces
were unchanged. Used the installed Temurin JDK 17.0.16 via `JAVA_HOME`/`PATH`;
the machine's default Java 8 cannot compile the new `Files.readString` and
`Files.writeString` calls. No additional RTL regression tests were run.

## Measured commit costs

Cycles to publication include controller stalls and, for cleanup, drain-guard
waiting. Instruction counts include guards and commits. Bank-sync columns
are the separately reported publication-to-ready intervals. Finish is the
absolute cycle when the cleanup commit's double-buffer synchronization is
done, not the end of packet simulation.

| Run | Packets | Install inst / cycles | Cleanup inst / cycles | Bank sync install / cleanup (cycles) | Finish cycle |
| --- | ---: | ---: | ---: | ---: | ---: |
| RR to SP | 480 | 9 / 18 | 6 / 296 | 5 / 3 | 923 |
| Large-tree RR to SP | 120 | 27 / 41 | 30 / 274 | 17 / 15 | 587 |
| Motivation R1: add | 2136 | 4 / 15 | 1 / 4 | 2 / 1 | 2022 |
| Motivation R2: stop the world | 2136 | 16 / 34 | 1 / 3 | 10 / 1 | 3041 |
| Motivation R3: whole tree | 2136 | 17 / 30 | 12 / 406 | 11 / 5 | 2452 |
| Motivation R4: confined | 2136 | 10 / 21 | 5 / 683 | 7 / 2 | 2713 |

## Validation

All **9144 packets** were delivered exactly once, with zero drops and no
within-flow reordering. Raw outcomes cover their input traces exactly;
`push_cycle` matches source generation time. Each per-run figure's packet
CSV matches that run's raw outcomes, and every finish timestamp matches its
cleanup finish timestamp.

Python checks: **47 passed, 1 skipped** (`unittest discover -s hw/python/tests -v`;
the skipped test is the opt-in RTL smoke). All 18 saved standalone plot scripts,
including the four historical scripts, ran in isolated folders. Two saved-result
fixture constants were refreshed: 12 per-run figure CSVs and the new RR-to-SP
second-packet completion at cycle 23. No verifier criteria were relaxed.

The [motivation comparison checks](motivating-example/validation.txt) passed.
R3/R4 drain durations remain 404/687 cycles from commit acceptance. Maximum
post-start zoom delays are R1=21, R2=1049, R3=416, and R4=17 cycles. R2 retains
137 packets at capture, peaks at 417 queued packets with traffic offered
throughout, and stops for 1024 cycles (1039-cycle maximum output gap).

The [large-tree phase report](large-tree-rr-to-sp/phase-verification.md)
records three failed checks, with all thresholds left unchanged:

- Old backlog: 28 packets, below the required 32.
- Drain duration: 257 cycles, below the required 800.
- Two old packets complete after the recorded root-drain marker at cycle 535:
  request 53 at cycle 540 and request 54 at cycle 550. The first new-tree
  completion is cycle 560, so old-before-new output ordering is preserved.
  The simulator records the root's hardware drain event; the verifier expects
  all downstream completions to precede that event. These are different points
  in the pipelined tree. The marker/verifier discrepancy has not been changed
  in this rerun.

## Outputs and reproduction

The current figures have been replotted from these same CSVs with two commit timelines:
pale blue is C1 (install), pale amber is C2 (cleanup), each shaded from start to
`ready_for_next_commit`. Both list start, commit acceptance, readiness, and the
shared old-tree drain/capture event. The former single finish line is now C2
readiness; C1 readiness explicitly marks the earlier install replay completion.
This presentation update changes no packet data, timing measurements, or hardware.

Current figures, raw per-packet CSVs, and self-contained `plot.py` scripts:

- [RR to SP](rr-to-sp/figures/)
- [Large-tree RR to SP](large-tree-rr-to-sp/figures/)
- [R1](motivating-example/r1-add/figures/), [R2](motivating-example/r2-stop-the-world/figures/),
  [R3](motivating-example/r3-whole-tree/figures/), [R4](motivating-example/r4-confined/figures/)
- [R2-R4 delay comparison](motivating-example/comparisons/r2-r4-delay-scatter/)
  and [R3-R4 throughput comparison](motivating-example/comparisons/r3-r4-throughput/)

Run sequentially from `pifo-hardware/` with JDK 17 selected:

```sh
.venv/bin/python hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp.json
.venv/bin/python hw/python/pifo_experiment_figures.py run --config experiments/large-tree-rr-to-sp.json
.venv/bin/python hw/python/pifo_motivation_all.py
```

The second command returns nonzero for the recorded phase-check failures;
the motivation command was still run. Older root-level RR/scatter artifacts
remain historical snapshots; the refreshed outputs are under `figures/`.
