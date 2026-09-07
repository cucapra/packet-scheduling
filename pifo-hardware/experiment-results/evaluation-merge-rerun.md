# Evaluation-branch merge and experiment validation

Merge of `pifo-stop-the-world-pop` at `2212bf3` into `pifo-hardware`, based on
`1e84360`, on 2026-09-07. The 16 imported evaluation cases were rerun and
the normal RR/SP image was compared against the known-good commit at both
reduced and full capacity. The final comparison passes. At the user's request
to finish promptly, the other existing normal-image results are retained from
the previous rerun; they are not claimed as newly rerun here.

## Image separation

The default `PifoMesh` / `rio.sim.RequestSimulatorCli` remains the normal image.
The separate `hw/spinal/rio/evaluation/` top levels opt into hardware stop,
counted token prefill, root replacement, frozen-subtree copy/inject, occupancy
probes and the hierarchical evaluation ranker. Those features are absent from
the normal generated RTL. The normal build retains its original priority
encoder; the evaluation implementation is in `hw/verilog/evaluation/`.
The shared command vocabulary is extended, so this is not a claim that the
two interfaces or complete generated netlists are byte-identical.

`sbt 'runMain rio.EvaluationBuildCheck'` passes for both images. It checks
feature absence/presence, shared-FIFO replay in both images, and rejection of
evaluation-only commands by the normal driver. This is elaboration and RTL
inspection, not gate-level synthesis or timing closure.

## Known-good comparison

A detached worktree at `1e84360` provides the reference. The same transaction
and traffic files are used for both revisions, with JDK 17 and Icarus.

The minimized RR-to-SP check changes only `fifoDepth=32` to `fifoDepth=1`
(32 scheduler entries per PE). Both revisions deliver 480 packets in 1913
cycles, without drops. Their complete packet-outcome and completion CSVs are
byte-identical. All 30 existing event fields match for both commits:

| Commit | Start | Accepted | Published | Replay ready | Old root drained |
| --- | ---: | ---: | ---: | ---: | ---: |
| Install | 601 | 619 | 621 | 626 | 724 |
| Cleanup | 626 | 632 | 731 | 734 | same drain |

Packet-outcome SHA-256:
`e2e05f43902f289008f2b38acf59feac49c92d5b5bc39806c9d28a1ae2b761d6`.
Completion CSV SHA-256:
`a1403ab4c9ae8b606d2cd83f67e22896066e4fd160b786751d653c164e0bf718`.

The full-size reference and final merged image both complete: 480 packets in 1800 cycles, with
install/cleanup readiness at 624/923 and old-root drain at 913. Its results
match the original checked-in artifacts. Generated requests, packet outcomes
and completions are byte-identical between the two revisions, and all 30
existing event fields match across both commits. Packet-outcome SHA-256:
`5220c8d2b05cef670f55aa9fe2631df3cbf61b67e9cd8ab5be562f20c1c8ccbf`.
The imported priority-encoder
rewrite caused substantially slower Icarus startup at this size; it is now
evaluation-only. The normal Verilog encoder is byte-identical to `1e84360`;
the evaluation encoder retains its previously measured logic, in a separately
named module. No merge regression was found in the final packet/timing checks.

An attempted Verilator comparison was not a valid reference: the old commit
itself fails on that backend (an early invalid output token), while the
merged normal image stalled behind a guard. These failed attempts are not
counted as successful experiment runs. The original normal-image backend
has been preserved, and temporary diagnostic changes were removed.

## Refreshed evaluation results

| Family | Runs | Generated | Completed | Explicitly unadmitted control packets |
| --- | ---: | ---: | ---: | ---: |
| Designated survivor | 10 | 30288 | 30288 | 0 |
| Multi-edit | 5 | 50025 | 47891 | 2134 |
| RR/SP hardware stop + prefill | 1 | 480 | 480 | 0 |
| Normal RR/SP comparison | 1 | 480 | 480 | 0 |

The transitioning evaluation runs have no drops or within-flow reorderings.
Each flow traverses a hardware FIFO leaf; source generation continues during
stops. Packet delay includes source-side waiting, and stop-buffer peaks are
measured rather than equated with retained old-tree occupancy.

Both images retain the current 256-entry shared replay FIFO. Cleanup uses
`GuardDrain` on every retired FIFO followed by ordinary invalidation writes
and commits. A materialized wrapper has a third reclamation commit after
collapse; it waits for queued root visits and in-flight PE work before clear.
Every commit has independent start, acceptance, publication and replay-ready
timestamps, instruction count and publication-cycle cost. The old-tree drain
is a shared physical event, not another drain per cleanup commit.

The designated-survivor wrapper stops for 83, 185, 284, 335 and 584 cycles:
one cycle per actual prefill token plus 81–82 cycles of overhead. Strict*
has no global pop stop. At the canonical 207-packet backlog, zoom peaks at
613/910 cycles and post-publication drains last 599/616 cycles.

The copy baseline moves 14 occupied virtual PIFOs across three PEs: 975
scheduler tokens representing 325 buffered packets. It supports frozen,
drain-only relocation, not a complete live-survivor ascent/state migration.

## Checks and limits

Python suite: **69 passed, 1 skipped** (the opt-in RTL smoke). All **26** saved
standalone plot scripts run in isolated folders with only their local CSVs.
New figure folders include complete `packets.csv` and `commits.csv` in
addition to plotted `data.csv`; scripts import only csv, pathlib and
Matplotlib. Timeline backgrounds distinguish install, cleanup and, when
present, reclamation. No standalone RTL regression suite was run.
The full-size comparison was the final hardware run; the broader normal-image
rerun was stopped before its next simulation to honor the request to finish.

The RR/SP hardware-stop phase verifier reports two old packet completions
after the root-drain marker: requests 248/249 finish at 1927/1931 after root
drain at 1924. The first new-tree packet completes at 1942, preserving
old-before-new output order. This is the same root-event versus downstream
completion distinction already recorded for the normal large-tree verifier;
the failed check and its thresholds are retained, not relabeled as a pass.

The multi-edit measurements do not support instantaneous first service or
unchanged delays for all four witness flows. The survivor sweep reaches a
507-packet backlog; 1000/2000-backlog targets are not measured with the
1024-token-per-PE substrate. See each family's report for these limitations.

## Outputs and reproduction

- [Designated-survivor results](designated-survivor/README.md),
  [zoom delay](designated-survivor/figures/zoom-delay/figure.png),
  [stop versus backlog](designated-survivor/figures/prefill-stop/figure.png).
- [Multi-edit results](multi-edit/README.md),
  [first service](multi-edit/figures/first-service/figure.png),
  [untouched-flow delay](multi-edit/figures/untouched-delay/figure.png).
- [RR/SP hardware-stop figures](rr-to-sp-stop-the-world-pop/figures/),
  [phase report](rr-to-sp-stop-the-world-pop/phase-verification.md).

From `pifo-hardware/`, with JDK 17 selected in JAVA_HOME and PATH:

```sh
sbt 'runMain rio.EvaluationBuildCheck'
.venv/bin/python hw/python/pifo_survivor_all.py
.venv/bin/python hw/python/pifo_multiedit_all.py
.venv/bin/python hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp-stop-the-world-pop.json
.venv/bin/python hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp.json
.venv/bin/python hw/python/pifo_experiment_figures.py run --config experiments/large-tree-rr-to-sp.json
.venv/bin/python hw/python/pifo_motivation_all.py
.venv/bin/python -m unittest discover -s hw/python/tests -q
```

The two phase-verification commands with recorded failed checks return
nonzero even though their packet simulations complete. Run the commands
sequentially; do not stop the remaining experiments at those known failures.
