# One request, several edits, fourteen flows

Run from `pifo-hardware`:

```sh
.venv/bin/python hw/python/pifo_multiedit_all.py
.venv/bin/python hw/python/pifo_multiedit_all.py --render-only
```

`request.json` contains the before/after tenant trees, stable physical slots,
hardware shape, request cycle and reset cost. `traffic.json` independently
describes all traffic, including traffic generated during outages. `settings.json`
holds measurement/figure settings. Every leaf is a real FIFO PIFO; packet metadata
is retained in a FIFO per flow in the simulator. Fixed 48-byte packets on a
16-byte/cycle link give one packet per three cycles at line rate. The rate tables
offer 1.40 before cycle 2000 and 0.85 afterwards, through cycle 34000. Figure B
shows the first 18000 cycles to keep the transition readable; all packets and the
longer steady-state recovery tail remain in its data CSV and measurements.

The compiler and simulator remain separate CLIs:

```sh
.venv/bin/python hw/python/pifo_multiedit_compiler.py experiments/multi-edit/request.json \
  --mechanism relocate --output experiment-results/multi-edit/relocate/transactions.txt
.venv/bin/python hw/python/pifo_simulator.py \
  --transactions experiment-results/multi-edit/relocate/transactions.txt \
  --traffic experiments/multi-edit/traffic.json \
  --output-dir experiment-results/multi-edit/relocate \
  --queue-depth 4096 --link-bytes-per-cycle 16 --max-cycles 50000 --verilator
```

## Mechanisms

| Run | Implementation |
| --- | --- |
| control | p1 throughout; arriving tenants have no arm and are recorded unserved, not dropped. |
| rio | Reweight realtime/bulk, configure media/gaming, quiesce legacy, commit. Only removal is behind `WaitPifoEmpty`. |
| prefill | Hardware stop; old tree remains on PEs 1/2/3, p2 on 4/5/6, real SP root on PE 7. Hardware prefill creates N high-priority old-side tokens. |
| relocate | Hardware stop; whole-PE moves 3→6, 2→5, 1→4. SP root reuses PE 1; p2 uses 2/3/7. Same hardware N-token prefill. |
| reset | Lossless reset/reinstall/replay baseline. Retained legacy packets get a temporary draining arm; after it empties the arm is removed. |

The two whole-tree implementations are additional **stop-and-prefill mechanisms**,
not the front-rewrite whole-tree implementation on the other branch. This distinction
is intentional: the requested evaluation compares placing a new SP above a tree
against moving the tree away to reuse its original root. Rio does not issue StopWorld.

`CopyPifoEngine engineId=source data=target` moves all occupied entries of one PE,
preserving vPIFO IDs, token IDs, ranks and equal-rank order. A separate controller
reads a dedicated indexed source port and appends through a dedicated destination
port, one entry/cycle, then clears the source. Ordinary pop and push-sort paths are
not used. Destinations must be empty. This is a **frozen drain-only tree move**;
it does not copy live rank-generator or mapper state. The compiler separately
installs relocated post mappings keyed by the original packed token IDs.

StopWorld is a hardware gate on all insertion ports and the root request port.
Before maintenance, hardware waits 32 cycles for insertion pipelines and waits for
all accepted root requests to finish. It snapshots the old root occupancy before
any source is cleared. Commit waits for copy/prefill completion and atomically
publishes the new root and mappings. Traffic sources keep generating throughout;
the input queue is lossless and is included in delay/buffer measurements.

`UpdateRankGroup` groups multiple flows into one tenant arm. `UpdateRankQuantum`
stages `120/weight`; both publish on commit. HWFQ computes
`max(virtual_time, previous_group_finish) + quantum`, without resetting finish on
reweight. Queued ranks are immutable. The older equal-weight RR brain is unchanged.
This experiment uses equal-size packets; this is not byte-weighted WFQ for varying
packet sizes.

## Outputs and interpretation

Every run has the identical `requests.csv`, compiled `transactions.txt`, a planner
report, completion times (including admission times), and `packet-outcomes.csv`:

```text
request_id,flow,size_bytes,push_cycle,pop_cycle,dropped
```

`push_cycle` means **generation**, including time waiting outside the hardware.
In control, the four unadmitted flows have a blank pop time and `dropped=false`.
The verifier demands complete, drop-free, per-flow FIFO output for all other runs.

`reconfiguration-events.csv` distinguishes request/start, commit accepted, physical
old-subtree/root drained, and command completion. `finish` includes backup-bank
synchronization; it is not synonymous with drain or first new service. Cleanup is
a second transaction; a third reclaims the detached SP's redundant tokens with
`ClearPifoEngine` after outstanding traversals have finished. All event rows are
retained. The SP root may switch out
after the old root empties while a few old packets remain in downstream pipelines.
Raw controller observations distinguish acceptance, hardware dispatch, commit
application and copy completion. `transactions.plan.json` reports eligible versus
guarded instruction counts; eligible does **not** mean all execute in one cycle.

`measurements.json` reports the measured per-flow t1 backlogs, first services,
paired delay differences from control, and stop-buffer occupancy. `retained_packets`
counts metadata already admitted when the stop quiesces; it is not peak occupancy.
The peak includes those packets plus packets generated at the input during the
stop. There is no claim that a finite buffer smaller than this peak is lossless.

Figure A (`pifo_multiedit_first_service.py`) has Rio/prefill/reset grouped bars.
Figure B (`pifo_multiedit_untouched_delay.py`) has shared-axis panels for Gmail,
Ssh, Http and Https, with the p1 control overlaid. Both have a separate
`--copy-comparison` variant comparing prefill and relocation. Each writes its own
PNG, SVG and source data under `experiment-results/multi-edit/figures/`.

Important limits on the proposed interpretation:

- Four flows have unchanged paths; fourteen is the union of ten old and four new
  flows. The request does not leave ten of fourteen flows unchanged.
- A serialized control port and multi-PE traversal cannot make all new tenants
  receive their first packet one or two cycles after the operator request.
- Unchanged paths do not imply identical schedules when their siblings change
  weights or leave. The report quantifies differences rather than asserting zero.
- Realtime offers only 0.20 after t1, below its nominal new 6/17 share. Once its
  backlog clears, throughput measures offered load, not its saturated allocation.
  The settling metric is explicitly the observed 0.20 steady rate.
- The reset has separately declared 512-cycle teardown and 513-cycle install
  budgets. Their sum is a minimum stop interval; actual instruction/bank-sync/replay
  activity may exceed it (the generous baseline overlaps this budget). These are
  model parameters, not measured hardware teardown latencies. One cycle is labeled
  1 ns for the microsecond interpretation, not a demonstrated timing closure.
