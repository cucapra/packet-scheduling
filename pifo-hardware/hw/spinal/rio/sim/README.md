# Request-level PIFO mesh simulator

This directory contains the request simulator and focused hardware simulations.
Run all commands below from `pifo-hardware/`. Hardware test commands are in the
[hardware README](../../README.md#core-hardware-tests).

The request simulator keeps full request metadata in simulator-side queues while the RTL schedules its existing compact
flow tokens. Each request has four input fields:

```csv
cycle,request_id,global_flow_id,size_bytes
0,1,1,64
10,2,2,1500
```

`cycle` is relative to workload start. Request IDs must be unique, cycles must be nondecreasing, sizes must be positive,
and the global flow ID must fit the configured vPIFO width. The highest possible flow ID is reserved for the mesh's
empty-PIFO response.

For every admitted request, the harness inserts one token into each engine, matching the existing Scala simulator. The
request is then stored in a bounded FIFO selected by `global_flow_id`. Root pops are issued at the hardware's
three-cycle accepted initiation interval. The dequeue driver checks readiness on falling edges, after the rising-edge
cycle counter update. Terminal scheduler tokens enter the configured prefetch window; the request
model independently serializes packet bytes at `ceil(size_bytes / link_bytes_per_cycle)` cycles. This reflects the
real split between compact scheduling tokens and the external packet-data link.

## Generate and run a trace

Generate round-robin, uniform-random, Poisson, or burst traffic:

```bash
python3 hw/python/request_generator.py pattern \
  --pattern round-robin --flows 1,2 --count 20 \
  --interval-cycles 4 --size-bytes 256 \
  --output /tmp/requests.csv
```

Run a complete simulation. When no control file is supplied, the CLI configures a flat FIFO at engine 1 / vPIFO 10:

```bash
sbt 'runMain rio.sim.RequestSimulatorCli \
  --trace /tmp/requests.csv \
  --output /tmp/request-results.csv \
  --link-bytes-per-cycle 64 \
  --no-control-socket --no-wave'
```

The result CSV contains arrival, admission, completion, admission-delay, and total-sojourn cycles for each request.

The default mapper implementation replays updates from the shared controller
FIFO after commit. `--control-queue-depth` defaults to **256** in both the Scala
CLI and `pifo_simulator.py`; one slot is reserved for commit. Size it above the
largest retained command span in a package, counting every command from its
first pre/post-mapper update through the command before commit. Flat FIFO
initialization needs two mapper updates per configured flow. Oversized packages
fail with a capacity error before they can block their own commit. See the
[transaction protocol](../../README.md#transactional-configuration).

## Run compiled transactions

Run an already-compiled workload without involving tree logic:

```bash
python3 hw/python/pifo_simulator.py \
  --transactions experiment-results/rr-to-sp/transactions.txt \
  --traffic experiment-results/rr-to-sp/traffic.json \
  --output-dir /tmp/pifo-run \
  --queue-depth 256 --link-bytes-per-cycle 64 --max-cycles 100000
```

The [Python compiler guide](../../../python/README.md) defines `tree-move.json`, all
five transition modes, and `traffic.json`. Compile a tree move there first, or
supply a hand-authored direct timeline. Complete experiment commands and plotting
recipes live in the [experiment guide](../../../../experiments/README.md).

The Python simulator converts only the traffic patterns to canonical request CSV. It passes that CSV and the unchanged
direct timeline to Scala with `--trace` and `--transactions`; the old bundle of single-transaction flags is gone. Run
`sbt 'runMain rio.sim.RequestSimulatorCli --help'` for the low-level syntax. The live control socket remains available
at `/tmp/rio-control.sock` unless disabled.

`pifo_simulator.py` also fixes the RTL initialization seed: by default it uses the traffic seed modulo 2³¹ via
SpinalHDL's `SPINAL_SIM_SEED`. Set that environment variable or pass `--simulation-seed` to override it; the CLI
option takes precedence. The selected seed is printed before simulation. Direct Scala invocations still use
SpinalHDL's default random seed unless the environment variable is set.

The transaction timeline is deliberately not JSON: it is a compact, line-oriented stream that both humans and the
Scala simulator consume directly. Its first line defines hardware shape and the root. Every later line is one control
instruction tagged with `at=init` or an integer cycle and a transaction name:

```text
schema=pifo-transactions-v1 rootEngine=1 rootVPifoId=10 numEngines=2 numVPifos=32 maxPacketPriority=65536 fifoDepth=32 prefetchBufferDepth=2
at=init name=initial-tree mode=direct command=UpdateBrainEngine engineId=1 vPifoId=10 flowId=0 data=1
at=init name=initial-tree mode=direct command=CommitMapper engineId=1 vPifoId=0 flowId=0 data=0
at=600 name=policy-change mode=full_transitive before=RR after=SP drainRoot=1:10 command=UpdateBrainEngine engineId=1 vPifoId=11 flowId=0 data=2
at=600 name=policy-change mode=full_transitive before=RR after=SP drainRoot=1:10 command=CommitMapper engineId=1 vPifoId=0 flowId=0 data=0
```

Lines with the same `at` and `name` form one contiguous package, which must end with exactly one `CommitMapper`.
Multiple timed packages are supported and must be ordered by cycle. `mode`, labels, `drainRoot`, optional
`gateFlows=1,2`, and optional `minStopCycles=1024` describe evaluator behavior around the otherwise direct command
package. `gateFlows` holds newly admitted flows until the commit is applied, so they cannot enter an unconfigured path
while commands are staging. `minStopCycles` is valid only for `stop_the_world` and sets the minimum interval from
capturing the old tree until traffic resumes.

## Direct transaction semantics

To bypass compilation, author or edit `pifo-transactions-v1` directly and pass it to `pifo_simulator.py`. Nothing in a
direct package is expanded, rewritten, or interpreted as a policy. There is one mesh-wide ready/valid configuration
ingress, so commands for different engines are still serialized and at most one line is accepted per clock. A package
containing `N` command lines therefore has `N` instruction acceptances and can take longer under backpressure.

`UpdateMapperPre` uses `vPifoId` as the raw input flow and `data` as the destination vPIFO.
`UpdateMapperPost` uses `(vPifoId, flowId)` as its key and `data` as the packed next-hop `(engine, vPifo)`.
`UpdateMapperNonExist` directly writes the selected engine's single-bank front table with source `vPifoId`, target
vPIFO `data`, and runtime enable false. Source and target must reside on the same engine. `CommitMapper` arms newly
written front entries but does not bank or copy them; the successful pop of the source's final entry sets runtime enable. Brain
commands use `engineId`/`vPifoId` as their target and `flowId` where required. Brain writes are immediate, so a direct
package owns their ordering and does not gain brain atomicity from `CommitMapper`.

`GuardDrain` is a queue barrier keyed by `engineId:vPifoId` (`flowId=0 data=0`). It consumes the same last-successful-pop
notification that enables the PE's front rewrite. Hardware remembers the notification, invalidates it on a later
accepted push to that FIFO, and recognizes FIFOs empty since reset. A settling cycle protects the final post-mapper
lookup. It does not rewrite mappings, perform cleanup, commit, stop traffic, or poll software. All commands behind it
wait; unrelated packet traffic keeps running. Before using it for reclamation, redirect/quiesce the old inputs and
let their in-flight inserts settle. Guard every retired FIFO, not just a root whose last token can still be descending.

The tree compiler does this by default: its first commit redirects old inputs to the new tree (or invalidates inputs
no longer used), then a second package guards every retired FIFO and uses ordinary zero/invalid writes followed by
`CommitMapper`. vPIFO 0 is the enqueue mapper's invalid/NOP sink. Old post-mapper entries and brain configuration/state
are cleared. Do not clear a pre-mapper slot now pointing at the new tree. The live front-rewrite alias is retained
because root/parent pop requests still use it; shared ancestors in a confined move are also retained. An additive or
reset move has no retired entries and gets a commit-only cleanup package. Initialization is not a transition.

For a raw transaction file, use the same format; `cleanupOf` is reporting metadata, not a hardware opcode:

```text
at=600 name=move-cleanup mode=direct cleanupOf=move command=GuardDrain engineId=1 vPifoId=10 flowId=0 data=0
at=600 name=move-cleanup mode=direct cleanupOf=move command=UpdateMapperPost engineId=1 vPifoId=10 flowId=33 data=0
at=600 name=move-cleanup mode=direct cleanupOf=move command=UpdateBrainEngine engineId=1 vPifoId=10 flowId=0 data=0
at=600 name=move-cleanup mode=direct cleanupOf=move command=CommitMapper engineId=1 vPifoId=0 flowId=0 data=0
```

This is a schematic one-FIFO cleanup, after a preceding `name=move` transaction in a file with the usual hardware
header; real trees require one guard per retired FIFO and invalidation of every retired populated slot. Packages
remain FIFO ordered, so both may use the same `at` cycle. The cleanup does not gate packet admission while its guard
waits. Raw direct packages are never expanded automatically because the simulator cannot infer which entries are dead.

## Evaluation-only hardware commands

These require the separate `rio.sim.EvaluationRequestSimulatorCli` top level, selected by
`pifo_simulator.py --evaluation-hardware`. They are not present as hardware features in the production image.
Normal-image experiments keep the known-good Icarus backend; the evaluation runners explicitly use Verilator.

`StopWorld` uses `engineId`/`vPifoId` to identify the old root and gates hardware ready signals until commit.
`PrefillPifo` uses `engineId`/`vPifoId` as its destination and `flowId` as the synthetic token. With `data=0`, hardware
uses the stopped root occupancy; a non-zero `data` is an explicit low-level count. Prefill always uses priority 1 and
commit backpressures until its autonomous one-token-per-cycle fill completes. `UpdateRoot` stages its
`engineId`/`vPifoId` for commit publication.

`CopyPifoEngine engineId=source data=target` moves a frozen PE's entries, preserving token IDs, virtual PIFO IDs,
ranks and equal-rank order. Hardware gates traffic, snapshots occupancy, then uses a separate indexed read/inject
datapath; it clears the source only when copying is complete. The destination must be empty. This does not migrate
live brain state. `ClearPifoEngine` logically invalidates all entries on an unreachable PE; it waits for pending
root visits and PE work before clearing counters. Normal mapper/brain invalidation commands still perform config
cleanup. `WaitPifoEmpty` is accepted for old evaluation command files; new compilers use `GuardDrain`.

The materialized-wrapper lifecycle is install → guarded old-tree cleanup/root collapse → detached-wrapper
reclamation. `cleanupOf` may chain those packages; each package still ends with one ordinary `CommitMapper`.
`install_finish_cycle` always gives the row's own ready-for-next-commit time, while `finish_cycle` includes its
linked cleanup. The last reclamation row gives final configuration readiness. Plots show every commit separately.
`controller-instructions.csv` distinguishes accepted, dispatched, replayed and published commands;
`maintenance-events.csv` includes counted prefill writes, copy-source occupancies and discarded wrapper tokens.

An enabled front entry substitutes the target vPIFO before the engine performs its PIFO lookup. On the activation
cycle, the engine backpressures its input once so the waiting request observes the registered enable on the next cycle.
There is no retry: the last source pop remains valid, and the next request directly pops the target. The transition II
is two cycles and steady-state traffic retains one accepted pop per engine cycle without a mesh loopback. An
unconfigured underflow produces no valid mesh message.

## Reconfiguration timestamps and drain time

Every figure CLI writes a standalone `plot.py` next to its plot CSV. Run it from any working directory:

```bash
python /path/to/figure-folder/plot.py
```

Only Matplotlib needs installing; the script does not import repository helpers, read a shared style/config file,
or access parent/sibling result folders. All flow labels, timestamp lines, commit-cost annotations, colors and DPI
are literal settings inside it. It recreates the local SVG and PNG. Throughput reads the already-smoothed CSV values
directly; packet scatter retains equal x/y scales, and delay-comparison panels share axes.

Every figure folder includes `packets.csv`, even when `data.csv` contains averaged throughput or a summary manifest.
It contains one row per generated packet, including drops, with these columns:

```csv
request_id,flow,flow_name,size_bytes,push_cycle,pop_cycle,delay_cycles,dropped
```

`flow` is the numeric ID; `flow_name` preserves the configured flow label. `push_cycle` is the source-generation
cycle, not hardware admission, and `delay_cycles = pop_cycle - push_cycle` includes external queueing during a stop.
Drops use `dropped=true` with blank pop/delay fields; delivered packets use `false`. Comparison traces prepend `run`,
so `(run,request_id)` identifies a packet. Raw `packet-outcomes.csv` at the run root retains the simulator schema
`request_id,flow,size_bytes,push_cycle,pop_cycle,dropped` (backfilled legacy runs also have the name/delay columns).

The general figure CLIs accept `--outcomes`; by default they use `packet-outcomes.csv` beside `--results` and check
that completed packets match. If that trace is absent, conversion is allowed only with a matching `requests.csv`
that proves all generated requests completed. Missing completions are never silently inferred as drops.

Fresh experiments export these by default. `pifo_export_plot_scripts.py --results-root experiment-results` also adds
scripts and packet CSVs to saved results without touching existing figures or measurements. Legacy root-level plots
use `rr-to-sp-packets.csv`, kept separate from newer run outcomes. That file records archived completions only;
unavailable historical sizes stay blank, and missing historical drop records cannot be recovered without a rerun.

The event CSV has a `reconfiguration` row and an associated `cleanup_commit` row. The first row includes the cleanup's
timestamps and counts for single-event figure readers. Each package still ends in exactly one ordinary `CommitMapper`.

- `start_cycle`: package feeding starts.
- `instruction_count`: controller instructions in the install package, including `CommitMapper`.
- `commit_cycle`: the `CommitMapper` ready/valid transfer is accepted by the controller queue.
- `commit_applied_cycle`: hardware publishes that commit (observed using `commitEpoch`, not a previous bank-copy busy
  interval). Acceptance is not publication: a commit can be queued behind a guard.
- `commit_cycles`: `commit_applied_cycle - start_cycle`, including command feeding, backpressure and any guard wait.
- `bank_cleanup_cycles`: `install_finish_cycle - commit_applied_cycle`; double-buffer synchronization, not drain time.
- `drain_cycle`: the cycle on which the old boundary's final successful PIFO pop raises `portDrained`; for
  `stop_the_world`, this is the cycle on which the quiesced old tokens are captured before reset.
- `install_finish_cycle`: the install commit's active-to-backup synchronization has completed (`commitReady` high).
- `finish_cycle`: the **cleanup commit's double-buffer cleanup has completed**, ready for the next commit. Without an
  associated cleanup package, it is that direct package's own double-buffer cleanup completion. It never means STW resume.
- `cleanup_start_cycle`, `cleanup_commit_cycle`, `cleanup_applied_cycle`, `cleanup_finish_cycle` describe the ordinary
  cleanup package, with the same start/queue acceptance/publication/bank-ready meanings. For compiled transitions,
  `finish_cycle = cleanup_finish_cycle`.
- `cleanup_instruction_count` includes its `GuardDrain`, invalidation/reset writes and final `CommitMapper`.
  `cleanup_commit_cycles = cleanup_applied_cycle - cleanup_start_cycle` includes waiting behind guards;
  `cleanup_bank_cleanup_cycles = cleanup_finish_cycle - cleanup_applied_cycle` reports the final bank replay separately.
- `resume_cycle`: STW-only, when retained tokens have been replayed, the minimum stop has elapsed, and traffic resumes.
- `drain_duration_cycles`: `drain_cycle - commit_cycle` for transitive drain modes; blank for stop-the-world because
  capture precedes commit.
- `retained_packets` is the old-tree queue snapshot captured immediately before reset. It excludes packets waiting at
  the admission gate and is not a high-water mark.
- `peak_buffer_occupancy_packets` is the largest number of generated-but-not-completed packets during the outage,
  including admitted queues, the closed admission gate, and link-prefetched packets. It is the aggregate buffer size
  needed for this trace to remain lossless.
- `minimum_stop_cycles` and `stop_duration_cycles` describe the configured and observed stop; the latter is
  `resume_cycle - drain_cycle`.

`install_finish_cycle` and `drain_cycle` are independent: the first bank replay may finish while the old tree is still
draining. Guarded retirement and its commit follow; the final bank replay finishes at `finish_cycle`. For STW, capture
precedes the install commit; resume and final configuration cleanup are separate milestones. The motivating
example regenerates both milestones from its explicit FIFO-leaf topology and cleanup commits. Older RR/SP archives predate this schema;
readers preserve their legacy interpretation rather than retroactively claiming they executed cleanup commits.

Figures display both commits instead of one ambiguous finish line:

| Marker | C1: install | C2: cleanup |
| --- | --- | --- |
| start | `start_cycle` | `cleanup_start_cycle` |
| commit accepted | `commit_cycle` | `cleanup_commit_cycle` |
| ready_for_next_commit | `install_finish_cycle` | `cleanup_finish_cycle` |
| old-tree-drained | `drain_cycle` | the same `drain_cycle` |

The pale-blue C1 and pale-amber C2 backgrounds each span start to readiness. This is not a start-to-drain interval:
C1 is normally ready before drain, and C2's background includes its guard wait. Both refer to the same retired tree,
so their drain lines coincide. Additive changes label the drain as not required; STW shows old-tree capture, not a
fictional drain, and separately marks traffic resume. Exact absolute cycles appear in the legends, including
coincident C1 readiness/C2 start. Packet input/output scatter has matching x/y markers and equal scales; packet-delay
scatter has only vertical time markers because its y-axis is a duration. Historical single-commit archives retain
only C1, with no inferred C2 timestamps from neighboring newer runs.

Outside stop-the-world, packet admission is paused only across the commit edge so one packet cannot be split between
tree versions; existing PIFO traffic continues during staging, drain, and mapper synchronization. Stop-the-world gates
admission and dequeue from start until resume while source arrivals continue accumulating. Figure captions show cycles
and instruction counts for both commits, and the one-accepted-instruction-per-cycle limit. Direct packages record start, commit, and finish;
drain fields are blank because their semantics are intentionally opaque.

## Live request feeder

Start the simulator in live mode:

```bash
sbt 'runMain rio.sim.RequestSimulatorCli \
  --live --flat-fifo-flows 1,2 --no-wave'
```

Then feed a trace from another terminal:

```bash
python3 hw/python/request_feeder.py /tmp/requests.csv
```

The feeder waits for `/tmp/rio-request.sock`, anchors trace cycle 0 at connection time, sends all requests, and closes
the workload with `command=end`. `--try-run` prints the wire protocol without connecting; `--no-end` permits multiple
feeders in one live run.

## Convert formal traces

The OCaml formal simulator writes packet CSV with `flow`, `arrived`, and `length` fields. Convert it while reusing the
flow IDs allocated by the hardware configuration tool:

```bash
python3 hw/python/request_generator.py formal-csv graphs/formal-result.csv \
  --flow-map /tmp/pifo-node-mapping.json \
  --cycles-per-second 1000 \
  --output /tmp/formal-requests.csv
```

Classic Ethernet PCAP inputs used by the formal simulator are supported directly as well:

```bash
python3 hw/python/request_generator.py pcap ../pcaps/two_then_three.pcap \
  --flow-map /tmp/pifo-node-mapping.json \
  --cycles-per-second 1000 \
  --output /tmp/pcap-requests.csv
```

The converter recognizes the formal simulator's source-MAC convention (`10:10:...` is flow A, `20:20:...` is B, and
so on). `--flow-map` is optional; omit it to use A=1, B=2, and so on. Unknown flow names or MAC addresses receive
deterministic non-conflicting IDs; use `--write-flow-map FILE` to record those assignments.
