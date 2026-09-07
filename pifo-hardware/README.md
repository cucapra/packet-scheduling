# Runtime reconfigurable pifo hardware

```bash
sbt "runMain rio.sim.PifoMeshSim"
```

For request queues, trace-driven or live workloads, traffic generation, and formal-trace conversion, see the
[request-level simulator guide](REQUEST_SIMULATOR.md).

This contains a PifoMesh Implementation. Current implementation assumes the insert input to be a `PacketToken = engineId ## flowId`. The `PacketToken` will be processed in PifoEngine in the following process:

### Enqueue Path

```
    ┌─────────────────────┐
    │  PacketToken        │-───────────────────┐
    │  engineId ## flowId │                    │
    └──────────┬──────────┘                    │
               │                               │
               ▼                               │
    ┌─────────────────────────────┐            │
    │  Transactional EnqueueMapper│-───────────┤
    │  flowId → vPifoId           │            │
    └──────────┬──────────────────┘            │
               │ vPifoId                       │
               ▼                               │
    ┌─────────────────────────────┐            │
    │  Brain                      │            │
    │  {engineId, vPifo, Flow}    │            │
    │  → Rank                     │            │
    └──────────┬──────────────────┘            │
               │ Rank                          │
               ▼                               ▼
    ┌──────────────────────────────────────────────┐
    │  PIFO.insert                                 │
    │  { vPifo=vPifoId, rank=Rank, data=Token }    │
    └──────────────────────────────────────────────┘
```

### Dequeue Path

```
    ┌─────────────────────┐
    │  DequeueRequest     │
    │  engineId ## vPifoId│
    └──────────┬──────────┘
               │
               ▼
    ┌─────────────────────┐
    │  PIFO.dequeue       │
    │  vPifoId →          │
    │{PacketToken, exist?}│-- if not exist? --> Drop
    └──────────┬──────────┘
               │ if exist?
               ▼
    ┌──────────────────────────────┐
    │  DequeueMapper               │
    │  data (PacketToken)          │
    │  → PacketToken               │
    └──────────┬───────────────────┘
               │ PacketToken
               ▼
    ┌──────────────────────────────┐
    │  Crossbar (xbar)             │
    |  case engineId               |
    │    Port 0 → Output           │
    │    Other  → Other Engines    │
    └──────────────────────────────┘
```

**Note**: For simplicity, `flowId` and `vPifoId` have the same width and use the name `vPifoId` in the code.

## Transactional configuration

The packet-visible mapping commands are transactional:

- Every command uses the mesh's single ready/valid control ingress. It accepts at most one instruction per clock;
  targeting different engines does not create parallel configuration ports. The ingress queue absorbs stalls but does
  not increase this rate.
- `UpdateMapperPre` and `UpdateMapperPost` write backup banks and do not immediately affect packets.
- `UpdateMapperNonExist` writes a single-bank per-engine front table as `{source vPIFO -> target vPIFO, enabled=false}`.
  Commit arms the new entry; popping the source's final entry enables it. The PE holds its input for that activation
  cycle, then subsequent requests are rewritten before the PIFO lookup without a retry or crossbar loopback.
- `UpdateMapperPost` is keyed by `(vPifoId, flowId)`, so multiple tree versions can retain different next hops for the same flow.
- `CommitMapper` publishes every pending mapper update across every engine on one clock edge. Its payload and `engineId` are ignored.
- A packet request accepted on the commit edge uses the old mappings; requests accepted after that edge use the new mappings.
- Replay is the default mapper synchronization: the controller retains commands in its existing FIFO, swaps banks,
  then rereads the same entries and reissues only pre/post-mapper writes into the shadow bank. There is no separate
  journal. Each mapper bank retains one packet-read port and one configuration-write port.
- `io.commitReady` is low and `io.replayBusy` is high during replay. Ingress and later queued commands wait until
  replay finishes; packet traffic continues. Empty commits need no replay-busy interval.
- The controller FIFO defaults to **256 entries**, with one reserved for commit. An epoch can retain at most 255
  commands from its first mapper update to the command preceding commit, including intervening unbanked commands.
  `io.replayLogAvailable` reports free non-commit slots in this same FIFO. Set `EngineConfig.commitQueueLength`
  or the simulator's `--control-queue-depth` to a power of two at least two for a different batch capacity.
- `GuardDrain` blocks the entire command queue until the specified `engineId:vPifoId` is drained. It uses the PEs'
  last-successful-pop (nearly-drained) notifications, remembers early notifications, and clears that state on FIFO
  refill. An already-empty FIFO passes. The producer must quiesce the old FIFO before reclamation.
  The guard only blocks: subsequent ordinary mapper/brain writes and `CommitMapper` perform cleanup.
  Guards execute on the first pass and are skipped during mapper replay.
- Brain policy and brain-state commands remain immediate and are intentionally outside the mapper transaction.

The experiment tools use explicit compiler and simulator boundaries:

- `pifo_tree_compiler.py` is the only component that understands a declarative tree move. It supports additive
  `in_place`, lossless `stop_the_world`, `full_transitive`, and partial-tree `confined_transitive` plans.
  Full and confined replacements allocate nonzero vPIFO IDs (vPIFO 0 remains the null sink) and emit the required
  same-engine front rewrite.
  Every move also emits a second, ordinary cleanup transaction: guard every retired FIFO, write invalid old mapper
  slots/reset retired brains, then commit. The live front-rewrite alias and shared ancestors remain intact. Additive
  and reset moves have nothing retired to invalidate and use an empty cleanup commit.
- `pifo_simulator.py` accepts that direct transaction timeline and a separate traffic-pattern timeline. It supports
  multiple packages at different cycles and never interprets trees or policies.
- `pifo_bandwidth_figure.py` and `pifo_packet_scatter_figure.py` render independently. Their data and resources stay
  in separate directories; only result/event loading and low-level drawing helpers are shared in
  `pifo_figures/common.py`.
- Every figure folder also includes a self-contained `plot.py`. It reads only local CSVs, has its own literal labels,
  timestamps and styling, and imports only the standard library and Matplotlib. Copy the folder anywhere and run
  `python plot.py` to recreate `figure.svg` and `figure.png`. Edit settings at the top of that script to customize it.
  Bandwidth is plotted from cached sampled/smoothed values, without a second convolution.
- Every figure, including throughput and comparisons, ships a local `packets.csv` with one row per generated packet:
  `request_id,flow,flow_name,size_bytes,push_cycle,pop_cycle,delay_cycles,dropped`. Comparisons add a `run` column.
  `flow` is the numeric flow ID and `flow_name` is its label. `push_cycle` is source generation, so delay includes time
  queued outside the switch; dropped packets have blank `pop_cycle` and `delay_cycles`. The run-level
  `packet-outcomes.csv` remains the simulator's complete raw trace; `data.csv` retains each figure's plotted data.
- `pifo_experiment_figures.py` invokes the compiler, simulator, and both per-figure CLIs before verification.

The request simulator records package start, commit queue acceptance, actual commit publication, and a drain/capture cycle for
transition modes that define one. It briefly gates new request admission at the commit edge so the per-engine tokens
for one request cannot land in different tree versions; packets already admitted continue normally. Lossless
stop-the-world additionally freezes admission and dequeue, captures the buffered packet metadata, resets and installs
the target, replays exactly one scheduler token per retained packet, and then resumes after `minStopCycles`. Traffic
sources continue generating while the gate is closed. Packet delay is measured from source generation, and the stop
event reports both the capture-time retained count and the peak number of outstanding buffered packets.
Figures show **both commits**: C1 installs the transition and C2 reclaims the retired configurations. Each has
`start`, `commit accepted`, `ready_for_next_commit`, and `old-tree-drained` markers. Pale blue (C1) and pale amber
(C2) backgrounds run from that commit's start to its readiness; C2's interval includes drain-guard waiting.
C1 readiness uses `install_finish_cycle`, while C2 readiness uses `cleanup_finish_cycle` (the raw CSV's
`finish_cycle`). Both commits share the same retired-root drain measurement, so those two lines coincide.
Additive changes label drain as not required; STW labels the capture event instead of pretending it is a drain,
and shows `resume_cycle` separately. Input/output scatter marks both axes at equal scale; delay plots mark only x.
Legends list the exact absolute cycle for each marker, including coincident events. Both commits report cycles to
publication and instruction counts (including guards and commit), with bank-replay cycles separate. Raw direct programs are not rewritten; author their
guard/write/commit packages explicitly and use `cleanupOf=<transition-name>` to associate the cleanup in reports.

Focused checks (no experiment sweep):

```bash
.venv/bin/python -m unittest discover -s hw/python/tests
sbt 'runMain rio.sim.DrainGuardSim' 'runMain rio.sim.ControlIngressRateSim'
PIFO_RTL_SMOKE=1 .venv/bin/python -m unittest discover -s hw/python/tests -p test_pifo_cleanup.py
```

The four motivating-example runs each generate an independent throughput and packet-delay scatter figure. R2–R4 also
share one scatter figure, and R3/R4 share one throughput comparison:

```bash
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
.venv/bin/python hw/python/pifo_motivation_all.py
```

Use `pifo_motivation_r1.py` through `pifo_motivation_r4.py` to run one case, or pass `--render-only` to reuse raw CSVs.
To add standalone scripts and local packet traces to saved results without rerunning simulations or redrawing figures, run
`.venv/bin/python hw/python/pifo_export_plot_scripts.py`. Legacy folders with two named figures receive two
`rr-to-sp-*-plot.py` scripts instead of one `plot.py`. Exporting again replaces the generated scripts, so keep custom
edits separately if you will regenerate an experiment.
Legacy root-level plots use `rr-to-sp-packets.csv` for their archived completions. The old 160-packet RR plots and
the current 480-packet run are distinct datasets: unavailable historical packet sizes remain blank, and historical
drop records cannot be reconstructed. New figures always use the complete simulator outcomes, including drops.

The checked `experiments/large-tree-rr-to-sp.json` regression uses a seven-node tree over four engines and validates
the observable RR-before-commit, old-tree-drain-first, and SP-after-drain phases. A `verification` block makes these
checks automatic and produces machine-readable and Markdown reports beside the experiment figures; see
`REQUEST_SIMULATOR.md` for the commands and reference measurements.

`PifoMeshSimController.transaction` stages a configuration, commits it, and returns a thread that completes after
the FIFO finishes replay (or consumes an empty commit). The older `config` helper is retained as an alias. Control-socket users must include a
`CommitMapper` line; `hw/python/config_to_socket_commands.py` emits one for each transaction.

Replay correctness checks (simulation only, with Icarus Verilog on `PATH`):

```bash
sbt 'runMain rio.sim.ReplayControlFifoSim' 'runMain rio.sim.ReplayMapperSim' \
    'runMain rio.sim.SharedReplayDriverSim' 'runMain rio.sim.TransactionalConfigSim' \
    'runMain rio.sim.ControlIngressRateSim' 'runMain rio.sim.FrontUnderflowRewriteSim'
```

The FIFO test covers depths 2, 4, 8, and the default 256 under backpressure,
wraparound, empty/full epochs, and reset. A standalone `PifoEngine` or `ReplayMapper`
must receive replay from its caller; `PifoMesh` supplies it automatically. The
older standalone `TransactionalMapper` component remains available for existing
direct users and is no longer instantiated by the mesh. Reset flushes controller
commands; it does not recover a mapper transaction interrupted during replay.

## TODO List

- [x] Make an unconfigured non-exist PIFO pop return no valid mesh message.
- [ ] Support packet meta data and packet identifier in mesh message and brain
- [ ] Support configurable brain policy
    - [ ] support configurable rank in WFQ
- [ ] Hardware controller for insertion and pop
- [x] Transactional update of mappers.
    - [ ] change some of the mappers to CAMs
- [ ] deque mapper 
    - [ ] support exist signal
    - [ ] support TCAM match (on vPifoId)
