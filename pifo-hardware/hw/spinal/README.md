# PIFO hardware

SpinalHDL sources live in `rio/`; the request harness and focused simulations are
in `rio/sim/`. Verilog sources live in `../verilog/`. Run commands from
`pifo-hardware/`.

For declarative configuration, see the [Python compiler guide](../python/README.md).
For trace-driven and live workloads, see the [simulator guide](rio/sim/README.md).

## Datapath

The insert input is a `PacketToken = engineId ## flowId`.
The token follows these enqueue and dequeue paths.

### Enqueue path

```text
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

### Dequeue path

```text
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

`PifoMeshSimController.transaction` stages a configuration, commits it, and returns a thread that completes after
the FIFO finishes replay (or consumes an empty commit). The older `config` helper is retained as an alias. Control-socket users must include a
`CommitMapper` line; `hw/python/config_to_socket_commands.py` emits one for each transaction.

## Evaluation hardware

The normal `PifoMesh` / `RequestSimulatorCli` build is the production image. It does not elaborate the stop gate,
prefill controller, occupied-PIFO copy/inject ports, maintenance occupancy probes or hierarchical evaluation ranker.
The separate top levels under `hw/spinal/rio/evaluation/` are `EvaluationPifoRTL`, `EvaluationPifoEngine`,
`EvaluationPifoMesh`, and `rio.sim.EvaluationRequestSimulatorCli`. Both images use the same shared-FIFO mapper replay
and drain guards. Maintenance programs must explicitly select `pifo_simulator.py --evaluation-hardware`;
the experiment runners do this automatically. The default simulator rejects evaluation-only commands.
The normal build also retains its existing priority encoder. The alternate encoder used by the evaluation
branch is isolated under `hw/verilog/evaluation/`, so it cannot change the normal image or its Icarus behavior.

`StopWorld` backpressures all hardware insert ports and the root-pop input. `PrefillPifo data=0` uses the stopped old
root's per-vPIFO occupancy as `N` and autonomously inserts `N` priority-1 scheduler tokens through the second PIFO
push port. `UpdateRoot` stages a new physical root. Commit waits for prefill completion, publishes that root, and
releases traffic.

Check production/evaluation isolation without simulation:

```bash
sbt 'runMain rio.EvaluationBuildCheck'
```

## Core hardware tests

These simulations use small, direct inputs and assert hardware behavior. They
require JDK 17, sbt, and Icarus Verilog on `PATH`:

```bash
sbt 'runMain rio.sim.BasicPifoSim' 'runMain rio.sim.ReplayControlFifoSim' \
    'runMain rio.sim.ReplayMapperSim' 'runMain rio.sim.SharedReplayDriverSim' \
    'runMain rio.sim.TransactionalConfigSim' 'runMain rio.sim.DrainGuardSim' \
    'runMain rio.sim.ControlIngressRateSim' 'runMain rio.sim.FrontUnderflowRewriteSim'
```

`BasicPifoSim` checks priority order, stable ties, port isolation, and empty
responses. The remaining checks cover FIFO replay, mapper visibility, atomic
configuration, drain guards, control ingress rate, and front rewrites.

The FIFO test covers depths 2, 4, 8, and the default 256 under backpressure,
wraparound, empty/full epochs, and reset. A standalone `PifoEngine` or `ReplayMapper`
must receive replay from its caller; `PifoMesh` supplies it automatically. The
older standalone `TransactionalMapper` component remains available for existing
direct users and is no longer instantiated by the mesh. Reset flushes controller
commands; it does not recover a mapper transaction interrupted during replay.

For a small mesh demonstration:

```bash
sbt 'runMain rio.sim.PifoMeshSim'
```

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
