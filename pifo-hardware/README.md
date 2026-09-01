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
- `UpdateMapperPre`, `UpdateMapperPost`, and `UpdateMapperNonExist` write backup banks and do not immediately affect packets.
- `UpdateMapperPost` is keyed by `(vPifoId, flowId)`, so multiple tree versions can retain different next hops for the same flow.
- `CommitMapper` publishes every pending mapper update across every engine on one clock edge. Its payload and `engineId` are ignored.
- A packet request accepted on the commit edge uses the old mappings; requests accepted after that edge use the new mappings.
- `io.commitReady` is low while the newly active banks are copied back into the backup banks. Another mapper update or commit waits until it returns high. Packet traffic continues during this synchronization.
- Brain policy and brain-state commands remain immediate and are intentionally outside the mapper transaction.

The experiment tools use explicit compiler and simulator boundaries:

- `pifo_tree_compiler.py` is the only component that understands a declarative tree move. It emits an initial package
  plus a `full_transitive` direct package that allocates a fresh complete tree, redirects every input, and chains the
  drained old root to the new root.
- `pifo_simulator.py` accepts that direct transaction timeline and a separate traffic-pattern timeline. It supports
  multiple packages at different cycles and never interprets trees or policies.
- `pifo_bandwidth_figure.py` and `pifo_packet_scatter_figure.py` render independently. Their data and resources stay
  in separate directories; only result/event loading and low-level drawing helpers are shared in
  `pifo_figures/common.py`.
- `pifo_experiment_figures.py` invokes the compiler, simulator, and both per-figure CLIs before verification.

The request simulator records package start, commit acceptance, synchronization finish, and—only for full-transitive
changes—the old-root drain cycle. It briefly gates new request admission at the commit edge so the per-engine tokens
for one request cannot land in different tree versions; packets already admitted continue normally.

The checked `experiments/large-tree-rr-to-sp.json` regression uses a seven-node tree over four engines and validates
the observable RR-before-commit, old-tree-drain-first, and SP-after-drain phases. A `verification` block makes these
checks automatic and produces machine-readable and Markdown reports beside the experiment figures; see
`REQUEST_SIMULATOR.md` for the commands and reference measurements.

`PifoMeshSimController.transaction` stages a configuration, commits it, and returns a thread that completes after
`commitReady` rises again. The older `config` helper is retained as an alias. Control-socket users must include a
`CommitMapper` line; `hw/python/config_to_socket_commands.py` emits one for each transaction.

## TODO List

- [ ] Support per-PIFO copy, make non-exist pifo pop return an invalid message
- [ ] Support packet meta data and packet identifier in mesh message and brain
- [ ] Support configurable brain policy
    - [ ] support configurable rank in WFQ
- [ ] Hardware controller for insertion and pop
- [x] Transactional update of mappers.
    - [ ] change some of the mappers to CAMs
- [ ] deque mapper 
    - [ ] support exist signal
    - [ ] support TCAM match (on vPifoId)
