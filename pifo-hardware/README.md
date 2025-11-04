# Runtime reconfigurable pifo hardware

```bash
sbt "runMain rio.sim.PifoMeshSim"
```

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
    │  EnqueueMapper              │-───────────┤
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

## TODO List

- [ ] Support per-PIFO copy, make non-exist pifo pop return an invalid message
- [ ] Support packet meta data and packet identifier in mesh message and brain
- [ ] Support configurable brain policy
    - [ ] support configurable rank in WFQ
- [ ] Hardware controller for insertion and pop
- [ ] Transactional update of 



