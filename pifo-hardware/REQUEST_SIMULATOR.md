# Request-level PIFO mesh simulator

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
request is then stored in a bounded FIFO selected by `global_flow_id`. When a terminal token leaves the mesh, the head
request from that flow is completed. The output link remains busy for
`ceil(size_bytes / link_bytes_per_cycle)` cycles before the next root dequeue.

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

## Reconfiguration experiment figures

`hw/python/pifo_experiment_figures.py` generates the workload, controller instructions, RTL results, and both requested
figure styles. The checked example starts with RR and changes to SP:

```bash
python3 hw/python/pifo_experiment_figures.py validate experiments/rr-to-sp.json
python3 hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp.json
```

The output directory contains the effective JSON, `initial-tree.commands`, `reconfiguration.commands`, request and
completion CSVs, `reconfiguration-events.csv`, aggregate/per-flow bandwidth data and figure, and the full packet
input/output scatter data and figure. Matplotlib is preferred; SVG plus FFmpeg is used automatically when Matplotlib
is unavailable. The scatter uses one shared 1:1 range for its input/output axes, keeps `y = x` at 45 degrees, and
draws start, commit, and old-tree-drain lines on both axes.

### Traffic and policy-change format

The compact policy-change form is:

```json
{
  "output_dir": "experiment-results/rr-to-sp",
  "seed": 7,
  "traffic": {
    "flows": [1, 2],
    "packets_per_flow": 80,
    "start_cycle": 0,
    "packet_rate": {
      "distribution": "uniform",
      "unit": "packets_per_cycle_per_flow",
      "min": 0.1,
      "max": 0.15
    },
    "packet_size_bytes": {
      "distribution": "normal",
      "mean": 256,
      "stddev": 64,
      "min": 64,
      "max": 512
    }
  },
  "reconfiguration": {
    "type": "policy_change",
    "cycle": 320,
    "before": "RR",
    "after": "SP",
    "strict_priorities": {"1": 1, "2": 32769}
  }
}
```

Every non-direct `policy_change` is forced to `full_transitive`; specifying any other `mode` is rejected. The runner
creates an unused copy of every node in the old tree, configures the new brains, stages new per-flow input and
port-qualified output mappings, maps the new root's miss to output, maps the old root's miss to the new root, and then
emits `CommitMapper`. Root dequeue requests continue to target the old physical root. Packets accepted before commit
drain through the old tree, while later packets enter the new tree.

The implicit initial tree is one root at engine 1 / vPIFO 10. For a multi-node tree, add:

```json
"initial_tree": {
  "root": "root",
  "nodes": {
    "root": {"engine_id": 1, "vpifo_id": 10, "policy": "RR"},
    "leaf": {"engine_id": 2, "vpifo_id": 12, "policy": "FIFO"}
  },
  "flow_paths": {
    "1": ["root", "leaf"],
    "2": ["root", "leaf"]
  }
}
```

A flow path starts at the root and may use at most one node per engine, because one request contributes one hardware
token per engine. Advanced policy changes can use `changes` instead of `before`/`after`:

```json
"reconfiguration": {
  "type": "policy_change",
  "cycle": 320,
  "changes": {
    "root": {"policy": "SP", "flow_state": {"1": 1, "2": 32769}}
  }
}
```

Traffic is generated in rounds, with one packet per flow per round. `packet_rate` is sampled per flow in packets per
cycle; `0.125` means one round every eight cycles. `packets_per_flow` is the count for each flow. Rate and packet size
support `constant`, `uniform`, and bounded `normal` distributions:

```json
{"distribution": "constant", "value": 0.125}
{"distribution": "uniform", "min": 0.1, "max": 0.15}
{"distribution": "normal", "mean": 0.125, "stddev": 0.02, "min": 0.05, "max": 0.25}
```

Packet rates must also set `"unit": "packets_per_cycle_per_flow"`. Normal samples are clamped to `min`/`max`; packet
sizes are rounded to positive bytes. Rate and size use separate seeded random streams. Lower SP values run first, and
priority zero is rejected.

### Direct transaction packages

Use `transaction_package` when the experiment already has exact controller instructions. The package is not expanded,
rewritten, or interpreted as a policy change; its commands are fed to the hardware control queue in listed order at
`cycle`. It must end with exactly one `CommitMapper`:

```json
"reconfiguration": {
  "type": "transaction_package",
  "cycle": 320,
  "name": "manual-change",
  "before_label": "RR",
  "after_label": "SP",
  "commands": [
    {"command": "UpdateBrainFlowState", "engineId": 1, "vPifoId": 10, "flowId": 33, "data": 1},
    {"command": "UpdateBrainFlowState", "engineId": 1, "vPifoId": 10, "flowId": 34, "data": 32769},
    {"command": "UpdateBrainEngine", "engineId": 1, "vPifoId": 10, "flowId": 0, "data": 2},
    {"command": "CommitMapper", "engineId": 1, "vPifoId": 0, "flowId": 0, "data": 0}
  ]
}
```

Each object translates literally to one controller line:

```text
command=UpdateBrainFlowState engineId=1 vPifoId=10 flowId=33 data=1
```

There is one mesh-wide ready/valid configuration ingress, so at most one of these lines can be accepted on each clock.
Commands for different engines are still serialized. A package containing `N` objects therefore has `N` instruction
acceptances (and can take longer if the ingress is backpressured); the small queue only buffers accepted commands.

`UpdateMapperPre` uses `vPifoId` as the raw input flow and `data` as the destination vPIFO.
`UpdateMapperPost` uses `(vPifoId, flowId)` as its key and `data` as the packed next-hop `(engine, vPifo)`.
`UpdateMapperNonExist` uses `vPifoId` as its key and `data` as the packed miss next hop. Brain commands use
`engineId`/`vPifoId` as their target and `flowId` where required. Brain writes are immediate, so a direct package owns
their ordering and does not gain brain atomicity from `CommitMapper`.

### Reconfiguration timestamps and drain time

A full-transitive run records, for example:

```csv
event,name,mode,from_policy,to_policy,instruction_count,scheduled_cycle,start_cycle,commit_cycle,finish_cycle,drain_cycle,drain_duration_cycles
reconfiguration,policy-change,full_transitive,RR,SP,10,320,320,330,4429,939,609
```

- `start_cycle`: package feeding starts.
- `instruction_count`: controller instructions in the package, including `CommitMapper`.
- `commit_cycle`: the `CommitMapper` ready/valid transfer is accepted by the controller queue.
- `drain_cycle`: the old root first reports non-existence after commit, or the final old request completes.
- `finish_cycle`: commit application and backup-bank synchronization have completed.
- `drain_duration_cycles`: `drain_cycle - commit_cycle`.

Packet admission is paused only across the commit edge so one packet cannot be split between tree versions. Existing
PIFO traffic continues during staging, drain, and mapper synchronization. Figure captions show the instruction count
and the one-accepted-instruction-per-cycle limit. Direct packages record start, commit, and finish; drain fields are
blank because their semantics are intentionally opaque.

### Large-tree phase regression

`experiments/large-tree-rr-to-sp.json` is the stress case for full-transitive ordering. It uses seven PIFOs along two
four-engine paths:

```text
root (engine 1, RR -> SP)
|- class_a (engine 2) -> leaf_a (engine 3) -> egress_a (engine 4): flow 1
`- class_b (engine 2) -> leaf_b (engine 3) -> egress_b (engine 4): flow 2
```

The 120-packet trace uses 512-byte packets, a 128-packet per-flow feeder queue, and arrivals spread across cycles
0–590. This keeps the plotted backlog compact while leaving both old- and new-epoch queues around the commit. Its
`verification` object sets minimum staging, old-backlog, drain-duration, and per-phase packet counts. When this object
is present, `run` writes `phase-verification.json` and `phase-verification.md` and exits nonzero unless all of these
properties hold:

1. The transaction and old-tree drain are long enough to observe.
2. Output before commit follows RR.
3. From commit until drain, only old-tree packets leave and they continue following RR.
4. After drain, only new-tree packets leave and SP priority order has no reversal.

```bash
python3 hw/python/pifo_experiment_figures.py validate experiments/large-tree-rr-to-sp.json
python3 hw/python/pifo_experiment_figures.py run --config experiments/large-tree-rr-to-sp.json
```

To check saved CSVs without rerunning RTL:

```bash
python3 hw/python/pifo_experiment_figures.py verify \
  --config experiments/large-tree-rr-to-sp.json \
  --results experiment-results/large-tree-rr-to-sp/request-results.csv \
  --events experiment-results/large-tree-rr-to-sp/reconfiguration-events.csv \
  --output-dir experiment-results/large-tree-rr-to-sp
```

The reference package has 28 commands: 7 new-node brain selections, 2 SP flow-state writes, 16 per-path mapper
writes, 2 miss redirects, and 1 commit. The run accepted them in 28 cycles (`start=240`, `commit=268`), had 45 old
packets pending at commit, drained the old tree at cycle 1475 (1207 drain cycles), and finished mapper synchronization
at cycle 8464. It observed
9 completions before commit, 45 during old-tree drain, and 66 after drain, with zero RR repetitions, zero early
new-tree outputs, zero late old-tree outputs, and zero SP priority reversals. A packet admitted on the commit edge is
classified as old, matching the mapper-bank publication contract.

To replot an existing run:

```bash
python3 hw/python/pifo_experiment_figures.py plot \
  --results experiment-results/rr-to-sp/request-results.csv \
  --events experiment-results/rr-to-sp/reconfiguration-events.csv \
  --output-dir experiment-results/rr-to-sp
```

The Scala CLI accepts the generated streams directly with `--control-file`, `--scheduled-transaction`,
`--transaction-cycle`, `--transaction-mode`, `--transaction-drain-root`, and `--transaction-event-output`. Run
`sbt 'runMain rio.sim.RequestSimulatorCli --help'` for the complete syntax. The live control socket remains available
at `/tmp/rio-control.sock` unless disabled.

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
