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
request is then stored in a bounded FIFO selected by `global_flow_id`. Root pops are issued at the hardware's
three-cycle accepted initiation interval. Terminal scheduler tokens enter the configured prefetch window; the request
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

## Reconfiguration workflow

The scripts have four narrow layers:

1. `pifo_tree_compiler.py` converts a declarative tree move into direct controller transactions. It supports
   `in_place`, `stop_the_world`, `full_transitive`, and `confined_transitive` modes.
2. `pifo_simulator.py` accepts exactly two model files—a direct transaction timeline and a traffic-pattern timeline—and
   produces the raw request, completion, and event CSVs.
3. `pifo_bandwidth_figure.py` and `pifo_packet_scatter_figure.py` independently derive and render one figure each.
   Shared result/event parsing and drawing primitives live in `pifo_figures/common.py`; figure-specific data and
   rendering stay in `pifo_figures/bandwidth.py` and `pifo_figures/packet_scatter.py`.
4. `pifo_experiment_figures.py` invokes the compiler, simulator, both figure CLIs, and optional verification.

The checked example starts with RR and changes to SP:

```bash
python3 hw/python/pifo_experiment_figures.py validate experiments/rr-to-sp.json
python3 hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp.json
```

The output directory exposes every boundary: `tree-move.json`, `traffic.json`, compiled `transactions.txt`, request and
completion CSVs, and `reconfiguration-events.csv`. Each figure owns a separate artifact directory:

- `figures/bandwidth/{data.csv,figure.svg,figure.png}`
- `figures/packet-scatter/{data.csv,figure.svg,figure.png}`

Matplotlib is preferred; SVG plus FFmpeg is used automatically when Matplotlib is unavailable. The scatter uses one
shared 1:1 range for its input/output axes, keeps `y = x` at 45 degrees, and draws start, commit, and old-tree-drain
lines on both axes.

Install the plotting dependency for the motivating-example delay plots in an isolated environment:

```bash
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

### Four-run motivating example

Each run has a minimal standalone script and always creates both formats requested from its own raw packet CSV:

```bash
.venv/bin/python hw/python/pifo_motivation_r1.py
.venv/bin/python hw/python/pifo_motivation_r2.py
.venv/bin/python hw/python/pifo_motivation_r3.py
.venv/bin/python hw/python/pifo_motivation_r4.py
```

Run all four plus the shared-axis comparisons with:

```bash
.venv/bin/python hw/python/pifo_motivation_all.py
```

Resources live under `experiments/motivating-example/`; outputs live under
`experiment-results/motivating-example/<case>/`. Every case contains `packet-outcomes.csv` with
`flow,push_cycle,pop_cycle,dropped` (plus request ID and size), `reconfiguration-events.csv`, and:

- `figures/throughput/{data.csv,figure.svg,figure.png}`
- `figures/delay-scatter/{data.csv,figure.svg,figure.png}`

The combined outputs are `comparisons/r2-r4-delay-scatter` and `comparisons/r3-r4-throughput`. Use `--render-only`
on any case or the all-case script to regenerate figures without rerunning RTL. The all-case validator checks identical
input traces, losslessness and per-flow FIFO order for all four runs, R2's minimum stop interval and retained-token
count, the R3/R4 drain ordering, and the expected R3-only zoom delay spike.

### Per-figure CLIs

Regenerate only the bandwidth figure and its aggregate/per-flow data:

```bash
python3 hw/python/pifo_bandwidth_figure.py \
  --results experiment-results/rr-to-sp/request-results.csv \
  --events experiment-results/rr-to-sp/reconfiguration-events.csv \
  --output-dir experiment-results/rr-to-sp/figures/bandwidth \
  --link-bytes-per-cycle 64 \
  --window-cycles 320 --sample-cycles 8 --flow-labels 1:A,2:B
```

The bandwidth series is a centered, normalized Hann convolution of packet-completion bytes. `--window-cycles`
sets the averaging timescale (larger is smoother), while `--sample-cycles` controls only how often that continuous
estimate is written and drawn. In an experiment JSON file, use the equivalent plot controls:

```json
"plot": {
  "bandwidth_window_cycles": 320,
  "bandwidth_sample_cycles": 8
}
```

Regenerate only the packet timing data and 1:1 scatter figure:

```bash
python3 hw/python/pifo_packet_scatter_figure.py \
  --results experiment-results/rr-to-sp/request-results.csv \
  --events experiment-results/rr-to-sp/reconfiguration-events.csv \
  --output-dir experiment-results/rr-to-sp/figures/packet-scatter \
  --flow-labels 1:A,2:B
```

### Two-file simulator CLI

Run an already-compiled workload without involving tree logic:

```bash
python3 hw/python/pifo_simulator.py \
  --transactions experiment-results/rr-to-sp/transactions.txt \
  --traffic experiment-results/rr-to-sp/traffic.json \
  --output-dir /tmp/pifo-run \
  --queue-depth 256 --link-bytes-per-cycle 64 --max-cycles 100000
```

The traffic file contains independently configurable patterns over time. Patterns may overlap; generated packets are
merged by cycle and assigned stable request IDs:

```json
{
  "schema": "pifo-traffic-v1",
  "seed": 7,
  "patterns": [
    {
      "name": "warmup",
      "start_cycle": 0,
      "flows": [1, 2],
      "packets_per_flow": 20,
      "packet_rate": {
        "distribution": "constant",
        "unit": "packets_per_cycle_per_flow",
        "value": 0.1
      },
      "packet_size_bytes": {"distribution": "constant", "value": 256}
    },
    {
      "name": "load-step",
      "start_cycle": 200,
      "flows": [1, 2],
      "packets_per_flow": 40,
      "packet_rate": {
        "distribution": "uniform",
        "unit": "packets_per_cycle_per_flow",
        "min": 0.15,
        "max": 0.25
      },
      "packet_size_bytes": {
        "distribution": "normal",
        "mean": 512,
        "stddev": 64,
        "min": 64,
        "max": 1500
      }
    }
  ]
}
```

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

### Tree-move compiler CLI

The compiler is the only layer that understands trees, policies, tree copying, or miss rewrites:

```bash
python3 hw/python/pifo_tree_compiler.py \
  --input experiment-results/rr-to-sp/tree-move.json \
  --output /tmp/transactions.txt
```

Its `pifo-tree-move-v1` input contains `hardware`, `old_tree`, and one declarative `move`. The move can provide a full
`target_tree` and one of the four modes above. The output is the exact direct timeline above: an initial-tree package
followed by one compiled package. The simulator therefore has no implicit tree-to-command translation. vPIFO 0 is
reserved as the mapper-reset null/NOP sink and is never allocated as a real copied node.

### Traffic and policy-change format

The compact policy-change form is:

```json
{
  "output_dir": "experiment-results/rr-to-sp",
  "seed": 7,
  "traffic": {
    "flows": [1, 2],
    "packets_per_flow": 240,
    "start_cycle": 0,
    "packet_rate": {
      "distribution": "uniform",
      "unit": "packets_per_cycle_per_flow",
      "min": 0.18,
      "max": 0.24
    },
    "packet_size_bytes": {
      "distribution": "normal",
      "mean": 128,
      "stddev": 24,
      "min": 64,
      "max": 192
    }
  },
  "reconfiguration": {
    "type": "policy_change",
    "cycle": 600,
    "before": "RR",
    "after": "SP",
    "strict_priorities": {"1": 1, "2": 32769}
  }
}
```

`full_transitive` copies every target node, redirects new inputs to the copy, and front-rewrites the old physical root
after it drains. `confined_transitive` finds the single changed subtree boundary, copies only that subtree, keeps all
unchanged ancestors in place, and installs the rewrite at that boundary. `in_place` accepts additive flow/path state
that leaves existing nodes and paths unchanged. `stop_the_world` pauses admission and root pops, lets prefetched output
finish, retains the buffered request metadata, resets the mesh, installs the target on the original physical root, and
replays one scheduler token for every retained request before resuming. Arrivals during the stop remain pending rather
than being dropped. The motivating R2 resource sets `minimum_stop_cycles` to 1024, which is at least 1.024 microseconds
for clocks at or below 1 GHz. In both transitive modes the front entry is initially disabled; the source's final
successful pop enables it for the next request, with no underflow retry or extra mesh hop.

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

### Direct transaction semantics

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

An enabled front entry substitutes the target vPIFO before the engine performs its PIFO lookup. On the activation
cycle, the engine backpressures its input once so the waiting request observes the registered enable on the next cycle.
There is no retry: the last source pop remains valid, and the next request directly pops the target. The transition II
is two cycles and steady-state traffic retains one accepted pop per engine cycle without a mesh loopback. An
unconfigured underflow produces no valid mesh message.

### Reconfiguration timestamps and drain time

A full-transitive run records, for example:

```csv
event,name,mode,from_policy,to_policy,instruction_count,scheduled_cycle,start_cycle,commit_cycle,finish_cycle,drain_cycle,drain_duration_cycles
reconfiguration,policy-change,full_transitive,RR,SP,9,600,600,608,4708,2424,1816
```

- `start_cycle`: package feeding starts.
- `instruction_count`: controller instructions in the package, including `CommitMapper`.
- `commit_cycle`: the `CommitMapper` ready/valid transfer is accepted by the controller queue.
- `drain_cycle`: the cycle on which the old boundary's final successful PIFO pop raises `portDrained`; for
  `stop_the_world`, this is the cycle on which the quiesced old tokens are captured before reset.
- `finish_cycle`: commit application and backup-bank synchronization have completed. For `stop_the_world`, it also
  means retained tokens have been replayed, the minimum stop has elapsed, and traffic has resumed.
- `drain_duration_cycles`: `drain_cycle - commit_cycle` for transitive drain modes; blank for stop-the-world because
  capture precedes commit.
- `retained_packets`, `minimum_stop_cycles`, and `stop_duration_cycles` describe a lossless stop; the last value is
  `finish_cycle - drain_cycle`.

`finish_cycle` and `drain_cycle` are independent: mapper synchronization may finish while an old transitive tree is
still draining. For stop-the-world, capture occurs before the replacement package commits and finish marks resume.

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

The reference package has 27 commands: 7 new-node brain selections, 2 SP flow-state writes, 16 per-path mapper
writes, 1 front underflow rewrite, and 1 commit. The run accepted them with a 28-cycle span (`start=240`, `commit=268`), had 45 old
packets pending at commit, drained the old tree at cycle 1464 (1196 drain cycles), and finished mapper synchronization
at cycle 8464. It observed
9 completions before commit, 45 during old-tree drain, and 66 after drain, with zero RR repetitions, zero early
new-tree outputs, zero late old-tree outputs, and zero SP priority reversals. A packet admitted on the commit edge is
classified as old, matching the mapper-bank publication contract.

The Python simulator converts only the traffic patterns to canonical request CSV. It passes that CSV and the unchanged
direct timeline to Scala with `--trace` and `--transactions`; the old bundle of single-transaction flags is gone. Run
`sbt 'runMain rio.sim.RequestSimulatorCli --help'` for the low-level syntax. The live control socket remains available
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
