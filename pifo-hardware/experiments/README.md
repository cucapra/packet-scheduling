# Reproducing the experiments

Keep experiment JSONs and Python sources in version control. All raw traces,
compiled transaction plans, measurements, reports, exported standalone plotters,
and PNG/SVG figures are generated under the ignored `experiment-results/` tree.
Plotting sources live in `hw/python/pifo_*figure*.py`, `hw/python/pifo_figures/`,
the comparison scripts, and `experiments/scalability/plot.py`.

Install Python 3.10 or newer, JDK 17, sbt, Icarus Verilog, Verilator, and a C++
build toolchain. Select JDK 17 with `JAVA_HOME` and `PATH`, then run from
`pifo-hardware/`:

```sh
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
.venv/bin/python run_experiments.py
```

The scalability sweep was developed with
`JAVA_TOOL_OPTIONS='-Xmx6G -XX:ActiveProcessorCount=2'`; set this before running
on a machine where JVM heap or thread defaults are unsuitable. Its large RTL
configuration and 40 simulations make the full sweep take tens of minutes.

The runner uses the invoking Python environment, runs from the repository's
hardware directory regardless of the caller's working directory, and stops at
the first simulation, verification, or plotting failure. It runs suites serially
because the RTL simulators share their build workspace. Existing outputs are
overwritten; no saved result is required or reused. Scalability uses its batch
backend to compile each hardware configuration once and run fresh simulator
instances for its 40 cases.

RTL initialization is seeded as well as traffic generation. The simulator uses
the traffic seed modulo 2³¹ unless `SPINAL_SIM_SEED` or `--simulation-seed` supplies
an override; the scalability batch uses the same default. Motivating-example
settings explicitly retain RTL seed `1179218032` from the original R2 run.
Without that seed, its late RR packet timings can change even when the transition
measurements agree. This seed is separate from the traffic seed.

| Suite | Source inputs | RTL runs |
| --- | --- | ---: |
| `rr-to-sp` | `rr-to-sp.json` | 1 |
| `large-tree-rr-to-sp` | `large-tree-rr-to-sp.json` | 1 |
| `rr-to-sp-stop-the-world-pop` | `rr-to-sp-stop-the-world-pop.json` | 1 |
| `motivating-example` | `motivating-example/` | 4 |
| `designated-survivor` | `designated-survivor/` | 15 |
| `multi-edit` | `multi-edit/` | 6 |
| `scalability` | `scalability/` | 40 |

Preview the commands or select suites:

```sh
.venv/bin/python run_experiments.py --dry-run
.venv/bin/python run_experiments.py --experiments motivating-example scalability
```

The per-suite READMEs describe individual runs, measurements, and rendering from
existing CSVs. Generated figure folders include `plot.py` and local CSVs; copy a
folder elsewhere and run `python plot.py` to redraw it with only Matplotlib.

Experiment verification runs as part of the suite runners. The separate
[Python core tests](../hw/python/README.md#core-tests) and
[hardware tests](../hw/spinal/README.md#core-hardware-tests) use small inputs;
they do not run experiments or inspect plots.

The canonical outputs are those produced by the current source inputs. Obsolete
root-level `rr-to-sp-*.svg/png` archives, including the older 160-packet RR/SP
dataset, are removed: the current RR/SP JSON specifies 480 packets. Those
historical plots and hand-written result summaries are not reproducibility
targets; the current runners generate figures and validation reports directly
from their new traces.

## Individual runs and figures

Run these commands from `pifo-hardware/`. Compiler input formats live in the
[Python README](../hw/python/README.md); simulator output fields are described in
the [simulator guide](../hw/spinal/rio/sim/README.md#reconfiguration-timestamps-and-drain-time).

The checked example starts with RR and changes to SP:

```bash
python3 hw/python/pifo_experiment_figures.py validate experiments/rr-to-sp.json
python3 hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp.json
```

The evaluation-only stop-the-world comparison uses the same interface:

```bash
python3 hw/python/pifo_experiment_figures.py validate experiments/rr-to-sp-stop-the-world-pop.json
python3 hw/python/pifo_experiment_figures.py run --config experiments/rr-to-sp-stop-the-world-pop.json
```

The output directory exposes every boundary: `tree-move.json`, `traffic.json`, compiled `transactions.txt`, request and
completion CSVs, the complete `packet-outcomes.csv`, and `reconfiguration-events.csv`. Each figure owns a separate artifact directory:

- `figures/bandwidth/{data.csv,packets.csv,plot.py,figure.svg,figure.png}`
- `figures/packet-scatter/{data.csv,packets.csv,plot.py,figure.svg,figure.png}`

Matplotlib is preferred; SVG plus FFmpeg is used automatically when Matplotlib is unavailable. The scatter uses one
shared 1:1 range for its input/output axes, keeps `y = x` at 45 degrees, and draws start, commit, old-tree-drain, and
stop-the-world resume lines on both axes.

Install the plotting dependency for the motivating-example delay plots in an isolated environment:

```bash
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

### Four-run motivating example

The checked-in p1 and p2 trees give every flow a distinct hardware FIFO leaf.
The runner explicitly selects the merged evaluation top level and Verilator,
matching the newer explicit-leaf evaluations, and uses a per-flow packet queue
depth of 4096. Run all four cases in one invocation when quoting comparisons so
the report cannot mix old and new topology artifacts.

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
`flow,push_cycle,pop_cycle,dropped` (plus request ID and size), where `push_cycle` is the source-generation cycle,
`reconfiguration-events.csv`, and:

- `figures/throughput/{data.csv,packets.csv,plot.py,figure.svg,figure.png}`
- `figures/delay-scatter/{data.csv,packets.csv,plot.py,figure.svg,figure.png}`

The combined outputs are `comparisons/r2-r4-delay-scatter` and `comparisons/r3-r4-throughput`. Use `--render-only`
on any case or the all-case script to regenerate figures without rerunning RTL. The all-case validator checks identical
input traces, generation-time packet timestamps, losslessness and per-flow FIFO order for all four runs, R2's minimum
stop interval, outage delay, and peak buffer occupancy, plus the R3/R4 drain ordering and R3's whole-tree zoom delay
spike relative to R4.

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

## Combined experiment JSON

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

Move modes and their restrictions are documented in the
[compiler guide](../hw/python/README.md#move-modes).

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

See the [traffic JSON reference](../hw/python/README.md#traffic-json) for rate
and size distributions.

## Large-tree phase verification

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
4. Old packets still in downstream FIFOs may complete after root drain. New-tree output starts only after the last
   old packet completes, and SP priority order has no reversal.

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
writes, 1 front underflow rewrite, and 1 commit. The verifier requires at least 24 old packets pending at publication
and 200 cycles from commit acceptance to root drain. These bounds reflect the corrected dequeue timing and
immediate mapper replay; the former 32-packet/800-cycle bounds belonged to an older simulator.
The workload itself is unchanged. Fresh measurements and both root-drain and final-old-packet timestamps are
written to the generated verification report. A packet admitted on the publication edge is classified as old,
matching the mapper-bank contract.
