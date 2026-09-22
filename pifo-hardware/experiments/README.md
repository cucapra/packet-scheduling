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

Run the source tests without any simulation results:

```sh
.venv/bin/python -m unittest discover -s hw/python/tests
```

After regenerating all suites, also check full packet traces, comparison data,
commit markers, and standalone plotters against the generated results:

```sh
PIFO_EXPERIMENT_RESULTS=1 .venv/bin/python -m unittest discover -s hw/python/tests
```

The canonical outputs are those produced by the current source inputs. Obsolete
root-level `rr-to-sp-*.svg/png` archives, including the older 160-packet RR/SP
dataset, are removed: the current RR/SP JSON specifies 480 packets. Those
historical plots and hand-written result summaries are not reproducibility
targets; the current runners generate figures and validation reports directly
from their new traces.
