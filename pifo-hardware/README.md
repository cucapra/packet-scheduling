# Runtime-reconfigurable PIFO hardware

## Project structure

| Path | Contents |
| --- | --- |
| [hw/spinal/](hw/spinal/README.md) | SpinalHDL hardware and hardware tests |
| [hw/spinal/rio/sim/](hw/spinal/rio/sim/README.md) | Simulator interfaces, commands, and output formats |
| [hw/verilog/](hw/verilog/) | Verilog modules and testbenches |
| [hw/python/](hw/python/README.md) | Compiler, input formats, simulator tools, plotting, and core tests |
| [experiments/](experiments/README.md) | Experiment source JSONs and reproduction guide |
| [run_experiments.py](run_experiments.py) | Runner for all experiment suites |

## Run experiments

Install Python 3.10+, JDK 17, sbt, Icarus Verilog, Verilator, and a C++ toolchain.
Select JDK 17 with `JAVA_HOME` and `PATH`, then run from `pifo-hardware/`:

```bash
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
.venv/bin/python run_experiments.py
```

Preview commands or select suites:

```bash
.venv/bin/python run_experiments.py --dry-run
.venv/bin/python run_experiments.py --experiments motivating-example scalability
```

The full run covers seven suites (68 RTL runs). Generated instructions, traces,
reports, and figures go under the ignored `experiment-results/` directory.
See the [experiment guide](experiments/README.md) for suite details and resource settings.
