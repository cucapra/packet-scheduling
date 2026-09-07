# Synthesis milestone evidence

Measured September 6, 2026. All completed runs cover 2 PEs, 32 global flow IDs,
32 virtual-PIFO IDs per PE, 1,024 shared entries per PE, and 8-bit ranks. The
existing mesh configuration/commit controller is included. These are synthesis
results; placement, routing, and timing closure were not run.

| Completed run | Tool and target | Result | Evidence |
|---|---|---|---|
| House PIFO, Quartus | Pro 25.3.1 Build 100; Agilex 7 AGFB014R24B2E2V | 90,077 estimated ALMs; 141,022 ALUTs; 42,356 registers; 297,440 block-memory bits | [Report](../RESULTS.md), [raw report](baseline/quartus-synthesis.rpt), [manifest](baseline/manifest.json) |
| House PIFO, Vivado | 2025.2 Build 6299465; KCU116 xcku5p-ffvb676-2-e; RuntimeOptimized | 198,425 LUTs; 42,085 FFs; 8 BRAM36; 2 URAM | [Report](../VIVADO_RESULTS.md), [utilization](baseline-vivado-runtime/reports/utilization.rpt), [manifest](baseline-vivado-runtime/manifest.json) |
| Stock PIFO + adapter, Quartus | Same Agilex 7 tool/target/settings | 71,478 estimated ALMs; 98,332 ALUTs; 43,176 registers; 297,440 block-memory bits | [Report](../STOCK_PIFO_RESULTS.md), [raw report](stock-pifo/pifo.syn.rpt), [comparison](stock-pifo/comparison.json) |

The house Quartus and Vivado measurements used identical generated RTL and
initialization files. Intel and AMD resource units are not interchangeable.
The current house backend passes [36,873 contract-test cycles](stock-pifo/house-validation/validation.json).
The stock backend [fails a wrong-port consecutive-pop test](stock-pifo/stock-validation/validation.json)
and remains experimental. **There is no completed Vivado stock-PIFO synthesis.**

## Setup and reproduction

The [workflow guide](../README.md) records discovery, licenses, sizes, and commands.
[`run.py`](../run.py), [`create_project.tcl`](../create_project.tcl), and
[`vivado_synth.tcl`](../vivado_synth.tcl) implement the saved synthesis flows.

- Quartus: `/data/work/quartus/quartus`; installed Agilex F-Series development-kit
  definition and BTS reference; working license path
  `/data/work/quartus/licenses/LR-187458_License.dat`. Balanced synthesis,
  100 MHz target, eight requested processors, virtual data pins.
- Vivado: `/data/work/vivado/2025.2/Vivado`; installed KCU116 board XML version 1.5
  and device files; successful Standard Edition synthesis checkout. The measured
  run used `out_of_context`, `flatten_hierarchy rebuilt`, `RuntimeOptimized`,
  100 MHz, and eight configured general threads.
- RTL generation: Java 21 and sbt 1.10.2, with the launcher/dependencies resolved
  from Maven Central. The launcher, tool databases, RTL build outputs, and
  synthesized checkpoints remain in ignored directories. License contents are
  not included in this milestone.

Each completed archive retains the tool version, manifest, console output,
resource report, and structured summary. Manifests record the parameters and
source/RTL hashes as measured. Quartus project/SDC and preparation logs are
preserved for both completed runs; the Vivado archive includes its XDC and
the synthesis commands in its complete console log. Earlier house manifests predate the backend
selector; regenerate before using the current `--reuse-rtl`/`--rtl-from` checks.
The house interface refactor preserved the datapath: [comparison evidence](stock-pifo/house-validation/baseline-equivalence.json).

## Diagnostic attempts

These directories are troubleshooting evidence, not successful measurements:

- `expired-license-attempt/`: earlier Quartus failures before the working license.
- `vcu118-license-attempt/`: Vivado device required an unavailable synthesis license.
- `zcu106-uninstalled-attempt/`: board definition existed, but device files were missing.
- `vivado-default-cancelled/`: default optimization stopped after 28:31 once the
  completed RuntimeOptimized result was verified.
- `vivado-environment/`: installed-device inventory and detected version.

The stock archive also preserves both the stock failure and house passing XSim
logs. Declaration reordering was applied only to the stock simulation copy;
Quartus synthesized the original `hw/verilog/pifo.sv` unchanged.
