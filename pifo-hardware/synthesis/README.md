# PIFO resource estimation

`run.py` elaborates the existing **complete PifoMesh** and runs Quartus or Vivado
**synthesis only**. All packet and configuration ports remain runtime inputs.
The result includes every PE, sorted PIFO, brain, mapper, front rewrite table,
crossbar, and the existing configuration/commit controller.

The [baseline report](RESULTS.md) records a successful Agilex 7 synthesis:
90,077 estimated ALMs, 42,356 registers, and 297,440 block-memory bits. It also
explains the component breakdown and the limits of synthesis-only estimates.
The [Vivado report](VIVADO_RESULTS.md) records the same RTL on KCU116:
198,425 LUTs, 42,085 FFs, 8 BRAM36, and 2 URAM with `RuntimeOptimized`.
The [stock-PIFO experiment](STOCK_PIFO_RESULTS.md) measures 71,478 ALMs on
Agilex 7, a 20.65% reduction. The stock core fails a consecutive-pop ordering
check, so this remains an experimental backend; `house` is the default.
The [milestone evidence index](results/README.md) links both tool setups,
completed results, functional validation, and separate diagnostic attempts.

## Reproduce the baseline

From `pifo-hardware`:

```bash
# Agilex 7, using the working local Intel license.
python3 synthesis/run.py --tool quartus \
  --license /data/work/quartus/licenses/LR-187458_License.dat

# Reproduce the measured Kintex UltraScale+ pass using the identical RTL.
python3 synthesis/run.py --tool vivado --name baseline-vivado-runtime \
  --rtl-from synthesis/build/baseline --vivado-directive RuntimeOptimized

# Or generate and synthesize directly in Vivado without requiring Quartus.
python3 synthesis/run.py --tool vivado --name baseline-vivado-runtime \
  --vivado-directive RuntimeOptimized
```

Defaults are 2 PEs, 32 vPIFO IDs per PE, 1,024 shared sorted entries per PE,
32 global flow IDs, 8-bit ranks, and a 100 MHz synthesis constraint. The clock
constraint is a target; this flow makes no timing-closure claim.

The default build names are `baseline` for Quartus and `baseline-vivado` for
Vivado. Use a new `--name` to retain an earlier working build. An invocation
updates that build's generated files and reports; preserved snapshots are in
`results/`.

Use `--pifo-backend stock --name stock-pifo` for the repository's original
`hw/verilog/pifo.sv` plus the PE empty/drain adapter. With `stock`, the default
names become `stock-pifo` and `stock-pifo-vivado`. This option does not change
the number of PEs, entries, virtual PIFOs, or flow IDs. See the experiment report
before interpreting these counts as equivalent hardware.

`--rtl-from BUILD_DIR` checks hardware parameters, current source hashes, and
every generated RTL/memory-initialization hash before copying the RTL into a
separate build. `--reuse-rtl` instead checks and reuses the current build's RTL.
Both reject incomplete generation, changed sources, changed generated files,
and size mismatches. When reusing a non-default size, repeat its size options.
Also repeat its `--pifo-backend`. The backend is part of the manifest, and the
selected external RTL is copied into `rtl/` and hashed. Old pre-backend build
manifests must be regenerated before using the current reuse checks; archived
results remain intact.

The script uses Java and downloads the sbt launcher version pinned in
`project/build.properties` into the ignored `synthesis/.tools/` cache if needed.
The project dependencies are resolved from Maven Central. Reusing RTL skips
Java/sbt and requires only Python 3 and the selected FPGA tool.

## Quartus environment

Quartus is found through `--quartus-root`, `QUARTUS_ROOTDIR`, `PATH`, or the shared
`/data/work/quartus/quartus` installation. The target part is read from the
installed Agilex F-Series development-kit definition. `--part` can override it.
License selection uses `--license`, existing `LM_LICENSE_FILE` or
`ALTERAD_LICENSE_FILE`, then `.dat` files in the installation's `Intel_lic/` and
`licenses/` directories. License contents are never copied into the project.

Only `quartus_syn` is run after project preparation. The QSF treats data ports
as virtual pins and reads the encoder as SystemVerilog despite its `.v`
extension. The board definition supplies the FPGA part; the board test system,
physical packet interfaces, and board I/O timing are outside this core estimate.

## Vivado environment

Vivado is found through `--vivado-root`, `XILINX_VIVADO`, `PATH`, or versioned
installations under `/data/work/vivado`, `/opt/Xilinx/Vivado`, and
`/tools/Xilinx/Vivado`. This machine has **Vivado 2025.2** at
`/data/work/vivado/2025.2/Vivado`.

The default target is read from the newest installed **KCU116** board XML:
**`xcku5p-ffvb676-2-e`**. Use `--board NAME` for another installed board or
`--part PART` for an explicit device. The Tcl flow verifies that the device
itself is installed; board metadata alone does not guarantee device support.

This part is supported by the license-free Standard Edition of Vivado 2025.2,
as documented in [AMD UG973, supported devices](https://docs.amd.com/r/2025.2-English/ug973-vivado-release-notes-install-license/Supported-Devices).
The initial VCU118 attempt required an unavailable synthesis license; ZCU106
board metadata was present but its Zynq device files were not installed.
KCU116 was selected after probing the actual installed devices. These findings
apply to the detected **2025.2** installation, not every Vivado release.

Vivado keeps its normal license search behavior. An optional `--license`
sets `XILINXD_LICENSE_FILE` for that invocation. The Intel license is for
Quartus; Vivado's default flow does not search the Intel license directories.

`vivado_synth.tcl` reads the same Verilog, encoder, and initialization files,
then runs:

```tcl
synth_design -top PifoMesh -part $part -mode out_of_context -flatten_hierarchy rebuilt
```

It creates a 100 MHz clock constraint by default and marks reset as a false
path. Out-of-context synthesis omits top-level I/O buffers. All scheduler
runtime inputs remain exposed. No `opt_design`, placement, routing, or
bitstream generation is invoked. Vivado's general thread limit is capped at
8; the actual parallelism depends on the synthesis stage.

The default synthesis directive is `default`. For a faster resource estimate,
the runner also exposes `--vivado-directive RuntimeOptimized`. AMD documents
that this performs fewer timing/RTL optimizations, so its mapped resource
counts can differ from the default pass. The chosen directive is recorded in
the manifest. See [AMD UG901, synthesis settings](https://docs.amd.com/r/2025.2-English/ug901-vivado-synthesis/Using-Synthesis-Settings).
The measured RuntimeOptimized pass completed in about 7.5 minutes. The default
pass was still in timing optimization when stopped after 28.5 minutes; it has
no completed resource report.

```bash
python3 synthesis/run.py --tool vivado --name baseline-vivado-runtime \
  --rtl-from synthesis/build/baseline --vivado-directive RuntimeOptimized
```

To inventory the installed parts independently, run from a temporary/build
directory so Vivado's working files stay there:

```bash
mkdir -p synthesis/build/device-probe
cd synthesis/build/device-probe
/data/work/vivado/2025.2/Vivado/bin/vivado -mode batch -nojournal \
  -source ../../probe_vivado.tcl -tclargs installed-parts.tsv
cd ../../..
```

## Hardware sizes and preparation

```bash
# Generate RTL/QSF/SDC and the size manifest without a Quartus license checkout.
python3 synthesis/run.py --prepare-only

# Reuse verified completed RTL with the working license on this machine.
python3 synthesis/run.py --reuse-rtl \
  --license /data/work/quartus/licenses/LR-187458_License.dat

# A different fixed hardware size; storage is shared across vPIFOs within each PE.
python3 synthesis/run.py --name pe4-v32-c1024 --engines 4 \
  --vpifos 32 --entries-per-pe 1024 --priority-bits 16

# Prepare Vivado RTL and XDC, validating the installed part without synthesis.
python3 synthesis/run.py --tool vivado --name prepare-vivado \
  --rtl-from synthesis/build/baseline --prepare-only
```

`--entries-per-pe` must be a power of two and divisible by `--vpifos`: the existing RTL derives
capacity as `numVPIFOs * fifoDepth`. The two are exposed separately here without
changing the hardware. Global flow-ID capacity and vPIFO-ID capacity are still
coupled in `PifoMessage`; a smaller traffic trace does not reduce table hardware.

Each build is placed in `synthesis/build/NAME/`. The useful outputs are:

- `manifest.json`: hardware sizes, source/generated-file hashes, board and tool
  paths, license search path, and completion status.
- `rtl/`: generated Verilog and memory initialization files.
- `generate.log` (when elaborated), `synthesis.log`: stage-specific output.
- `quartus-version.txt` or `vivado-version.txt`: the detected tool build.
- `pifo.qsf`, `pifo.sdc`, `project.log`: Quartus project preparation.
- `output_files/pifo.syn.rpt`: Quartus synthesis report. Check its success status
  before using any resource numbers; a failed run is not an area measurement.
- `pifo.xdc`, `pifo_synth.dcp`, `vivado.log`: Vivado constraint, synthesized
  checkpoint, and tool log. The checkpoint is left in the ignored build tree.
- `reports/utilization.rpt`, `reports/utilization-hierarchy.rpt`,
  `reports/ram-utilization.rpt`, `reports/primitive-counts.tsv`: Vivado resource
  reports. `synthesis_complete.txt` is written only after synthesis, all reports,
  and a no-blackbox check succeed.

Extract resource and hierarchy tables as JSON:

```bash
python3 synthesis/summarize_quartus.py \
  synthesis/build/baseline/output_files/pifo.syn.rpt \
  synthesis/build/baseline/quartus-summary.json

python3 synthesis/summarize_vivado.py \
  synthesis/build/baseline-vivado-runtime \
  synthesis/build/baseline-vivado-runtime/vivado-summary.json
```

The extractors reject failed/incomplete synthesis. Quartus's per-entity report uses
ALUTs and registers; ALMs are estimated for the complete design. RAMs may still
have type `AUTO`, so block-memory bits are not a final physical M20K count.

Vivado reports LUTs, flip-flops, distributed RAM, RAMB18/RAMB36, and DSP
primitives. These are synthesis mappings. LUT counts and Intel ALMs use
different resource units, so do not compare them as interchangeable quantities.

## Generic RTL audit

When the Quartus license is unavailable, Yosys can independently check the
generated RTL and count explicit flip-flops and logical memories. This does
**not** estimate Agilex ALM packing, M20K allocation, or timing. The audit flattens
the mesh before optimization, so constants such as the disabled second push
port propagate through the PE hierarchy. Memories remain unmapped.

With Yosys on `PATH`, after generating the baseline:

```bash
cd synthesis/build/baseline/rtl
yosys -Q -T -s ../../../audit.ys > ../generic-audit.log 2>&1
cd ../../../..
python3 synthesis/summarize_audit.py \
  synthesis/build/baseline/generic-netlist.json \
  synthesis/build/baseline/generic-summary.json
```

On this machine Yosys 0.33 was extracted locally from Ubuntu's `yosys` package,
without a system install. Its executable is
`synthesis/.tools/yosys-root/usr/bin/yosys`; from the `rtl` directory above use
`../../../.tools/yosys-root/usr/bin/yosys` in place of `yosys`.

The audit runs `check -assert` and rejects remaining module blackboxes or latches
when summarized. It is structural validation, not a functional packet-scheduling
regression. That generic audit is for the house backend. For either backend's
generated PIFO interface, `python3 synthesis/validate_pifo.py BUILD_DIR` uses
the installed Vivado XSim to check ordering, concurrency, overflow, and empty/
drain signals against a queue model. It requires a completion marker and returns
nonzero on the known stock-core failure; synthesis success is tracked separately.
