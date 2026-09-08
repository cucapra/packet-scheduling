# PIFO resource estimation

`run.py` elaborates the existing **complete PifoMesh** and runs Quartus or Vivado
**synthesis only**. All packet and configuration ports remain runtime inputs.
The result includes every PE, sorted PIFO, brain, mapper, front rewrite table,
crossbar, and the existing configuration/commit controller.

The archived read/copy [baseline report](RESULTS.md) records a successful Agilex 7 synthesis:
90,077 estimated ALMs, 42,356 registers, and 297,440 block-memory bits. It also
explains the component breakdown and the limits of synthesis-only estimates.
The [Vivado report](VIVADO_RESULTS.md) records the same RTL on KCU116:
198,425 LUTs, 42,085 FFs, 8 BRAM36, and 2 URAM with `RuntimeOptimized`.
The [stock-PIFO experiment](STOCK_PIFO_RESULTS.md) measures 71,478 ALMs on
Agilex 7, a 20.65% reduction. The stock core fails a consecutive-pop ordering
check, so this remains an experimental backend; `house` is the default.
The [milestone evidence index](results/README.md) links both tool setups,
completed results, functional validation, and separate diagnostic attempts.
The [hardware-overhead experiments](../experiments/hardware-overhead/README.md)
use `--configuration static` to replace atomic mapper banks with ordinary tables
and ignore commits, then compare that baseline with controller replay (`--configuration replay`, now the default).
`--build-root` places the large synthesis sweep on a chosen work filesystem.

Use `--pifo-backend external` to measure RIO without PIFO cores. All PIFO request,
response, empty, and drain signals are exposed at the top level; surrounding
RIO logic remains connected to runtime ports. This scope excludes sorter,
entry storage, occupancy, and drain detection and requires a separate PIFO
budget for any combined estimate. The hardware-overhead experiments now use
this boundary, with earlier whole-mesh evidence retained separately. The primary resource totals and percentage denominator add five separately measured matching house PIFOs back to the RIO-only counts.

`--configuration replay --control-queue-depth 4` is the default. Commands remain
in the existing control FIFO and are read a second time after commit; there is
no separate replay journal. Each mapper bank retains one read and one write
port. The FIFO depth is shared with the ordinary baseline and reserves one
entry for commit, limiting retained epochs to depth minus one commands. See
the [replay protocol](../experiments/hardware-overhead/REPLAY.md) for credit and
driver rules. The old `--replay-log-depth` and journal-placement options have
been removed from this runner. The `dynamic` read/copy option remains for
historical reproduction. Saved resource figures at `d5a10e8` measured the
separate-journal replay implementation; shared FIFO replay has correctness
validation only so far.

Use `--generate-only` to elaborate RTL and save its manifest without creating
a vendor project or running synthesis. Then `validate_replay.py BUILD` runs
XSim and checks the generated RAM declarations/ports for a 2-PE, 8-ID external
PIFO setup. The [protocol guide](../experiments/hardware-overhead/REPLAY.md#correctness-validation-without-synthesis)
has complete commands, including the full packet and driver regressions.

## Run the default replay design

From `pifo-hardware`:

```bash
# Agilex 7, using the working local Intel license.
python3 synthesis/run.py --tool quartus \
  --license /data/work/quartus/licenses/LR-187458_License.dat

# Synthesize the same replay RTL on Kintex UltraScale+.
python3 synthesis/run.py --tool vivado --name baseline-replay-vivado-runtime \
  --rtl-from synthesis/build/baseline-replay --vivado-directive RuntimeOptimized

# Or generate and synthesize directly in Vivado without requiring Quartus.
python3 synthesis/run.py --tool vivado --name baseline-replay-vivado-runtime \
  --vivado-directive RuntimeOptimized
```

Defaults are 2 PEs, 32 vPIFO IDs per PE, 1,024 shared sorted entries per PE,
32 global flow IDs, 8-bit ranks, and a 100 MHz synthesis constraint. The clock
constraint is a target; this flow makes no timing-closure claim.

The default replay build names are `baseline-replay` for Quartus and `baseline-replay-vivado` for
Vivado. Use a new `--name` to retain an earlier working build. An invocation
updates that build's generated files and reports; preserved snapshots are in
`results/`.

Use `--pifo-backend stock --name stock-pifo` for the repository's original
`hw/verilog/pifo.sv` plus the PE empty/drain adapter. With `stock`, the default
replay names become `stock-pifo-replay` and `stock-pifo-replay-vivado`. This option does not change
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

`--quartus-compact-init` creates a `quartus-rtl/` view containing equivalent
`ram_init_file` attributes and zero-filled MIF ranges, avoiding long frontend
processing of millions of literal initialization words. The converter verifies
the entire binary contents and dimensions, rejects unsupported/nonzero files,
and leaves canonical `rtl/` unchanged. `quartus-initialization.json` records
input and derived hashes. The hardware-overhead control experiment produced
identical resource counts with and without this option.

`probe_shared_init.py SOURCE_BUILD NEW_BUILD` optionally reuses identical
zero-filled MIF files across equal RAM geometries in a separate Quartus build.
The [three 128-ID controls](../experiment-results/hardware-overhead/diagnostics/shared-initialization/README.md)
retained identical resources and had shorter synthesis times. The main
experiments keep their original initialization workflow.

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

`--vivado-allow-over-capacity` is an explicit estimation-only option for designs
that exceed the selected device. It replaces the process-local
`rt::check_resource` pre-mapping gate through `synth.elaboration.rodinMoreOptions`;
the hook is idempotent because Vivado can evaluate it more than once. It changes
no installed files, RTL, optimization passes, device capacities, or licensing.
This version-specific workaround was checked on Vivado 2025.2: a smaller RAM
control had identical utilization tables with and without it, while an oversized
RAM completed at 7,424 BRAM36 tiles against the target's 480 available tiles.
See `experiment-results/hardware-overhead/diagnostics/vivado-resource-limit-probes/`.
Oversized results cannot establish fit or timing, and no implementation follows.

```bash
python3 synthesis/run.py --tool vivado --name baseline-replay-vivado-runtime \
  --rtl-from synthesis/build/baseline-replay --vivado-directive RuntimeOptimized
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
  --rtl-from synthesis/build/baseline-replay --prepare-only
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
  synthesis/build/baseline-replay/output_files/pifo.syn.rpt \
  synthesis/build/baseline-replay/quartus-summary.json

python3 synthesis/summarize_vivado.py \
  synthesis/build/baseline-replay-vivado-runtime \
  synthesis/build/baseline-replay-vivado-runtime/vivado-summary.json
```

The extractors reject failed/incomplete synthesis. Quartus's per-entity report uses
ALUTs and registers; ALMs are estimated for the complete design. RAMs may still
have type `AUTO`, so block-memory bits are not a final physical M20K count.
For a RAM-free PIFO, Quartus omits the memory summary rows. The reader records
zero only after verifying zero memory in every hierarchy entity and no inferred
RAM instances. `python3 synthesis/test_summarize_quartus.py` checks this case,
unknown memory, the historical overflow case, and ordinary reported RAM totals.

Vivado reports LUTs, flip-flops, distributed RAM, RAMB18/RAMB36, and DSP
primitives. These are synthesis mappings. LUT counts and Intel ALMs use
different resource units, so do not compare them as interchangeable quantities.

## Large Quartus memory totals

The completed five-PE, 1,024-ID read/copy run omits the aggregate memory rows
and reports `-2147483648` for the root hierarchy's block-memory total. Its
individual RAM sizes remain positive. The report reader recovers
**2,516,583,192 bits** by summing `Implementation Bits` and independently
checking the sum of disjoint direct-child `Block Memory Bits` values. It keeps
the raw report unchanged and records the recovery in `resource-summary.json`.
The omitted MLAB count remains unreported; it is never replaced with zero.
The [validation record](../experiment-results/hardware-overhead/diagnostics/quartus-memory-total-overflow/validation.json)
checks all 14 Quartus cases completed when the issue was found and rejects
inconsistent detailed totals. The original synthesis completed successfully;
this is a report-accounting workaround and requires no synthesis rerun.

## Historical replay journal placement

This section and its diagnostic scripts apply to the archived separate-journal
RTL at `d5a10e8`. The current shared FIFO implementation has no journal to place.

The replay configuration uses two simple-dual-port mapper banks and records
configuration writes in a shared controller FIFO. In the original 1,024-ID
Quartus run, the mapper banks infer correctly but the journal maps to 655,360
storage registers. The equivalent isolated journal infers block RAM, so the
full-design result must be checked rather than inferred from an isolated test.

`probe_replay_journal.py SOURCE_BUILD FRESH_BUILD --force-m20k` resynthesizes
the unchanged core with `RAMSTYLE_ATTRIBUTE M20K` scoped to the journal array.
It reuses verified RTL/MIF inputs and preserves the source result. Add
`--isolated` for the extracted-journal control. The
[experiment setup and evidence](../experiment-results/hardware-overhead/r4-replay/journal-m20k/README.md)
record both automatic and explicit placement. This is a Quartus project
assignment; the canonical RTL and Vivado setup remain the original R4 inputs.

Use `check_replay_mapping.py BUILD OUTPUT_JSON --require-log-ram` to require
two post-mapper banks per PE, no copy-read replicas, and a simple-dual-port RAM
journal in a completed Quartus report. Without the final flag, the checker
also accepts an explicitly evidenced logic/register journal and records that
implementation instead of interpreting its zero block-memory count as absence.

## Generic RTL audit

When the Quartus license is unavailable, Yosys can independently check the
generated RTL and count explicit flip-flops and logical memories. This does
**not** estimate Agilex ALM packing, M20K allocation, or timing. The audit flattens
the mesh before optimization, so constants such as the disabled second push
port propagate through the PE hierarchy. Memories remain unmapped.

With Yosys on `PATH`, after generating the baseline:

```bash
cd synthesis/build/baseline-replay/rtl
yosys -Q -T -s ../../../audit.ys > ../generic-audit.log 2>&1
cd ../../../..
python3 synthesis/summarize_audit.py \
  synthesis/build/baseline-replay/generic-netlist.json \
  synthesis/build/baseline-replay/generic-summary.json
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
