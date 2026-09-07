# Dynamic configuration hardware overhead

These experiments compare RIO logic with two configuration implementations.
The primary R1/R2 runs **exclude the PIFO cores**, following the requested scope
change. They measure overhead rather than assuming it is small.

- **Static / ordinary tables:** one synchronous RAM per pre/post lookup table;
  configuration writes take effect immediately. The command ingress FIFO,
  decoder, per-engine routing, and ordinary configuration handlers remain.
  Commit messages consume their ingress slot and have no state effect. There
  is no bank swap, bank synchronization, commit backpressure, or commit-armed
  drain rewrite in the live hardware.
- **RIO dynamic:** the existing transactional double-bank tables, synchronized
  publication, bank copying, and drain-triggered front rewrites.

Both use the same rank width, PE count, control widths, and runtime ports.
`--pifo-backend external` exposes each core's pushes, pop request, pop response,
empty indication, and drain event as top-level ports. These are wiring boundaries,
with unconstrained response inputs and observable request outputs. There is no
constant-output PIFO stub, sorter, storage, occupancy counter, or drain detector
in the measured netlist. The controller, lookup tables, brain, PE stream logic,
and crossbar remain. Synthesis is allowed to prune unused signals normally.

The 1,024-entry PIFO capacity is retained as an integration/budget parameter;
it contributes **no PIFO storage or sorter hardware** to these isolated counts.
The stock PIFO remains outside the comparison because of its recorded ordering
defect. Earlier complete whole-mesh measurements and interrupted attempts are
preserved under `experiment-results/hardware-overhead/whole-mesh/`, with the
corresponding `whole-mesh-*.json` reference cases and source snapshot.

## Experiment list

1. **R1: fixed hardware comparison.** Five PEs, 1,024 vFlow IDs, 1,024 shared PIFO
   entries per PE reserved for the later PIFO budget, eight-bit ranks. Compare ordinary tables with RIO dynamic on
   Quartus/Agilex 7 and Vivado/Kintex UltraScale+. Report each platform's native
   resource units, absolute difference, and percentage difference.
2. **R2: vFlow sweep.** Hold five PEs and 1,024 entries per PE fixed; sweep
   32, 64, 128, 256, 512, and 1,024 IDs. Plot logic, registers, memory, and
   dynamic-over-ordinary percentage separately for the two platforms.
3. **R3: isolated lookup pipeline exploration.** Compare one- and eight-cycle
   reads for one ordinary 131,072-by-10-bit lookup bank, matching one post-mapper
   bank's geometry at 128 IDs. Keep an initiation interval of one cycle and
   measure both tools. This is a separate component experiment; its added
   latency is not integrated into the RIO mesh used for R1/R2.
4. **R4: controller instruction replay.** Replace table scanning/copy reads with
   a shared update log, then replay into the shadow bank after each swap. Compare
   32, 128, and 1,024 IDs against both prior implementations on both vendors.
   See the [replay protocol, capacity, validation, and workflow](REPLAY.md).

For every resource, `absolute_change = dynamic - static` and
`percent_change = 100 * absolute_change / static`. A zero denominator is `N/A`.
Failed or incomplete synthesis is not a zero-cost result. The configuration
command path is included in both versions. Percentages use **ordinary RIO logic
without PIFO** as denominator; they are not whole-scheduler overhead percentages.

## Adding a PIFO budget later

For each vendor resource separately, a later component estimate can use
`total_static ≈ rio_static + pifo_static` and
`total_dynamic ≈ rio_dynamic + pifo_dynamic`. If both variants use the same PIFO
macro of cost `P`, the absolute difference remains `rio_dynamic - rio_static`,
and the combined overhead is
`100 * (rio_dynamic - rio_static) / (rio_static + P)`. A common PIFO cost lowers
the percentage but does not remove the additional mapper memory.

Measure that macro with all interfaces above exposed and the same part,
capacity, widths, clock, and synthesis settings. Include the empty/drain adapter
if the selected PIFO does not provide it. If the variants use different adapters,
budget them separately. Sums of isolated synthesis counts are approximate because
whole-design synthesis can optimize across the boundary. ALMs and LUTs are not
interchangeable; use the native units of each vendor.

For five PEs and 1,024 entries per PE, the raw sorted-entry payload is
`5 * 1024 * (log2(vflows) + 8 + log2(vflows) + 3)` bits: 107,520 bits at 32 IDs
and 158,720 bits at 1,024 IDs. This is a logical storage budget, **not an FPGA
resource estimate**: it excludes comparators, shifting, valid/occupancy state,
priority encoding, drain detection, and mapping overhead. The archived
`pifo-storage-budget.csv` records this calculation. Actual PIFO ALM/LUT/FF/RAM
cost is deferred; it is never substituted with zero in a combined estimate.

`pifo-residual-diagnostic.csv` also subtracts the isolated RIO counts from the
preserved whole-mesh counts at 32 IDs. This residual includes optimization
changes across the boundary, so it is evidence of PIFO's dominant logic cost
at that point, not a standalone PIFO measurement or a model for 1,024 IDs.

## Namespace and device capacity

In the current implementation vFlow/global-flow capacity is coupled to the
virtual-PIFO ID capacity. The experiment retains this definition. `fifoDepth`
is adjusted as `1024 / vflows` so shared PIFO space remains constant.
The synthesized core supports the entire encoded ID range, including the
all-ones ID checked by the interface test below. The traffic simulator's
separate empty-token reservation is outside this synthesis scope.

With five PEs, token width is `log2(vflows) + 3`, and the flow-state/post-mapper
address depth per PE is `8 * vflows^2`. At 1,024 IDs this means **8,388,608 words
per PE**. Declared main tables require 1,887,436,800 bits with ordinary tables
and 2,432,696,320 bits with dynamic tables, before width pruning, read-port
replication, or FPGA mapping. This sizing issue is separate from atomicity.

The requested point is a legal RTL configuration, but a successful synthesis
alone must not be described as a physical fit on the reference devices. Results
must identify capacity limits; a compact association table or independent
flow/vPIFO namespaces would be a separate architecture experiment applied to
both configurations, not a silent reduction of the requested size.

Quartus uses AGFB014R24B2E2V, balanced synthesis and virtual data pins. Vivado uses
xcku5p-ffvb676-2-e, out-of-context synthesis, rebuilt hierarchy, and
`RuntimeOptimized`. Both use a 100 MHz constraint and eight configured threads.
Neither flow runs implementation. Large Vivado estimates explicitly skip its
pre-mapping device-capacity gate using the documented local workflow option;
this does not establish a device fit. The version-specific Tcl hook was checked
against an unchanged smaller RAM control, and an oversized RAM probe completed.
The target's reported capacities remain the physical limits.
The prior [synthesis setup](../../synthesis/README.md)
records the tool discovery, board references, and license details.

## Run and reproduce figures

From `pifo-hardware`:

```bash
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt

.venv/bin/python hw/python/pifo_hardware_overhead_r1.py
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py

# On this machine, use the larger data filesystem for tool databases and RTL.
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py \
  --build-root /data/work/rio-synthesis/hardware-overhead --jobs 2

# Regenerate figures solely from archived measured CSV data.
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py --render-only

# Optional component exploration, with simulation followed by both vendors.
.venv/bin/python hw/python/pifo_lookup_pipeline.py \
  --build-root /data/work/rio-synthesis/lookup-pipeline
```

The [complete report](../../experiment-results/hardware-overhead/full-report/report.md)
combines R1–R4 with the completed Quartus journal-only M20K control. To regenerate
its PDF, self-contained HTML, Markdown, nine figures, CSVs, and ZIP bundle using
only archived results:

```bash
.venv/bin/pip install -r requirements.txt
.venv/bin/python hw/python/pifo_hardware_overhead_report.py
```

This command does not invoke synthesis or require an FPGA license. It validates
completed run status, reused baseline values, resource rows, and RAM component
totals before plotting. `full-report.json` records the evidence date and scope;
the generated `validation.json` records input hashes. Original R1–R4 artifacts
are preserved, including the automatic Quartus replay result whose journal
spilled into registers. The M20K-forced point is shown as a separate measurement.

The R1 and R2 runners share build names so the 1,024-ID results can be reused.
RIO-only builds begin with `rio-only-`; whole-mesh build directories are separate.
Reuse checks the hardware settings, source/RTL hashes, directive, and successful
report completion. `--only-vflows 32,64` schedules part of the grid while keeping
the full grid in the result report. Do not launch two jobs for the same build
directory simultaneously. `--collect-only` updates the archive/CSVs from existing
builds without launching synthesis.
With two or more jobs, the runner reserves separate Quartus and Vivado queues
so one vendor cannot block all work for the other. After an interrupted
orchestrator, `--adopt-running` waits for compatible live synthesis wrappers
and resumes the remaining grid without replacing their files. It verifies
dimensions and source/RTL hashes before adopting a live job.
Use two jobs for the full grid on this 220 GiB host; higher concurrency can
exhaust RAM when the 1,024-ID vendor jobs overlap. The recorded run initially
used more workers and later paused/resumed selected processes under memory
guards. These scheduling events are retained in `diagnostics/`.

Each case follows the existing experiment layout:

- `experiment-config.json`, `execution.json`: configuration and actual work root.
- `runs/<build>/`: source/size manifest, setup, vendor logs, raw reports, and summary.
- `resources.csv`: measured counts with native units and source references.
- `comparison.csv`, `report.md`: paired absolute/percentage differences.
- `device-capacity.csv`: comparison against capacities reported by the tools;
  exceeding a capacity rules out a fit, while passing does not prove routability.
- `failed-capacity-checks.csv`, `failed-capacity-comparison.csv`: RAM requirements
  explicitly reported by failed synthesis capacity checks. These diagnostic
  counts are separate from completed utilization reports and their plots.
- `run-status.json`: successful, failed, or incomplete status for every requested run.
- R1: `figures/resource-table/{data.csv,figure.svg,figure.png}`.
- R2: `figures/{logic,registers,memory,overhead-percent}/{data.csv,figure.svg,figure.png}`.

## Functional and elaboration checks

`synthesis/validate_configuration.py` checks a small full mesh through its normal
control and packet interfaces. It verifies staged-versus-immediate mapping
visibility, repeated commits, and the highest port/flow/token encoding. Both
versions passed: the ordinary version checked 633 cycles and seven packets;
the dynamic version checked 1,494 cycles and seven packets. The numeric-smoke
validation archives record the RTL and testbench hashes and simulator logs.
The new external boundary also passed both tests after binding the house PIFO
back to its ports, with the same cycle and packet counts. The binding and
reference RTL hashes are archived in `validation/rio-boundary-*-smoke/`.

Large-memory initialization uses numeric zeros through SpinalHDL's `initBigInt`,
avoiding an elaborated `UInt` literal per RAM word. Table contents and hardware
geometry are unchanged. This follows the [SpinalHDL 1.12.3 implementation](https://github.com/SpinalHDL/SpinalHDL/blob/v1.12.3/core/src/main/scala/spinal/core/Mem.scala#L197).
The earlier large elaboration attempt is retained as a cancelled diagnostic;
it has no synthesis result.

Quartus uses a separate derived RTL view with `ram_init_file` attributes and
compact zero-filled MIF ranges. The script verifies every canonical binary
initialization word, width, and depth, then disables only the equivalent
`$readmemb` block in that Quartus view. Canonical simulation/Vivado RTL remains
unchanged. Each manifest records the conversion and derived hashes. A small
RIO control produced identical counts with either format: 191 ALMs, 375 ALUTs,
318 registers, and 1,736 block-memory bits. The conversion follows the vendor's
[inferred-memory initialization attribute](https://docs.altera.com/r/docs/683283/18.1/quartus-prime-standard-edition-user-guide/ram-initialization-file-for-inferred-memory)
and [MIF format](https://resources.altera.com/quartushelp/17.0/reference/glossary/def_mif.htm).

## Pipelined lookup exploration

The ordinary and atomic RAM lookup paths already use one-cycle synchronous
reads. R3 tests an isolated registered output pipeline at a smaller table size.
Its testbench checks data/valid alignment, bubbles, bursts, highest addresses,
read/write collisions, and reset during outstanding reads. The one-cycle path
checked 255 responses; the eight-cycle path checked 249, with outstanding valid
responses intentionally cleared by reset. Both accept one request per cycle.
No timing closure or full-mesh atomic commit behavior is established by R3.

At this tested geometry, extra pipeline stages did not change the block-memory
allocation: Quartus retained 1,310,720 mapped bits, and Vivado retained 36 BRAM36
tiles with no URAM. Quartus changed from 7 to 84 registers at 359 ALMs; Vivado
changed from 102 to 111 LUTs and 3 to 20 FFs, using 10 LUTs for memory in the
long pipeline. Vivado's `Synth 8-7124` diagnostic identifies poor URAM utilization
for this narrow table in both variants. See the
[R3 report](../../experiment-results/hardware-overhead/r3-lookup-pipeline/report.md).

The 1,024-ID replay run exposes a different geometry: Vivado's `Synth 8-6793`
diagnostic reports 104 required pipeline stages for automatic URAM mapping of
each deep `engineCAM` table, with zero available internal pipeline stages.
Those tables have 8,388,608 words and are unchanged by mapper replay. This is a
tool diagnostic, not a measured 104-cycle implementation or a claim that it
would fit the device. The small R3 result cannot be extrapolated to that case.

A useful integrated follow-up is a registered bank-selection/output
stage for deep tables, with the same extra stage in both configurations. It
would add read latency while retaining one request per cycle; request metadata,
bank selection at acceptance, response buffering, and commit-boundary behavior
must advance together. The input/output stream queues must cover all in-flight
responses under backpressure.

That pipeline can improve a long RAM-to-multiplexer timing path. It does not
reduce the number of dense table words, so it cannot solve the above capacity
growth. Compare it separately at a measured feasible table size, including
latency, initiation interval, registers, and timing; synthesis counts alone do
not establish a frequency improvement. R1/R2 retain the existing lookup latency
so their difference isolates the configuration implementation.

The 1,024-ID ordinary-table Vivado run provides a concrete motivation: messages
`Synth 8-6792` and `Synth 8-6793` report selecting BRAM instead of URAM because
the deep memory has insufficient pipeline registers. For the reported
`Mapper_14:/ram_reg` instance, the tool requests 104 additional pipeline stages
and finds zero. This is a mapping diagnostic, not a measured timing result.
The existing synchronous RAM read does not supply those additional stages.

A practical follow-up should therefore explore explicit RAM banking plus a
registered bank-selection tree, as well as a simple output register. Increasing
the latency of one enormous inferred cascade by 104 cycles is a different
tradeoff from banking a table into shorter memories. Apply the selected
geometry and latency to both ordinary and dynamic versions, then measure native
BRAM/URAM counts, logic, registers, throughput, and latency. Preserve the bank
selected when a request was accepted across a commit, and extend the copy
address/valid pipeline if the synchronization read latency changes.

The synthesis hierarchy also exposes a separate implementation opportunity:
the existing atomic mapper declares two reads and a write for each bank. At
128 IDs, Quartus maps each post-mapper bank into two RAM replicas, yielding
5,242,880 mapped memory bits per PE versus 1,310,720 for the ordinary mapper.
Packet reads and copy reads need separate addresses while copying; writes target
the inactive bank. Sharing a physical read/write port according to bank role
could reduce replication, but requires a changed RAM template and commit/copy
regression tests. Pipelining alone does not remove that port requirement.
