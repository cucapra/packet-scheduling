# Tree-size scalability

This sweep feeds `pifo_multiedit_compiler.compile_request` the same tenant-tree
schema as `experiments/multi-edit/request.json`. It does not introduce another
transition implementation. The two-request, five-size, four-mechanism matrix
contains 40 hardware runs. Relocation is excluded.

## Controlled inputs

- Existing tenants m = 2, 4, 8, 16, 32; existing flows N = 2m.
- WFQ root, initially equal tenant weights. Existing tenant disciplines cycle
  SP, RR, WFQ. WFQ tenants use flow weights 1:2; RR tenants use 1:1.
- Every flow has its own hardware FIFO leaf, as in multi-edit.
- R-add installs the same two-flow SP tenant at the first free slot, with
  root weight 1. R-reweight changes only tenant_01's root weight from 1 to 2.
- `Tenant_02_A` (flow 3, inside an unchanged RR tenant) is the same untouched
  witness at every point. Names and IDs are recorded in each `flows.csv`.
- 48-byte packets, 16 bytes/cycle, t1 = 2000, generation ends at cycle 10000.
  Aggregate offered load is 1.10 before and after t1. Each active flow gets
  an equal rate. For R-add, traffic for the new flows starts at t1 and load
  is redistributed over N+2 flows; the target-policy control uses that same
  traffic file, including no pre-t1 traffic for the arriving tenant.
- Per-flow CBR phases are staggered across the aggregate packet interval.
  Larger m therefore does not create a larger synchronized burst. Rounding
  gives 734 packets before t1 (1.101 measured load) at every point, and
  2933–2934 afterwards. Per-flow counts within a phase differ by at most one.

Hardware is fixed across points and mechanisms: 7 PEs, 128 virtual-PIFO IDs
per PE, 512 shared scheduler-token slots per PE (`fifo_depth=4`), 16-token
prefetch, and 4096 packet-metadata entries per flow. The 128-ID address space
accommodates 64 existing flows, two arriving flows and the reserved sentinel.
The total scheduler-token capacity per PE matches multi-edit; `fifo_depth`
is the multiplier for that shared capacity, not a per-flow occupancy limit.

The shared command/replay FIFO is fixed at 1024 entries using the existing
CLI option. The largest retained command sequence is 723 entries, so the
default 256-entry FIFO cannot represent this sweep without changing the
transaction protocol. No extra commits are inserted. Baseline algorithms
are unchanged; reset retains its declared 512-cycle teardown and 513-cycle
installation budgets. These are model costs, not measured hardware costs.

The simulator's SpinalHDL elaboration limit is increased to accommodate the
existing WFQ ranker's port/flow write-select decoder at 128 vPIFOs. This
changes neither the generated scheduling logic nor bank replay semantics.
The source gate remains unbounded and measured, not claimed as hardware RAM.

## Run the pilot before the matrix

Select JDK 17 in `JAVA_HOME` and `PATH`. Commands run from `pifo-hardware/`:

```sh
.venv/bin/python hw/python/pifo_scalability.py prepare
.venv/bin/python hw/python/pifo_scalability.py run --requests reweight --tenants 2 16 --runs rio
```

Compare the pilot rows in `experiment-results/scalability/measurements.csv`
and the individual commit rows before launching the rest. `prepare` emits
request/traffic files and compiled plans, but no simulated measurements.
Single runs write an ignored `simulation.log` in their run directory.
Do not run RTL jobs concurrently: the existing simulator shares its build
workspace. All commands stop on simulation or validation failure.

The full matrix, or selected remaining runs:

```sh
.venv/bin/python hw/python/pifo_scalability.py run
.venv/bin/python hw/python/pifo_scalability.py batch --skip-completed
.venv/bin/python hw/python/pifo_scalability.py run --requests add --tenants 4 --runs rio prefill reset control-p2
.venv/bin/python hw/python/pifo_scalability.py summarize
```

`batch` elaborates/compiles each distinct hardware/backend configuration once,
then creates a fresh backend instance and simulator state for every run.
It writes a portable `batch-arguments.txt` and an ignored `batch.log` under
the results directory. `--skip-completed` validates and reuses completed
cases; use it only with unchanged experiment inputs. Omitting it reruns
the selected cases, including pilots. No hardware state is shared between
cases. The direct Scala CLI also accepts `--batch FILE`, with one
tab-separated argument list per line.

## Measurements

Every result directory has compiled `transactions.txt` / `transactions.plan.json`
and the usual `requests.csv`, `request-results.csv`, `packet-outcomes.csv`,
`controller-instructions.csv`, and `maintenance-events.csv`. Transition runs
also have `reconfiguration-events.csv`; controls have no reconfiguration.
Each run also includes `flows.csv`, mapping numeric IDs to flow names.

`measurements.csv` reports main and guarded instruction counts separately;
initial-policy bootstrapping is excluded. Each `commits.csv` row includes
start, acceptance, publication, replay-ready time, instruction count, cycles
to publication, and bank replay. Main publication/replay-ready latency is
measured from actual transaction start; completion means final configuration
readiness minus the requested t1, including guarded cleanup/reclamation.
Neither final packet completion nor root drain is labeled configuration
completion. The reset compiler's commit-only retirement remains counted.
`guarded_instructions` follows the existing compiler's post-install cleanup
category; `guard_drain_instructions` separately counts actual GuardDrain
commands, so the reset's commit-only follow-up is not mistaken for a drain
guard. Mapper-write counts distinguish useful replay from fixed readiness
bookkeeping in an unbanked-only ChangeMeta transaction.

Global stop is actual request-start to source/pop resume, including driver
quiescence; RTL gate width and the reset's minimum model budget are separate.
The reported peak stop buffer includes packets waiting at the source as well
as admitted packets. A second peak is independently reconstructed from packet
generation/completion CSVs. Rio and control must have zero global stop.

Untouched delay is paired by request ID against the same point's steady-p2
control: `pop_cycle(run) - pop_cycle(control)`. Since source times are identical,
this is exactly the delay difference for that packet. The reported peak is
the maximum signed difference over witness packets outstanding in either run
at t1 or generated afterwards, through source end. Negative values are not
clipped or replaced with absolute differences. Complete pairs are saved in
`untouched-delay.csv`; a missing control yields an unavailable metric, not zero.
This comparison includes the control's different prehistory for R-reweight;
it is not an isolation of replay latency from scheduling-state differences.

Validation checks trace identity within each point, complete packet coverage,
zero drops, per-flow FIFO order, and accepted instructions against compiler
counts. All transitioning mechanisms must have identical measured t1 backlog.
Actual utilization and source-side waiting are retained to reveal congestion
or hardware-capacity confounds instead of assuming equal offered load is enough.

After all 40 runs complete, `summarize` generates the three requested panels
for each request and a separate bank-replay figure. Each of `figures/add/`,
`figures/reweight/`, and `figures/bank-replay/` under the results directory
contains `data.csv`, `commits.csv`, and a self-contained `plot.py`; rerendering
requires only those local files and Matplotlib. Raw packet CSVs remain in the
per-point directories, with flow-name mappings in the matching input directory.
This measures cycles on a fixed substrate, not area scaling or timing closure.
