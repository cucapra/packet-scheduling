# Quartus experiment with the repository's stock SystemVerilog PIFO

The full mesh synthesized successfully with **71,478 estimated ALMs**, versus
90,077 for the house PIFO: **18,599 fewer ALMs (20.65%)**. However, the stock
core fails a functional ordering test: consecutive pops of one virtual PIFO
can return another virtual PIFO's entry. These are **experimental resource
measurements, not a validated replacement**. The default backend remains `house`.

## Fixed hardware and tool settings

Both runs target **Agilex 7 AGFB014R24B2E2V**, using Quartus Prime Pro
**25.3.1 Build 100**, balanced synthesis, eight requested threads, virtual data
pins, and a 100 MHz clock constraint. Only synthesis was run; there is no
placement, routing, timing-closure result, or board-shell integration.

The device comes from the installed Agilex F-Series development-kit definition.
The working license is `/data/work/quartus/licenses/LR-187458_License.dat`.
The stock pass finished on **September 6, 2026, at 20:01 EDT**; Quartus reported
3 minutes 6 seconds and 3,693 MB peak virtual memory.

The dimensions and surrounding hardware match the [baseline](RESULTS.md):

- **2 PEs**, **32 virtual-PIFO IDs per PE**, and **32 global flow IDs**.
- **1,024 shared sorted entries per PE**, 2,048 entries across the mesh.
  Capacity is shared among the virtual PIFOs, not 1,024 entries per virtual PIFO.
- 8-bit rank, 5-bit virtual-PIFO ID, and 7-bit token: 20 bits per sorted entry.
- Existing brains, transactional mappers, front rewrite tables, configuration/
  commit control, crossbar, and hardware queues remain present with runtime inputs.
- The simulation request controller, packet payload queues, NIC interfaces, DDR,
  and the unfinished separate hardware admission/dequeue controller are excluded.

## Resource comparison

| Complete mesh resource | House PIFO | Stock PIFO + adapter | Change |
|---|---:|---:|---:|
| Estimated ALMs | 90,077 | **71,478** | **−20.65%** |
| Share of 487,200 device ALMs | 18.49% | 14.67% | −3.82 percentage points |
| Combinational ALUTs | 141,022 | **98,332** | **−30.27%** |
| Dedicated logic registers | 42,356 | **43,176** | +820 (+1.94%) |
| Block-memory bits | 297,440 | **297,440** | unchanged |
| MLAB memory bits | 0 | 0 | unchanged |
| DSP blocks | 0 | 0 | unchanged |

Quartus reports ALMs as a whole-design estimate; its hierarchy table uses ALUTs.
The memory count is mapped logical block-memory bits, not a fitted M20K count.

| Stock mesh component | Combinational ALUTs | Registers | Block-memory bits |
|---|---:|---:|---:|
| PE 0, including its PIFO and adapter | 49,070 | 21,469 | 148,672 |
| PE 1, including its PIFO and adapter | 49,065 | 21,469 | 148,672 |
| Crossbar | 164 | 224 | 0 |
| Remaining mesh control/queue/routing | 33 | 14 | 96 |
| **Total** | **98,332** | **43,176** | **297,440** |

The following is an alternative, disjoint partition of that same total:

| Logic group, both PEs combined where applicable | ALUTs | Registers | Block-memory bits |
|---|---:|---:|---:|
| Stock `pifo` cores, excluding adapters | 94,857 | 41,118 | 0 |
| Required occupancy/empty/drain adapters | 1,902 | 740 | 0 |
| All other existing mesh hardware | 1,573 | 1,318 | 297,440 |
| **Total** | **98,332** | **43,176** | **297,440** |

The raw stock PIFOs still account for **96.47% of combinational ALUTs**. They
remain sorted register arrays with comparison and shift logic; this substitution
does not turn PIFO storage into block RAM. Excluding only the stock PIFO cores
leaves **3,475 ALUTs, 2,058 registers, and 297,440 block-memory bits**, including
the required adapters. This is a useful boundary for reporting the surrounding
mesh's cost; it is not an isolated measurement of the new reconfiguration logic.

## Integration and functional limitation

`--pifo-backend stock` selects `StockPifoRTL`, which instantiates the existing
[`hw/verilog/pifo.sv`](../hw/verilog/pifo.sv) with `NUMPIFO=1024`, `BITPORT=5`,
`BITPRIO=8`, `BITDATA=7`, and `PIFO_ID=0`. That source is **unchanged**, including
its embedded priority encoders. Its SHA-256 is
`818055f3765dea66e7d33c2046d03d94b1ef0e51fc921391d627c6e416e66093`.
The separate `pifo_new.sv` is an unfinished stub and is not used.

The adapter supplies per-port empty/drain information for front rewrite, tracks
total and per-port occupancy, and accepts pushes in pop/push1/push2 order subject
to capacity. The full PE still disables push2, as in the baseline. Stock drop
inputs are disabled. The measured area includes this adapter and the original
stock pipeline; no extra request spacing or throughput restriction was added.

The testbench checks the generated PIFO interface against an independent stable
priority queue at the actual **1,024-entry, 32-port, 8-bit-rank, 7-bit-data** size.
With Vivado **XSim 2025.2**, the stock adapter passes empty-pop and immediate
push-forwarding checks, then fails this sequence:

1. Push `(port=1, rank=1, data=11)`.
2. Push `(port=2, rank=2, data=22)`.
3. Push `(port=1, rank=3, data=33)`.
4. Wait two idle cycles, then pop port 1 on two consecutive cycles.
5. The first pop returns data 11. The second returns **rank 2/data 22**,
   while **rank 3/data 33** is required.

This also reproduces directly on the stock core at eight entries, without the
adapter. Inspection points to its pipelined pop-index adjustment: the next
lookup can select the entry being removed, then use that position after the
array shifts. Per-port counters cannot correct the returned entry. Later stock
stress cases are not reached after this failure. The stock core needs a
correctness repair or a different replacement before this resource result can
support a functionally equivalent implementation claim.

XSim requires the three existing `pf_*_nxt` declarations to appear before their
first use. The validation runner moves only those declarations in a simulation
copy; it changes no logic. Quartus compiled the original file. Early Verilator
probes had encoder simulation issues, and Questa could not check out a simulator
license; neither is used as the functional evidence for this result.

The house backend's newly generated RTL matches the measured baseline exactly
after normalizing only source-line-derived signal names. All 16 initialization
files are byte-identical. The common-interface refactor does not alter the
house datapath. The house backend passes **36,873 checked cycles** with the same
testbench, including the stock counterexample, stable ties, simultaneous
pop/two-push operations, full capacity/overflow, and deterministic random traffic.
Its validation logs are archived in `results/stock-pifo/house-validation/`.

Quartus reports no post-synthesis combinational-loop or inferred-latch DRC
violations. Existing board-reset integration warnings remain: no Reset Release
IP, reset reachability, and asynchronous-clear RAM controls. Stock-source
declaration-order and arithmetic-width warnings are retained in the logs.
Successful synthesis does not establish functional correctness.

## Reproduce and inspect

From `pifo-hardware`:

```bash
python3 synthesis/run.py --tool quartus --name stock-pifo \
  --pifo-backend stock \
  --license /data/work/quartus/licenses/LR-187458_License.dat

python3 synthesis/summarize_quartus.py \
  synthesis/build/stock-pifo/output_files/pifo.syn.rpt \
  synthesis/build/stock-pifo/quartus-summary.json

# Expected to return exit status 1 with the stock wrong-port counterexample.
python3 synthesis/validate_pifo.py synthesis/build/stock-pifo

# Generate the default backend and run the same independent contract checks.
python3 synthesis/run.py --name house-backend-check --prepare-only
python3 synthesis/validate_pifo.py synthesis/build/house-backend-check
```

The synthesis snapshot, manifest, summaries, logs, and functional failure are
saved under [`results/stock-pifo/`](results/stock-pifo/). The manifest records
both source hashes and the exact generated RTL/dependency hashes read by Quartus.
Functional validation is recorded separately from synthesis completion.
