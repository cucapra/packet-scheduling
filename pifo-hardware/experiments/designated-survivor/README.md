# What the designated-survivor link buys

Run from `pifo-hardware`:

Use a Python environment with `requirements.txt` installed; the commands below use the repository's `.venv`.

```sh
.venv/bin/python hw/python/pifo_survivor_all.py
```

The runner invokes two separate CLIs: `pifo_survivor_compiler.py` generates direct timed transactions using the existing tree-to-tree compiler; `pifo_simulator.py` consumes those transactions and a separate traffic file. Results go to `experiment-results/designated-survivor/`, with one directory per pre-phase and one subdirectory per mechanism.

```sh
# The canonical 2000-cycle pre-phase only:
.venv/bin/python hw/python/pifo_survivor_all.py --pre-cycles 2000
# Add only the copy series, reusing the existing link/reserved results:
.venv/bin/python hw/python/pifo_survivor_all.py --runs copy
# Replot/revalidate existing triples without rerunning RTL:
.venv/bin/python hw/python/pifo_survivor_all.py --render-only
# Independent, minimal per-figure scripts:
.venv/bin/python hw/python/pifo_survivor_zoom_figure.py
.venv/bin/python hw/python/pifo_survivor_stop_figure.py
```

`tree-move.json` describes p1 = Strict(zoom, gmail) to p2b = Strict(zoom, RR(gmail, spotify)). Every flow terminates in a distinct hardware FIFO node, in addition to the simulator-side packet metadata FIFO. Push creates a token at each node on its path, including the FIFO; pop must traverse the FIFO before reaching the packet output. RTL still stores scheduler tokens, not packet payloads.

| Policy | Flow | Pop path |
| --- | --- | --- |
| p1 | zoom / gmail | root (PE 1) → dedicated FIFO (PE 3) → output |
| p2b | zoom | root (PE 1) → dedicated FIFO (PE 3) → output |
| p2b | gmail / spotify | root (PE 1) → RR (PE 2) → dedicated FIFO (PE 3) → output |

Old and new FIFO versions use different vPIFO IDs. The reserved wrapper is on PE 4, above both trees, and is allocated outside the union of their occupied PEs. Strict* leaves PE 4 unused. The copy baseline stops the hardware, copies the old FIFO PE 3 → 4 and root PE 1 → 2, then prefills a wrapper at the original root on PE 1. Its new tree uses root PE 2, RR PE 3 and FIFO PE 4, with distinct old/new vPIFO IDs. All three mechanisms use the same four-PE shape, identical initial configuration and identical generated traffic for each point. `transactions.plan.json` records the actual compiled enqueue/pop paths and copy list. `settings.json` describes the shared CBR trace and sweep durations.

The runner explicitly selects `--evaluation-hardware`, building `EvaluationPifoMesh` rather than the production image. All three mechanisms use the current 256-entry shared command FIFO and mapper-write replay immediately after commit. The fourth PE widens post-mapper addresses, but synchronization replays written slots; it does not scan the full 512-entry bank. The stop/prefill/copy datapaths are absent from the normal `PifoMesh` build.

The sources offer zoom 0.40 throughout, gmail 0.80 before t1 and 0.20 afterward, and spotify 0.20 after t1. Rates are fractions of a 16-byte/cycle link, with 48-byte packets. Sources keep generating throughout configuration and stops. `push_cycle` is generation time; `admitted_cycle` in request-results.csv records switch admission separately.

## Scope

The pre-phases remain 0, 1000, 2500 and 5000 cycles; the 2000-cycle trace supplies Figure A. Rates, sizes, seeds and phase durations are unchanged so this modification isolates the explicit FIFO layer. Backlogs and link utilization are measured again for the new topology. The x-axis uses packets outstanding at t1, and logs separately give the hardware old-root count prefetched after quiescence. Post-phases are at least 8000 cycles and at least the pre-phase plus 4000 cycles.

Each PE has 8 × 128 = 1024 shared physical token slots. Old/new per-flow FIFO nodes share PE 3's capacity; adding a FIFO node does not allocate a separate 1024-entry memory. Packet metadata FIFOs have 4096 slots per flow. The source-side gate queue is unbounded in the lossless model; measured peak occupancy is reported rather than claiming unlimited hardware memory. The 1000/2000-packet points remain omitted. Raising only `queue_depth` cannot provide more hardware token capacity.

## Measured lifecycle

The materialized baseline uses StopWorld, an autonomous one-write-per-cycle PrefillPifo, and UpdateRoot/CommitMapper to publish the wrapper. The driver first waits for a partly admitted packet and outstanding traversals to finish; the RTL also gates insertion and root requests and snapshots the old root. Both intervals are logged, so software quiescence is not disguised as per-entry hardware work.

Both figures contain Strict*, reserved-wrapper and copy/prefill series. Figure B measures the entire birth stop, including relocation in the copy case. Measurements split actual prefill writes, copy dispatch-to-completion cycles, and other overhead. Copy moves two scheduler tokens per retained packet (root plus FIFO); copied occupied PIFOs and token counts are checked against the hardware snapshot. Figure A measures zoom delay from generation, including all time held at the door.

After GuardDrain on every old FIFO, a second package invalidates retired mappings/brains, stops creating wrapper entries and publishes the survivor as root. A third cleanup package uses ClearPifoEngine plus ordinary invalidation writes and a commit. Clear waits for queued root visits and active PE work before invalidating the detached wrapper's remaining tokens by clearing its counters; it does not rely on a slow bank-copy delay. This is logical reclamation, not a sequential token deletion. The packet FIFOs on PE 3 remain intact and surviving packets must still complete. Validation uses compiler/unit checks, elaboration-only image-isolation checks and the experiment runs themselves, not standalone RTL regression tests.

All commits have separate start, commit accepted, ready-for-next-commit and shared old-tree-drained markers, with blue/amber/green backgrounds. Each figure folder includes a minimal standalone `plot.py`, its plot data, full `packets.csv` and `commits.csv`; copying that folder is sufficient to replot without repository code or shared styles.

`maintenance-events.csv` records driver/hardware stops, the hardware count snapshot, prefill start/completion with actual accepted token writes, root detach/publication with live occupancy, and reclamation with discarded token count. `controller-instructions.csv` distinguishes queue acceptance from dispatch and commit publication. Raw per-packet CSVs and both figures' plotted data are retained.

The copy series measures frozen old-tree descent with the dedicated brain-bypassing read/inject datapath. It preserves old token IDs and ranks; new arrivals go only to the freshly configured survivor. Teardown changes the root and reclaims the wrapper, just as in the reserved baseline. It does **not** measure live-survivor ascent or brain-state migration, which remain unsupported. Reservation avoids relocation; Strict* avoids the wrapper itself.
