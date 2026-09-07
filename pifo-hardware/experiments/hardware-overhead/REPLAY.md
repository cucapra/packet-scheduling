# Controller instruction replay

R4 implements the proposed alternative double buffer and measures it with the
PIFO cores excluded. The existing `dynamic` configuration remains the full-table
read/copy reference. Select `--configuration replay` for controller replay, or
`--configuration static` for the ordinary single-bank baseline.

## Transaction protocol

1. The controller reserves a replay-log entry when it accepts a pre/post-mapper
   update at ingress, including commands waiting in the normal control FIFO.
2. It records the update and routes it to the inactive bank. A stream fork holds
   the command until both actions have accepted it. Repeated writes are recorded
   in order; the last write to an address wins.
3. A commit swaps all pre/post mapper banks in all PEs on the same cycle.
4. The controller replays the recorded updates through the ordinary write path,
   which now targets the old active bank. No table read is needed to synchronize.
5. Configuration ingress and queued commands wait while replay is active. The
   next commit cannot execute until the final replayed update has been accepted.
   Packet lookup ports continue reading the active bank throughout replay.

Each mapper bank declares one synchronous packet-read port and one write port.
The selected bank is captured with each accepted lookup, so a read issued on a
swap cycle returns data from the bank selected for that request. The shared log
uses one read and one write port. Commands for unbanked brain/state/front-rewrite
tables execute once and are not replayed. Replaying those commands would repeat
unrelated state changes, rather than synchronize the banked lookup tables.

The bank-equality invariant is simple: if both banks start an epoch in state
`S`, applying the ordered update sequence `U` to the shadow produces `U(S)`.
The swap publishes that state. Replaying the same sequence into the old active
bank, which still contains `S`, leaves both banks at `U(S)`. The controller holds
the next epoch until that replay finishes. Duplicate writes need no special
handling because replay preserves their order. Entries absent from the log
retain their previous values in both banks.

## Log capacity and resource accounting

`--replay-log-depth` is a power of two, defaulting to **16,384 instructions across
the mesh**. R4 keeps this depth fixed at 32, 128, and 1,024 vFlows. At five PEs it
covers one pre- and one post-mapper update per vFlow per PE at the 1,024-ID point
(10,240 updates). Programs with more updates require a larger log or batches
that may be published as separate commits.

The top level exposes `replayBusy` and `replayLogAvailable`. Software must respect
the advertised credits: a full log accepts commits and unbanked commands but
backpressures further mapper updates. It must submit a commit before offering
more mapper updates than available credits. Updates are never silently dropped
or evicted. There is no unbounded transaction log or implicit partial commit.

An instruction stores a pre/post selector, PE ID, vPIFO ID, flow token, and the
low data bits used by the mapper. Its width is
`1 + engine_id_bits + vpifo_id_bits + 2 * token_bits`: **40 bits at 1,024 IDs**,
or 655,360 declared bits for the 16,384-entry log before FPGA mapping. The
controller log, counters, routing, and exported status are included in all R4
resource counts. Both mapper banks remain allocated, and dense table depth is
unchanged. This removes the copy read ports; it does not remove the second bank
or solve the quadratic address-space growth.

Synchronization work scales with the number of staged instructions, rather than
the full table depth. The controller issues at most one replayed instruction per
cycle globally. An empty commit needs no replay. Actual busy time also depends
on write-path readiness; it is not assumed to be constant for arbitrary traffic.

The current read/copy RTL holds synchronization busy for `D + 1` cycles for a
depth-`D` mapper: `D` pipelined copy reads followed by the final write. PEs copy
in parallel. At 1,024 IDs the deepest mapper has 8,388,608 words, so this is
8,388,609 busy cycles after each commit. Replay instead has `N` busy cycles
when its log and destination accept one instruction per cycle, with `N` counting
the staged mapper instructions across all PEs and bounded by 16,384 here.
These are controller cycle counts derived from the RTL, not measured routed
latencies. The focused test checks 52 busy cycles for 52 replayed instructions.

The protocol starts from initialized, equal banks. It does not provide recovery
of an interrupted transaction after a runtime reset clears the controller log.

## Validation and measurements

The full small-mesh packet test passed with seven packets and 633 cycles,
preserving staged mapping visibility, repeated commits, and the highest encoded
flow ID. The focused two-PE test passed 29 commits and 52 updates in 270 cycles,
with 52 replay-busy cycles. It checks the complete contents of both banks against
the accepted update history after synchronized batches, including duplicates,
partial epochs, FIFO wraparound, empty commits, a full log, and queued commits.
It verifies that a next-epoch update accepted behind a commit retains its log
reservation while the preceding epoch replays. It also checked 88 packet
lookups, including 15 during replay and 22 on a swap cycle. A prior phase of the
test checked another lookup alignment and is retained in the validation archive.
No commit executed while replay was active.

At 128 IDs, Quartus maps a post-mapper to two 1,310,720-bit RAM instances, one per
bank. The read/copy reference uses four such instances. Including the controller
log, total mapped memory decreases from 31,487,332 to 18,879,076 bits (40.04%).
Logic and register counts are measured independently; reduced RAM replication
does not guarantee a lower logic count at every point.
At the same point Vivado decreases from 1,040 to 694 BRAM36 tile equivalents
(33.27%) and from 14,035 to 13,567 LUTs (3.33%). Quartus estimates 10,251 ALMs
versus 9,593 for read/copy (6.86% higher). The comparison includes the shared
log and controller as well as the mapper RAMs.

At 1,024 IDs, Vivado decreases from 81,937.5 to 51,230.5 BRAM36 tile
equivalents (37.48%) and from 345,710 to 308,796 LUTs (10.68%). The
post-mapper banks account for 30,720 BRAM36 tiles, half the read/copy count;
the shared log adds 18 tiles. Unbanked `engineCAM` tables still account for
20,480 tiles in all three designs. The
[RAM breakdown](../../experiment-results/hardware-overhead/r4-replay/memory-breakdown.md)
reconciles each component with the reported total. Replay still uses 42.90%
more BRAM and 40.93% more LUTs than the ordinary-table baseline at this point.
These results support reducing synchronization overhead, but do not establish
negligible overhead or a physical fit for the current dense tables.

The original 1,024-ID Quartus run also completed. Its post-mapper RAMs decrease
from 2,181,038,080 to 1,090,519,040 implementation bits, with exactly two simple
dual-port banks per PE and no copy-read replicas. However, automatic mapping
implements the shared 655,360-bit journal in registers: that FIFO hierarchy
reports 655,446 registers and zero RAM bits. Whole-design totals are 479,742 ALMs,
681,729 ALUTs, 923,812 registers, and 1,426,064,152 RAM bits. Compared with
read/copy, RAM decreases 43.33% and ALMs decrease 8.92%, while registers increase
243.48%. This register cost is included in the comparison; zero journal RAM does
not mean zero journal storage. The smaller Quartus journals and all Vivado
journals map to block RAM.

R4 uses the same two target parts, eight-thread settings, 100 MHz constraint,
external PIFO interface, and Vivado RuntimeOptimized directive as R1/R2. Large
Vivado cases use the explicit estimation-only capacity hook. Results remain
synthesis estimates and cannot establish a physical fit or timing closure.
The [R4 report](../../experiment-results/hardware-overhead/r4-replay/report.md)
contains the actual counts, absolute changes, and percentages against both
ordinary tables and the read/copy implementation. All original R4 measurements
are complete.

The old static/read-copy source is preserved in
`diagnostics/read-copy-source-snapshot/` and the new source in `r4-replay/workflow/`
under `experiment-results/hardware-overhead/`. At 32 IDs, regenerated static and
read/copy RTL matched their reference RTL after bijective renaming of generated
source-line identifiers; every initialization file also matched exactly.

## Reproduce

From `pifo-hardware`:

```bash
.venv/bin/python hw/python/pifo_hardware_replay.py
.venv/bin/python hw/python/pifo_hardware_replay.py --collect-only
.venv/bin/python hw/python/pifo_hardware_replay.py --render-only
# Reconcile RAM components directly from the archived reports.
.venv/bin/python hw/python/pifo_replay_memory_breakdown.py

# Small protocol validation (generate before running the test).
python3 synthesis/run.py --name replay-check --engines 2 --vpifos 8 \
  --entries-per-pe 32 --pifo-backend external --configuration replay \
  --replay-log-depth 4 --prepare-only
python3 synthesis/validate_replay.py synthesis/build/replay-check
```

To compare a new replay synthesis with the committed reference measurements,
without depending on this machine's original build directory:

```bash
.venv/bin/python hw/python/pifo_hardware_replay.py \
  --reference-root experiment-results/hardware-overhead/r2-vflows/runs
```

To regenerate the exact old static/read-copy sources, restore them into a fresh
directory. This keeps the recorded reference hashes valid and avoids replacing
the original builds with the later source revision:

```bash
REFERENCE_COPY=/data/work/rio-synthesis/read-copy-reproduction
mkdir "$REFERENCE_COPY"
cp -a experiment-results/hardware-overhead/diagnostics/read-copy-source-snapshot/{hw,project,build.sbt} "$REFERENCE_COPY/"
cp -a experiment-results/hardware-overhead/workflow/{hw,synthesis} "$REFERENCE_COPY/"
cp -a experiments "$REFERENCE_COPY/"
# Updated report readers handle Quartus's overflowing aggregate RAM total.
cp hw/python/pifo_hardware_overhead.py "$REFERENCE_COPY/hw/python/"
cp synthesis/summarize_quartus.py "$REFERENCE_COPY/synthesis/"
.venv/bin/python "$REFERENCE_COPY/hw/python/pifo_hardware_overhead_r2.py" \
  --build-root "$REFERENCE_COPY/builds" --jobs 2

# Use those newly synthesized references in R4.
.venv/bin/python hw/python/pifo_hardware_replay.py \
  --reference-root "$REFERENCE_COPY/builds"
```

The runner adopts compatible live builds, uses distinct replay build names,
and preserves the references. It copies RTL for Vivado before launching Quartus
on that build, and waits for 120 GiB of available host RAM and other large
Vivado synthesis wrappers to finish before starting a large replay case.
The measured run initially used an 80 GiB threshold; a later host-memory peak
required pausing concurrent jobs with SIGSTOP and resuming with SIGCONT. That
scheduling event and the original threshold are preserved in `diagnostics/`.
It did not change RTL or synthesis settings. The log capacity is an explicit
experiment parameter.
