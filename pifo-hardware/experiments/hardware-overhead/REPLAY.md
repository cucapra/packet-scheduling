# Controller instruction replay

Replay is the default RTL and synthesis configuration. The ordinary comparison
uses `--configuration static`, retaining the controller and ignoring commits.
R1/R2 compare these two designs; both totals and percentage denominators include
five measured PIFOs. See [the experiment definitions](README.md).

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

## Journal and storage

`--replay-log-depth` is a power of two, defaulting to **16,384 instructions across
the mesh**. R1/R2 keep this depth fixed throughout the 32–1,024 vFlow sweep. At five PEs it
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
controller log, counters, routing, and exported status are included in all replay
resource counts. Both mapper banks remain allocated, and dense table depth is
unchanged. This removes the copy read ports; it does not remove the second bank
or solve the quadratic address-space growth.

For this five-PE sweep the instruction width is `10 + 3 * log2(vflows)`:
25, 28, 31, 34, 37, and 40 bits at 32, 64, 128, 256, 512, and 1,024 IDs. A fixed-capacity journal therefore
grows only with the encoded ID width, while each post-mapper bank grows as
`8 * vflows^2 * (log2(vflows) + 3)` bits. This is a declared-storage model,
not an ALM/LUT prediction. Replay removes read-port replication from the
double-buffer implementation, but atomic publication still needs two banks
instead of the ordinary implementation's one.

Synchronization work scales with the number of staged instructions, rather than
the full table depth. The controller issues at most one replayed instruction per
cycle globally. An empty commit needs no replay. Actual busy time also depends
on write-path readiness; it is not assumed to be constant for arbitrary traffic.

The protocol starts from initialized, equal banks. Runtime reset clears the
journal; it does not recover an interrupted configuration transaction.
Quartus assigns the journal array to M20K by default. Mapper banks each retain
one synchronous read port and one write port; their mapping is checked from
the synthesis memory-instance records.

## Validation

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

The default-configuration check repeats the focused test without specifying
`--configuration`. Its [saved validation](../../experiment-results/hardware-overhead/validation/default-replay/validation.json)
and [RTL equivalence checks](../../experiment-results/hardware-overhead/validation/default-replay/default-equivalence.json)
confirm that the default selects replay and agrees with the completed explicit
replay reference measurements.

## Reproduce

From `pifo-hardware`:

```bash
# Both-vendor experiment grid and fixed setup, including measured PIFOs.
.venv/bin/python hw/python/pifo_hardware_overhead_r2.py
.venv/bin/python hw/python/pifo_hardware_overhead_r1.py --collect-only

# Focused protocol validation using the default replay configuration.
python3 synthesis/run.py --name replay-check --engines 2 --vpifos 8 \
  --entries-per-pe 32 --pifo-backend external --replay-log-depth 4 --prepare-only
python3 synthesis/validate_replay.py synthesis/build/replay-check
```

The [results index](../../experiment-results/hardware-overhead/README.md) links
the requested tables, figures, and source data. Historical raw vendor reports,
source snapshots, and protocol validation are retained as reproduction evidence.
