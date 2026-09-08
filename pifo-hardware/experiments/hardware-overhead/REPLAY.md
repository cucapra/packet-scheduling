# Controller FIFO replay

Replay is the default RTL configuration. The controller executes and replays
commands from the **same control FIFO RAM**; there is no separate journal.
The ordinary comparison (`--configuration static`) uses the same configured
FIFO depth, retains command routing, and ignores commits.

The saved R1/R2 synthesis measurements are from the earlier separate-journal
implementation at `d5a10e8`. They have not been regenerated for shared FIFO replay.
Their totals and percentage denominators continue to include five measured PIFOs.

## Transaction protocol

1. Each accepted command is written once into the controller FIFO. An execution
   pointer reads commands in order. A release pointer frees unbanked commands
   immediately until the first pre/post-mapper update starts a retained epoch.
2. Mapper updates write the inactive bank. Their original FIFO entries, and all
   subsequent commands through commit, remain allocated for the second pass.
3. Commit swaps the selected mapper banks in every PE on the same cycle. The
   controller saves the commit boundary and rewinds its read pointer to the
   first retained command.
4. The same RAM read port issues the retained pre/post updates again. Their
   ordinary write path now targets the old active bank. Brain, state, and
   front-rewrite commands are skipped on this pass, so they execute only once.
5. Entries are reclaimed as replay advances; the commit marker is reclaimed at
   the end. Ingress and later queued commands wait until replay finishes.
   Commands already queued after that commit remain intact for the next epoch.

Repeated writes preserve their order, so the last write to an address wins.
Untouched entries retain their values in both banks. An empty commit needs no
replay. Packet lookups continue throughout synchronization; a lookup on the
swap cycle returns the bank selected when that lookup was issued.

## Capacity and memory ports

`--control-queue-depth D` (Scala `EngineConfig.commitQueueLength`) sets the only
command RAM depth for either configuration. It must be a power of two at least
two and defaults to **four entries**, unchanged from the ordinary controller.
One entry is reserved for commit. Thus an epoch can retain at most **D − 1
commands from its first mapper update to immediately before commit**, including
any intervening unbanked commands. An unbanked-only prefix can stream without
accumulating retained entries. Larger atomic batches require a larger FIFO;
they are never silently split into multiple commits.

`replayBusy` indicates the second pass. The legacy port `replayLogAvailable`
now reports free non-commit FIFO slots, excluding the reserved slot; its width
is `log2Up(D + 1)`. All command kinds consume shared capacity. A full retained
epoch can still accept commit into its reserved slot. Once that slot also holds
a queued command, the physically full FIFO stalls all ingress until space frees.
During replay, ingress is stalled regardless of the number of reclaimed slots.
Drivers must send commit before exhausting the retained-command limit. The
request simulator accepts the same `--control-queue-depth` option and rejects
oversized packages or streamed epochs with a capacity error.

A FIFO word is the complete control message:
`3 + engine_id_bits + vpifo_id_bits + token_bits + 32` bits. At five PEs and
1,024 IDs, both ordinary and replay use `D × 61` payload bits (**244 bits at
D = 4**). Replay adds pointer/phase control, not another payload array. This is
a declaration-level storage calculation, not a mapped FPGA resource estimate.

The FIFO has one synchronous read port and one write port. The read output
register holds a stalled command; no separate journal or payload cache is
allocated. Both passes time-share that read port. Each selected mapper bank
still has one synchronous packet-read port and one configuration-write port,
with mutually exclusive write enables selecting the inactive bank. Other
mapper memories and bank write logic are unchanged.

Replay latency depends on the retained span and downstream readiness. Skipped
unbanked commands still consume read cycles. Runtime reset flushes the FIFO;
it does not recover a mapper transaction interrupted between staging and replay.

## Correctness validation without synthesis

From `pifo-hardware`:

```bash
python3 synthesis/run.py --tool vivado --name shared-replay-d4 \
  --engines 2 --vpifos 8 --entries-per-pe 32 --pifo-backend external \
  --control-queue-depth 4 --generate-only
python3 synthesis/validate_replay.py synthesis/build/shared-replay-d4

# Repeat with depths 2 and 8 and distinct build names.
# Full packet regression with the unchanged house PIFO:
python3 synthesis/run.py --tool vivado --name shared-replay-packet \
  --engines 1 --vpifos 8 --entries-per-pe 32 --pifo-backend house \
  --control-queue-depth 4 --generate-only
python3 synthesis/validate_configuration.py synthesis/build/shared-replay-packet

# Driver regression, with Icarus Verilog on PATH:
sbt 'runMain rio.sim.SharedReplayDriverSim'
```

The FIFO scoreboard checks every output against accepted commands and an
independent expected replay sequence under backpressure. The two-PE bench checks
both banks against per-commit histories, simultaneous swaps, duplicates, queued
next epochs, and packet reads overlapping swaps/replay. The packet test checks
staged visibility, repeated commits, and highest encoded flow IDs. Generated
RTL is also checked for a single controller RAM and one read/write port per bank.
[Saved validation](../../experiment-results/hardware-overhead/validation/shared-control-fifo/validation.json)
records tool evidence and hashes. These checks do not run synthesis or implementation.
