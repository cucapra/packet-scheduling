# Journal placement control

The original R4 Quartus result implements the large journal in logic/registers.
This follow-up changes only its RAM placement assignment, using the same
canonical RTL and the same compact initialization files. It includes two small
isolated-journal controls and a separate synthesis of the full RIO core.

See [measured counts and completion status](report.md). The original R4 values
remain in the main experiment tables. The isolated counts are never substituted
for a complete-core measurement.

The full-core QSF adds:

```tcl
set_instance_assignment -name RAMSTYLE_ATTRIBUTE M20K -entity StreamFifo_52 -to logic_ram
```

The helper discovers the generated module by its journal fields and depth,
checks that the module has exactly one top-level instance, and records the
module/RTL hashes. It does not depend on the generated suffix remaining `52`.
In isolated mode it extracts that module and changes only its top-level name
to `PifoMesh`; the archived `runs/isolated-*/rtl/` files contain the exact inputs.
The probe manifests inherit the source core's hardware/source metadata; their
`journal_probe.scope` distinguishes the actual measurement scope, and
`journal_probe.module_sha256` identifies the extracted source module.

This follows Altera's documented [RAM placement attribute](https://docs.altera.com/r/docs/683296/25.3/quartus-prime-pro-edition-settings-file-reference-manual/ramstyle_attribute)
and [RAM inference guidance](https://www.intel.com/content/www/us/en/docs/programmable/683082/25-1/controlling-ram-inference-and-implementation.html).
The assignment does not relax read-during-write semantics. The Quartus inference
log identifies `OLD_DATA` in both cases. The isolated automatic and M20K controls
both report 655,360 RAM bits and identical logic/register totals.

Reproduce from `pifo-hardware`, with the source case already prepared by the
ordinary replay workflow:

```bash
REPLAY_SOURCE=/data/work/rio-synthesis/hardware-replay/rio-replay-pe5-v1024-c1024-j16384-quartus
JOURNAL_ROOT=/data/work/rio-synthesis/journal-m20k
python3 synthesis/probe_replay_journal.py "$REPLAY_SOURCE" "$JOURNAL_ROOT/isolated-auto" --isolated
python3 synthesis/probe_replay_journal.py "$REPLAY_SOURCE" "$JOURNAL_ROOT/isolated-m20k" --isolated --force-m20k
python3 synthesis/probe_replay_journal.py "$REPLAY_SOURCE" \
  "$JOURNAL_ROOT/rio-replay-pe5-v1024-c1024-j16384-m20k-quartus" --force-m20k
python3 synthesis/collect_replay_journal.py --build-root "$JOURNAL_ROOT" --reference "$REPLAY_SOURCE"
```

Use fresh build directories. The full-core probe keeps symlinks to the source
RTL and compact initialization view, verifies their recorded hashes, and never
replaces the source build. Its manifest records the source path, exact QSF
assignment, tool version, license source path, part, constraints, and workflow
hashes. No license contents or tool databases are archived here.
