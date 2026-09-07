# Shared zero-initialization files

This optional Quartus workflow probe gives equal-width/depth RAMs the same
zero-filled MIF file. Every RAM instance retains independent runtime state.
It uses a separate build, verifies the original derived RTL/MIF hashes and
zero contents, and leaves the original build intact. The new build links to
the original canonical RTL directory, which must remain available.

At five PEs and 128 IDs, all three controls produced exactly the same ALM,
ALUT, register, block-memory, MLAB, and DSP counts as their individual-file
references. Quartus-reported synthesis times were:

| Configuration | Individual MIFs | Shared MIFs | Original time | Shared time |
|---|---:|---:|---:|---:|
| Ordinary | 30 | 6 | 35 s | 26 s |
| Read/copy | 40 | 6 | 65 s | 47 s |
| Replay | 40 | 6 | 52 s | 45 s |

These are single-run runtime observations on the same host, alongside other
synthesis jobs. They do not establish the speedup at 1,024 IDs. The primary
R1/R2/R4 runs retain their original initialization workflow.

From `pifo-hardware`, use a fresh destination directory:

```bash
python3 synthesis/probe_shared_init.py \
  /data/work/rio-synthesis/hardware-replay/rio-replay-pe5-v128-c1024-j16384-quartus \
  /data/work/rio-synthesis/shared-init/new-replay-control
```

`validation.json` records equality and measured times; `runs/` contains the
actual setup, manifests, reports, and comparisons. `workflow/` preserves the
probe script used, alongside the shared vendor helpers archived in the parent
hardware-overhead workflow directories.
