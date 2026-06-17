# Programmable Packet Scheduling

A scheduler installed on a smartNIC has to keep running. The operator may want to add a new flow, retire an old one, change a weight, or restructure the tree of disciplines that decides who gets the next packet, and the substrate cannot afford to stop, drain, and reinitialize each time. Prior work (e.g., vPIFO, SIGCOMM '24) has pushed programmable hierarchical scheduling onto FPGA substrates but treats reconfiguration as stop-the-world: any change to the running policy means a full reinitialization.

This project is about closing that gap. We model a live scheduler as a PIFO tree (Formal Abstractions, OOPSLA '23), fix a small grammar of _atomic diffs_ on that tree, and show that every reachable reconfiguration is realizable as a sequence of those diffs threaded by intermediate _link_ schedulers. The transitionary regime is itself an ordinary scheduler, the diffs are individually sound, and the whole sequence is safe by composition. We lower the grammar to a hardware substrate that commits each diff atomically with respect to user `push`/`pop` operations, so the user-observable trace is well-formed at every instant.

## Layout

- `paper/` — the paper on live reconfiguration of hierarchical packet schedulers; see `paper/sketch.md` for the current draft.
- `rio/` — the compiler (OCaml): policy source, diff inference, and lowering to the hardware IR.
- `pifo-hardware/` — the FPGA substrate that hosts a running scheduler and executes atomic commits.
- `p4-progs/` — P4 programs used in evaluation and supporting infrastructure.
- `graphs/`, `talks/` — supporting material.
