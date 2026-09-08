# Shared-FIFO replay

Replay is the default implementation and uses the existing 256-entry controller
FIFO, with one slot reserved for commit. There is no separate replay journal.
The current [protocol and simulation checks](../../REPLAY.md) describe bank
selection, command retention, commit acknowledgement, and FIFO capacity.

The completed [Quartus and Vivado experiments](../../experiment-results/shared-fifo-overhead/README.md)
compare this implementation with ordinary tables using the same FIFO depth.
Earlier prototype validation is retained under
`experiment-results/hardware-overhead/validation/shared-control-fifo/`.
