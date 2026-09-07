# Isolated large lookup: pipeline mapping exploration

One ordinary lookup bank; geometry matches one post-mapper bank at 5 PEs / 128 vFlows. Not an integrated RIO result.

Both accept one request per cycle; output latency is 1 or 8 cycles.
Synthesis only; no implementation or frequency improvement has been established.

| Platform | Resource | Short pipeline | Long pipeline | Difference | Change |
|---|---|---:|---:|---:|---:|
| Quartus | Estimated ALMs | 359 | 359 | +0 | +0.00% |
| Quartus | Combinational ALUTs | 286 | 285 | -1 | -0.35% |
| Quartus | Registers / FFs | 7 | 84 | +77 | +1100.00% |
| Quartus | Mapped block-memory bits | 1,310,720 | 1,310,720 | +0 | +0.00% |
| Quartus | MLAB memory bits | 0 | 0 | +0 | N/A |
| Quartus | DSP blocks | 0 | 0 | +0 | N/A |
| Vivado | CLB LUTs | 102 | 111 | +9 | +8.82% |
| Vivado | Registers / FFs | 3 | 20 | +17 | +566.67% |
| Vivado | BRAM36 tile equivalents | 36 | 36 | +0 | +0.00% |
| Vivado | URAM288 blocks | 0 | 0 | +0 | N/A |
| Vivado | Allocated BRAM + URAM bits | 1,327,104 | 1,327,104 | +0 | +0.00% |
| Vivado | LUTs used as RAM | 0 | 10 | +10 | N/A |
| Vivado | DSP blocks | 0 | 0 | +0 | N/A |

The validation archive checks data/valid alignment, bubbles, bursts, collisions, highest addresses, and reset during outstanding reads.
