# Controller instruction replay versus full-table synchronization

Five PEs, fixed 1,024-entry PIFO integration budget per PE; PIFO cores excluded. Global replay log: 16,384 mapper instructions.
Static uses one ordinary bank. Dynamic uses the original read/copy synchronization. Replay uses two 1R/1W banks and a shared controller log.
The log reserves space at ingress, records only pre/post mapper updates, and replays them in order after an atomic global swap.
Configuration ingress and subsequent commits wait during replay; packet lookups continue. Batches must fit the advertised log credits.
Synthesis only. Large estimates exceed device memory capacities; no implementation or timing-closure claim.

## Fixed 1,024-ID comparison

Changes below compare replay with read/copy; the full table also reports changes from ordinary tables.

| Platform | Resource | Ordinary | Read/copy | Replay | Replay − read/copy | Change |
|---|---|---:|---:|---:|---:|---:|
| vivado | CLB LUTs | 219,107 | 345,710 | 308,796 | -36,914 | -10.68% |
| vivado | Registers / FFs | 84,909 | 76,037 | 73,815 | -2,222 | -2.92% |
| vivado | BRAM36 tile equivalents | 35,850 | 81,937.5 | 51,230.5 | -30,707 | -37.48% |
| vivado | URAM288 blocks | 0 | 0 | 0 | +0 | N/A |
| vivado | Allocated BRAM + URAM bits | 1,321,574,400 | 3,020,544,000 | 1,888,561,152 | -1,131,982,848 | -37.48% |
| vivado | LUTs used as RAM | 824 | 824 | 824 | +0 | +0.00% |
| vivado | DSP blocks | 0 | 0 | 0 | +0 | N/A |

## All measured differences

| Platform | vFlows | Comparison baseline | Resource | Baseline | Replay | Difference | Change |
|---|---:|---|---|---:|---:|---:|---:|
| quartus | 32 | static | Estimated ALMs | 1,385 | 3,136 | +1,751 | +126.43% |
| quartus | 32 | static | Combinational ALUTs | 1,800 | 3,856 | +2,056 | +114.22% |
| quartus | 32 | static | Registers / FFs | 1,303 | 2,885 | +1,582 | +121.41% |
| quartus | 32 | static | Mapped block-memory bits | 659,532 | 1,397,612 | +738,080 | +111.91% |
| quartus | 32 | static | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 32 | static | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 32 | dynamic | Estimated ALMs | 3,565 | 3,136 | -429 | -12.03% |
| quartus | 32 | dynamic | Combinational ALUTs | 4,188 | 3,856 | -332 | -7.93% |
| quartus | 32 | dynamic | Registers / FFs | 3,080 | 2,885 | -195 | -6.33% |
| quartus | 32 | dynamic | Mapped block-memory bits | 1,644,972 | 1,397,612 | -247,360 | -15.04% |
| quartus | 32 | dynamic | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 32 | dynamic | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | static | CLB LUTs | 3,309 | 4,011 | +702 | +21.21% |
| vivado | 32 | static | Registers / FFs | 2,036 | 2,125 | +89 | +4.37% |
| vivado | 32 | static | BRAM36 tile equivalents | 30 | 51.5 | +21.5 | +71.67% |
| vivado | 32 | static | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | static | Allocated BRAM + URAM bits | 1,105,920 | 1,898,496 | +792,576 | +71.67% |
| vivado | 32 | static | LUTs used as RAM | 532 | 552 | +20 | +3.76% |
| vivado | 32 | static | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | dynamic | CLB LUTs | 3,853 | 4,011 | +158 | +4.10% |
| vivado | 32 | dynamic | Registers / FFs | 2,355 | 2,125 | -230 | -9.77% |
| vivado | 32 | dynamic | BRAM36 tile equivalents | 60 | 51.5 | -8.5 | -14.17% |
| vivado | 32 | dynamic | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | dynamic | Allocated BRAM + URAM bits | 2,211,840 | 1,898,496 | -313,344 | -14.17% |
| vivado | 32 | dynamic | LUTs used as RAM | 592 | 552 | -40 | -6.76% |
| vivado | 32 | dynamic | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 128 | static | Estimated ALMs | 4,068 | 10,251 | +6,183 | +151.99% |
| quartus | 128 | static | Combinational ALUTs | 4,424 | 12,754 | +8,330 | +188.29% |
| quartus | 128 | static | Registers / FFs | 1,499 | 8,701 | +7,202 | +480.45% |
| quartus | 128 | static | Mapped block-memory bits | 11,813,092 | 18,879,076 | +7,065,984 | +59.81% |
| quartus | 128 | static | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 128 | static | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 128 | dynamic | Estimated ALMs | 9,593 | 10,251 | +658 | +6.86% |
| quartus | 128 | dynamic | Combinational ALUTs | 15,179 | 12,754 | -2,425 | -15.98% |
| quartus | 128 | dynamic | Registers / FFs | 8,996 | 8,701 | -295 | -3.28% |
| quartus | 128 | dynamic | Mapped block-memory bits | 31,487,332 | 18,879,076 | -12,608,256 | -40.04% |
| quartus | 128 | dynamic | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 128 | dynamic | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 128 | static | CLB LUTs | 7,362 | 13,567 | +6,205 | +84.28% |
| vivado | 128 | static | Registers / FFs | 6,107 | 7,837 | +1,730 | +28.33% |
| vivado | 128 | static | BRAM36 tile equivalents | 500 | 694 | +194 | +38.80% |
| vivado | 128 | static | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 128 | static | Allocated BRAM + URAM bits | 18,432,000 | 25,583,616 | +7,151,616 | +38.80% |
| vivado | 128 | static | LUTs used as RAM | 832 | 912 | +80 | +9.62% |
| vivado | 128 | static | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 128 | dynamic | CLB LUTs | 14,035 | 13,567 | -468 | -3.33% |
| vivado | 128 | dynamic | Registers / FFs | 8,707 | 7,837 | -870 | -9.99% |
| vivado | 128 | dynamic | BRAM36 tile equivalents | 1,040 | 694 | -346 | -33.27% |
| vivado | 128 | dynamic | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 128 | dynamic | Allocated BRAM + URAM bits | 38,338,560 | 25,583,616 | -12,754,944 | -33.27% |
| vivado | 128 | dynamic | LUTs used as RAM | 1,072 | 912 | -160 | -14.93% |
| vivado | 128 | dynamic | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | static | CLB LUTs | 219,107 | 308,796 | +89,689 | +40.93% |
| vivado | 1024 | static | Registers / FFs | 84,909 | 73,815 | -11,094 | -13.07% |
| vivado | 1024 | static | BRAM36 tile equivalents | 35,850 | 51,230.5 | +15,380.5 | +42.90% |
| vivado | 1024 | static | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | static | Allocated BRAM + URAM bits | 1,321,574,400 | 1,888,561,152 | +566,986,752 | +42.90% |
| vivado | 1024 | static | LUTs used as RAM | 824 | 824 | +0 | +0.00% |
| vivado | 1024 | static | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | dynamic | CLB LUTs | 345,710 | 308,796 | -36,914 | -10.68% |
| vivado | 1024 | dynamic | Registers / FFs | 76,037 | 73,815 | -2,222 | -2.92% |
| vivado | 1024 | dynamic | BRAM36 tile equivalents | 81,937.5 | 51,230.5 | -30,707 | -37.48% |
| vivado | 1024 | dynamic | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | dynamic | Allocated BRAM + URAM bits | 3,020,544,000 | 1,888,561,152 | -1,131,982,848 | -37.48% |
| vivado | 1024 | dynamic | LUTs used as RAM | 824 | 824 | +0 | +0.00% |
| vivado | 1024 | dynamic | DSP blocks | 0 | 0 | +0 | N/A |

Incomplete measurements (never interpreted as zero):

- quartus, 1024 IDs, dynamic: `synthesis_running`.
- quartus, 1024 IDs, replay: `synthesis_running`.

Reference static/read-copy measurements retain their exact archived source snapshot. Replay has a separately hashed source snapshot; device, widths, PE count, external PIFO boundary, clock, and vendor directive match.
The replay log is fixed at the same depth across this sweep. Its cost is therefore more prominent at small table sizes.
