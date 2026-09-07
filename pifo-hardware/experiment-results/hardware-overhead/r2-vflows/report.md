# RIO resource scaling with the vFlow namespace (PIFO excluded)

**RIO logic only; PIFO cores are excluded through explicit top-level interfaces.**
Percentages use the ordinary RIO logic as denominator, not the total scheduler.
PIFO storage, sorting, occupancy and drain detection require a separate resource budget.

Difference = dynamic − static; percentage uses static as the denominator.
Blank percentages mean a zero baseline. Only completed synthesis reports supply resource values.

| Platform | vFlows | Resource | Static | Dynamic | Absolute change | Change |
|---|---:|---|---:|---:|---:|---:|
| quartus | 32 | Mapped block-memory bits | 659,532 | 1,644,972 | +985,440 | +149.42% |
| quartus | 32 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 32 | Estimated ALMs | 1,385 | 3,565 | +2,180 | +157.40% |
| quartus | 32 | Combinational ALUTs | 1,800 | 4,188 | +2,388 | +132.67% |
| quartus | 32 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 32 | Registers / FFs | 1,303 | 3,080 | +1,777 | +136.38% |
| vivado | 32 | BRAM36 tile equivalents | 30 | 60 | +30 | +100.00% |
| vivado | 32 | Allocated BRAM + URAM bits | 1,105,920 | 2,211,840 | +1,105,920 | +100.00% |
| vivado | 32 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | CLB LUTs | 3,309 | 3,853 | +544 | +16.44% |
| vivado | 32 | LUTs used as RAM | 532 | 592 | +60 | +11.28% |
| vivado | 32 | Registers / FFs | 2,036 | 2,355 | +319 | +15.67% |
| vivado | 32 | URAM288 blocks | 0 | 0 | +0 | N/A |
| quartus | 64 | Mapped block-memory bits | 2,793,512 | 7,222,952 | +4,429,440 | +158.56% |
| quartus | 64 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 64 | Estimated ALMs | 2,077 | 6,097 | +4,020 | +193.55% |
| quartus | 64 | Combinational ALUTs | 2,341 | 6,799 | +4,458 | +190.43% |
| quartus | 64 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 64 | Registers / FFs | 1,401 | 4,998 | +3,597 | +256.75% |
| vivado | 64 | BRAM36 tile equivalents | 120 | 240 | +120 | +100.00% |
| vivado | 64 | Allocated BRAM + URAM bits | 4,423,680 | 8,847,360 | +4,423,680 | +100.00% |
| vivado | 64 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 64 | CLB LUTs | 3,684 | 5,991 | +2,307 | +62.62% |
| vivado | 64 | LUTs used as RAM | 612 | 732 | +120 | +19.61% |
| vivado | 64 | Registers / FFs | 2,886 | 4,216 | +1,330 | +46.08% |
| vivado | 64 | URAM288 blocks | 0 | 0 | +0 | N/A |
| quartus | 128 | Mapped block-memory bits | 11,813,092 | 31,487,332 | +19,674,240 | +166.55% |
| quartus | 128 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 128 | Estimated ALMs | 4,068 | 9,593 | +5,525 | +135.82% |
| quartus | 128 | Combinational ALUTs | 4,424 | 15,179 | +10,755 | +243.11% |
| quartus | 128 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 128 | Registers / FFs | 1,499 | 8,996 | +7,497 | +500.13% |
| vivado | 128 | BRAM36 tile equivalents | 500 | 1,040 | +540 | +108.00% |
| vivado | 128 | Allocated BRAM + URAM bits | 18,432,000 | 38,338,560 | +19,906,560 | +108.00% |
| vivado | 128 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 128 | CLB LUTs | 7,362 | 14,035 | +6,673 | +90.64% |
| vivado | 128 | LUTs used as RAM | 832 | 1,072 | +240 | +28.85% |
| vivado | 128 | Registers / FFs | 6,107 | 8,707 | +2,600 | +42.57% |
| vivado | 128 | URAM288 blocks | 0 | 0 | +0 | N/A |
| quartus | 256 | Mapped block-memory bits | 49,841,312 | 136,315,552 | +86,474,240 | +173.50% |
| quartus | 256 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 256 | Estimated ALMs | 10,528 | 66,104 | +55,576 | +527.89% |
| quartus | 256 | Combinational ALUTs | 12,722 | 70,781 | +58,059 | +456.37% |
| quartus | 256 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 256 | Registers / FFs | 1,597 | 61,244 | +59,647 | +3734.94% |
| vivado | 256 | BRAM36 tile equivalents | 2,082.5 | 4,482.5 | +2,400 | +115.25% |
| vivado | 256 | Allocated BRAM + URAM bits | 76,769,280 | 165,242,880 | +88,473,600 | +115.25% |
| vivado | 256 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 256 | CLB LUTs | 26,859 | 37,402 | +10,543 | +39.25% |
| vivado | 256 | LUTs used as RAM | 942 | 1,542 | +600 | +63.69% |
| vivado | 256 | Registers / FFs | 17,826 | 18,221 | +395 | +2.22% |
| vivado | 256 | URAM288 blocks | 0 | 0 | +0 | N/A |
| quartus | 512 | Mapped block-memory bits | 209,715,932 | 587,203,292 | +377,487,360 | +180.00% |
| quartus | 512 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 512 | Estimated ALMs | 87,531 | 199,435 | +111,904 | +127.84% |
| quartus | 512 | Combinational ALUTs | 80,970 | 203,606 | +122,636 | +151.46% |
| quartus | 512 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 512 | Registers / FFs | 70,950 | 127,982 | +57,032 | +80.38% |
| vivado | 512 | BRAM36 tile equivalents | 8,647.5 | 19,215 | +10,567.5 | +122.20% |
| vivado | 512 | Allocated BRAM + URAM bits | 318,781,440 | 708,341,760 | +389,560,320 | +122.20% |
| vivado | 512 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 512 | CLB LUTs | 64,537 | 86,161 | +21,624 | +33.51% |
| vivado | 512 | LUTs used as RAM | 664 | 664 | +0 | +0.00% |
| vivado | 512 | Registers / FFs | 37,735 | 35,875 | -1,860 | -4.93% |
| vivado | 512 | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | BRAM36 tile equivalents | 35,850 | 81,937.5 | +46,087.5 | +128.56% |
| vivado | 1024 | Allocated BRAM + URAM bits | 1,321,574,400 | 3,020,544,000 | +1,698,969,600 | +128.56% |
| vivado | 1024 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 1024 | CLB LUTs | 219,107 | 345,710 | +126,603 | +57.78% |
| vivado | 1024 | LUTs used as RAM | 824 | 824 | +0 | +0.00% |
| vivado | 1024 | Registers / FFs | 84,909 | 76,037 | -8,872 | -10.45% |
| vivado | 1024 | URAM288 blocks | 0 | 0 | +0 | N/A |

Incomplete or failed points (not treated as zero resource usage):

- quartus, dynamic, 1024 vFlows: `synthesis_running`.

Resource counts exceeding the target device's reported capacity:

| Platform | vFlows | Configuration | Resource | Used | Available | Utilization |
|---|---:|---|---|---:|---:|---:|
| vivado | 128 | static | BRAM36 tile equivalents | 500 | 480 | 104.17% |
| vivado | 128 | dynamic | BRAM36 tile equivalents | 1,040 | 480 | 216.67% |
| vivado | 256 | static | BRAM36 tile equivalents | 2,082.5 | 480 | 433.85% |
| vivado | 256 | dynamic | BRAM36 tile equivalents | 4,482.5 | 480 | 933.85% |
| vivado | 512 | static | BRAM36 tile equivalents | 8,647.5 | 480 | 1801.56% |
| vivado | 512 | dynamic | BRAM36 tile equivalents | 19,215 | 480 | 4003.12% |
| vivado | 1024 | static | CLB LUTs | 219,107 | 216,960 | 100.99% |
| vivado | 1024 | static | BRAM36 tile equivalents | 35,850 | 480 | 7468.75% |
| vivado | 1024 | dynamic | CLB LUTs | 345,710 | 216,960 | 159.34% |
| vivado | 1024 | dynamic | BRAM36 tile equivalents | 81,937.5 | 480 | 17070.31% |

`device-capacity.csv` compares capacities present in the vendor reports. Passing these checks does not establish routability.
Quartus synthesis reports an ALM capacity but does not provide a fitted M20K allocation here.

Oversized Vivado estimates use a version-specific hook that skips the pre-mapping device-capacity check.
RTL and optimization passes are unchanged; reported device capacities remain physical limits. See each run manifest.
Control-probe resource counts were identical with and without the hook. No implementation was attempted.

Synthesis-only estimates; no routed fit or timing closure. The two vendors use different logic units.
The flow/vPIFO namespaces remain coupled, and the dense per-port/token tables grow quadratically.
