# RIO dynamic configuration overhead: fixed hardware (PIFO excluded)

**RIO logic only; PIFO cores are excluded through explicit top-level interfaces.**
Percentages use the ordinary RIO logic as denominator, not the total scheduler.
PIFO storage, sorting, occupancy and drain detection require a separate resource budget.

Difference = dynamic − static; percentage uses static as the denominator.
Blank percentages mean a zero baseline. Only completed synthesis reports supply resource values.

| Platform | vFlows | Resource | Static | Dynamic | Absolute change | Change |
|---|---:|---|---:|---:|---:|---:|
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
