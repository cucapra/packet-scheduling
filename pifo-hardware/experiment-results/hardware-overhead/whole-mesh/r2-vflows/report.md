# RIO resource scaling with the vFlow namespace

Difference = dynamic − static; percentage uses static as the denominator.
Blank percentages mean a zero baseline. Only completed synthesis reports supply resource values.

| Platform | vFlows | Resource | Static | Dynamic | Absolute change | Change |
|---|---:|---|---:|---:|---:|---:|
| quartus | 32 | Mapped block-memory bits | 659,532 | 1,644,972 | +985,440 | +149.42% |
| quartus | 32 | DSP blocks | 0 | 0 | +0 | N/A |
| quartus | 32 | Estimated ALMs | 217,237 | 227,676 | +10,439 | +4.81% |
| quartus | 32 | Combinational ALUTs | 336,940 | 362,361 | +25,421 | +7.54% |
| quartus | 32 | MLAB memory bits | 0 | 0 | +0 | N/A |
| quartus | 32 | Registers / FFs | 108,993 | 110,800 | +1,807 | +1.66% |
| vivado | 32 | BRAM36 tile equivalents | 30 | 60 | +30 | +100.00% |
| vivado | 32 | Allocated BRAM + URAM bits | 1,105,920 | 2,211,840 | +1,105,920 | +100.00% |
| vivado | 32 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 32 | CLB LUTs | 512,909 | 513,506 | +597 | +0.12% |
| vivado | 32 | LUTs used as RAM | 532 | 592 | +60 | +11.28% |
| vivado | 32 | Registers / FFs | 109,975 | 110,340 | +365 | +0.33% |
| vivado | 32 | URAM288 blocks | 0 | 0 | +0 | N/A |
| vivado | 64 | BRAM36 tile equivalents | 120 | 240 | +120 | +100.00% |
| vivado | 64 | Allocated BRAM + URAM bits | 4,423,680 | 8,847,360 | +4,423,680 | +100.00% |
| vivado | 64 | DSP blocks | 0 | 0 | +0 | N/A |
| vivado | 64 | CLB LUTs | 505,397 | 517,460 | +12,063 | +2.39% |
| vivado | 64 | LUTs used as RAM | 612 | 732 | +120 | +19.61% |
| vivado | 64 | Registers / FFs | 121,046 | 122,385 | +1,339 | +1.11% |
| vivado | 64 | URAM288 blocks | 0 | 0 | +0 | N/A |

Incomplete or failed points (not treated as zero resource usage):

- quartus, static, 64 vFlows: `cancelled_for_scope_change`.
- quartus, dynamic, 64 vFlows: `cancelled_for_scope_change`.
- quartus, static, 128 vFlows: `cancelled_for_scope_change`.
- quartus, dynamic, 128 vFlows: `cancelled_for_scope_change`.
- vivado, static, 128 vFlows: `cancelled_for_scope_change`.
- vivado, dynamic, 128 vFlows: `cancelled_for_scope_change`.
- quartus, static, 256 vFlows: `prepared`.
- quartus, dynamic, 256 vFlows: `prepared`.
- vivado, static, 256 vFlows: `cancelled_for_scope_change`.
- vivado, dynamic, 256 vFlows: `cancelled_for_scope_change`.
- quartus, static, 512 vFlows: `prepared`.
- quartus, dynamic, 512 vFlows: `prepared`.
- vivado, static, 512 vFlows: `prepared`.
- vivado, dynamic, 512 vFlows: `prepared`.
- quartus, static, 1024 vFlows: `cancelled_for_scope_change`.
- quartus, dynamic, 1024 vFlows: `cancelled_for_scope_change`.
- vivado, static, 1024 vFlows: `synthesis_running_failed`. ERROR: [Synth 8-5834] Design needs 104980 RAMB18 which is more than device capacity of 960
- vivado, dynamic, 1024 vFlows: `synthesis_running_failed`. ERROR: [Synth 8-5834] Design needs 197155 RAMB18 which is more than device capacity of 960

RAM requirements reported by failed synthesis capacity checks:

**These are diagnostic counts before successful synthesis completion, not final utilization reports.**

| Platform | vFlows | Resource | Static required | Dynamic required | Difference | Change | Device capacity |
|---|---:|---|---:|---:|---:|---:|---:|
| vivado | 1024 | RAMB18 equivalents | 104,980 | 197,155 | +92,175 | +87.80% | 960 |

Resource counts exceeding the target device's reported capacity:

| Platform | vFlows | Configuration | Resource | Used | Available | Utilization |
|---|---:|---|---|---:|---:|---:|
| vivado | 32 | static | CLB LUTs | 512,909 | 216,960 | 236.41% |
| vivado | 32 | dynamic | CLB LUTs | 513,506 | 216,960 | 236.68% |
| vivado | 64 | static | CLB LUTs | 505,397 | 216,960 | 232.94% |
| vivado | 64 | dynamic | CLB LUTs | 517,460 | 216,960 | 238.50% |

`device-capacity.csv` compares capacities present in the vendor reports. Passing these checks does not establish routability.
Quartus synthesis reports an ALM capacity but does not provide a fitted M20K allocation here.

Synthesis-only estimates; no routed fit or timing closure. The two vendors use different logic units.
The flow/vPIFO namespaces remain coupled, and the dense per-port/token tables grow quadratically.
