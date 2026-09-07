# RIO dynamic configuration overhead: fixed hardware

Difference = dynamic − static; percentage uses static as the denominator.
Blank percentages mean a zero baseline. Only completed synthesis reports supply resource values.

| Platform | vFlows | Resource | Static | Dynamic | Absolute change | Change |
|---|---:|---|---:|---:|---:|---:|

Incomplete or failed points (not treated as zero resource usage):

- quartus, static, 1024 vFlows: `cancelled_for_scope_change`.
- quartus, dynamic, 1024 vFlows: `cancelled_for_scope_change`.
- vivado, static, 1024 vFlows: `synthesis_running_failed`. ERROR: [Synth 8-5834] Design needs 104980 RAMB18 which is more than device capacity of 960
- vivado, dynamic, 1024 vFlows: `synthesis_running_failed`. ERROR: [Synth 8-5834] Design needs 197155 RAMB18 which is more than device capacity of 960

RAM requirements reported by failed synthesis capacity checks:

**These are diagnostic counts before successful synthesis completion, not final utilization reports.**

| Platform | vFlows | Resource | Static required | Dynamic required | Difference | Change | Device capacity |
|---|---:|---|---:|---:|---:|---:|---:|
| vivado | 1024 | RAMB18 equivalents | 104,980 | 197,155 | +92,175 | +87.80% | 960 |

`device-capacity.csv` compares capacities present in the vendor reports. Passing these checks does not establish routability.
Quartus synthesis reports an ALM capacity but does not provide a fitted M20K allocation here.

Synthesis-only estimates; no routed fit or timing closure. The two vendors use different logic units.
The flow/vPIFO namespaces remain coupled, and the dense per-port/token tables grow quadratically.
