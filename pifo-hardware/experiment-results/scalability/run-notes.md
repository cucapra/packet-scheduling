# Scalability run provenance

All 40 cases were run on 2026-09-09 in one batch, finishing at 17:49 UTC.
No packet, instruction, or reconfiguration outcomes were carried over from
the pilots or other experiment families. The batch's sbt invocation took
1509 seconds, including elaboration and all simulations.

The source base was `b9c0e5ac76fb615fd2fad73ddb08bdcf7716bbf7`, plus the
`RequestSimulatorCli.scala` changes included with this experiment: a larger
elaboration-width limit and optional compiled-model reuse. The compiler,
replay logic, and baseline algorithms were unchanged. All four mechanisms
used the same `EvaluationPifoMesh` image, which extends the merged `PifoMesh`
and uses its shared command FIFO, mapper replay, and drain guards.

Environment: sbt 1.10.2, Eclipse Adoptium JDK 17.0.20.1, SpinalHDL 1.12.3,
Verilator 5.039 (`v5.038-156-g3ca1c9b6d`). `JAVA_HOME` and `PATH` selected
JDK 17, with `JAVA_TOOL_OPTIONS='-Xmx6G -XX:ActiveProcessorCount=2'`.

Command, run from the repository's `pifo-hardware/` directory:

```sh
.venv/bin/python hw/python/pifo_scalability.py batch \
  --requests reweight add --tenants 2 16 4 8 32 \
  --runs rio control-p2 prefill reset
```

The [batch argument manifest](batch-arguments.txt) records each case's exact
Scala arguments. The ignored local `batch.log` contains all 40 start/done
pairs and simulator output. Each `doSim` creates fresh backend/simulator
state; only the compiled model is reused. For both requested R-reweight
pilots (m=2 and m=16), all six raw CSVs from the standalone run and its batch
rerun were byte-identical. The m=16 rerun followed other mechanisms in the
batch, also checking that their state did not leak into it.

Input and source fingerprints (SHA-256):

| File | SHA-256 |
| --- | --- |
| `hw/spinal/rio/sim/RequestSimulatorCli.scala` | `de0dfd69081261e87d32c523f7ec2d2ff2fec334c7f1624f072516a7574d5a39` |
| `hw/spinal/rio/ReplayControlFifo.scala` | `d43818d915394d3c9e7998e75a3184c191d718e02a522cf0f0b76fe257d3b3c4` |
| `hw/python/pifo_multiedit_compiler.py` | `affa0bdfb18486a95835800c5315b7c396ef54efa553697e78494bcd088d54c8` |
| `experiments/scalability/settings.json` | `a6b533d6e96e2fa66ca55c3dcfcff3bd6f70f65efe5817260b6820ec0d4b58be` |

All 40 executed transaction programs were checked against freshly compiled
requests. Every initial and target flow, including the arriving tenant's
two flows, was verified to have a separate hardware FIFO leaf.

The final CSV audit verified 146,712 generated and completed packets,
zero drops/reorderings, byte-identical source traces across the four runs
at each point, and identical pre-t1 backlog (75 packets, one still waiting
at the input). Every event-reported peak stop buffer matched an independent
reconstruction from packet generation/completion CSVs. Raw CSV timestamps
were all inside this batch's execution window.

The final summaries and three standalone figure folders were regenerated
from those raw CSVs after the batch. Rendering and reporting changes did
not change the simulated programs or outcomes.

The existing Python suite completed after figure generation: 75 tests run,
74 passed and one skipped, including standalone rendering of the new figure
folders. No new tests were added. The Scala CLI changes compiled and ran in
the full batch; whitespace validation also passed.
