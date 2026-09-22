# Python compiler and input formats

The tree compiler turns a declarative tree change into controller instructions.
The simulator consumes those instructions together with a separate traffic JSON.
Run the commands below from `pifo-hardware/`.

This directory contains only shared tools. Experiment runners, specialized
compilers, validation, and plotting live beside the JSONs under
[experiments/](../../experiments/README.md).

| Tool | Input | Output |
| --- | --- | --- |
| [pifo_tree_compiler.py](pifo_tree_compiler.py) | `tree-move.json` | `transactions.txt` |
| [pifo_simulator.py](pifo_simulator.py) | `transactions.txt` and `traffic.json` | Request, packet-outcome, and event CSVs |

[pifo_config.py](pifo_config.py) defines the shared tree, policy-change, and traffic
types. It has no dependency on experiment configuration or plotting. The
transaction and traffic file formats are handled by
[pifo_transaction_program.py](pifo_transaction_program.py) and
[pifo_traffic_program.py](pifo_traffic_program.py).

Keep authored inputs under `experiments/<name>/`: `tree-move.json`, `traffic.json`,
and optional runner-specific `settings.json`. Multi-edit and scalability use
`request.json` with their specialized compilers. Older RR/SP cases bundle traffic,
tree changes, simulation, and plotting settings in one experiment JSON; those
files are consumed by `pifo_experiment_figures.py`, not the tree compiler directly.

Generated instructions, traces, reports, and figures belong under the ignored
`experiment-results/<name>/`. A hand-authored direct instruction program is source
and belongs under `experiments/<name>/transactions.txt`.
See the [experiment guide](../../experiments/README.md) to run complete suites.

## Compile a tree move

Compilation needs Python 3.10 or newer and only the standard library:

```bash
python3 hw/python/pifo_tree_compiler.py \
  --input experiments/motivating-example/r4-confined/tree-move.json \
  --output experiment-results/compiler-example/transactions.txt
```

## Tree-move JSON

A `pifo-tree-move-v1` file has four fields: `schema`, `hardware`, `old_tree`,
and `move`. It describes one transition. This complete example changes
`SP(A, B)` into `SP(A, RR(B, C))`: flow 1 keeps its path, flow 2 moves under a new
RR group, and flow 3 joins that group.

```json
{
  "schema": "pifo-tree-move-v1",
  "hardware": {
    "num_engines": 3,
    "num_vpifos": 8,
    "max_packet_priority": 256,
    "fifo_depth": 4,
    "prefetch_buffer_depth": 2
  },
  "old_tree": {
    "root": "root",
    "nodes": {
      "root": {
        "engine_id": 1, "vpifo_id": 1, "policy": "SP",
        "flow_state": {"1": 1, "2": 2}
      },
      "a": {"engine_id": 2, "vpifo_id": 1, "policy": "FIFO"},
      "b": {"engine_id": 2, "vpifo_id": 2, "policy": "FIFO"}
    },
    "flow_paths": {
      "1": ["root", "a"],
      "2": ["root", "b"]
    }
  },
  "move": {
    "type": "policy_change",
    "mode": "confined_transitive",
    "cycle": 100,
    "name": "add-group",
    "target_tree": {
      "root": "root",
      "nodes": {
        "root": {
          "engine_id": 1, "vpifo_id": 1, "policy": "SP",
          "flow_state": {"1": 1, "2": 2, "3": 2}
        },
        "a": {"engine_id": 2, "vpifo_id": 1, "policy": "FIFO"},
        "group": {"engine_id": 2, "vpifo_id": 2, "policy": "RR"},
        "b": {"engine_id": 3, "vpifo_id": 1, "policy": "FIFO"},
        "c": {"engine_id": 3, "vpifo_id": 2, "policy": "FIFO"}
      },
      "flow_paths": {
        "1": ["root", "a"],
        "2": ["root", "group", "b"],
        "3": ["root", "group", "c"]
      }
    }
  }
}
```

`nodes` assigns policies and physical engine/vPIFO locations to logical names;
`flow_paths` defines the topology. A path starts at `root` and visits at most one
node per engine. Policies are `FIFO`, `RR`, `WFQ`, and `SP`. For SP, `flow_state`
must give every flow on that node a priority in `1..max_packet_priority-1`;
smaller values run first. `flow_state` defaults to an empty object.

`type` remains `policy_change` for topology changes too. `cycle` schedules the
installation package's start; the commit publishes later after its instructions
execute. `name` identifies the transaction. Optional `before_label` and
`after_label` label reports and default to the old/new root policies.

## Move modes

`move.mode` selects the transition strategy and defaults to `full_transitive`.

| Mode | Behavior | Constraints |
| --- | --- | --- |
| `in_place` | Configures added nodes and flow mappings without copying the tree. | Existing node placement/policy and existing flow paths must stay unchanged; added nodes must be reachable by added flows. |
| `full_transitive` | Allocates fresh vPIFOs for every target node. New admissions enter the new tree; dequeue requests switch through a front rewrite when the old root drains. | Old and new roots must be on the same engine; enough free vPIFOs must exist. |
| `confined_transitive` | Copies the changed subtree and preserves its ancestors and unaffected branches. The rewrite is installed at the subtree boundary. | Requires an explicit target, at least one changed existing flow path, a preserved ancestor, and one shared old/new boundary on the same engine. An unchanged flow cannot share the retired boundary. Added flows must enter the new boundary. |
| `stop_the_world` | The simulator pauses admission/dequeue, captures buffered requests, resets/configures the mesh, replays retained tokens, and resumes. Sources continue generating into the closed admission gate. | Requires an explicit target with the original physical root. Optional `minimum_stop_cycles` sets the minimum capture-to-resume interval. |
| `stop_the_world_pop` | Hardware stops traffic while a priority-1 prefill constructs an SP wrapper that serves the old tree before the new tree. Commit publishes the wrapper root and releases traffic. | Requires a spare engine unused by both trees, room for copied nodes/wrapper tokens, and the evaluation hardware image. |

For the example, `confined_transitive` preserves `root` and `a`, and rewrites old
`b` to the newly allocated `group`. `full_transitive` copies the entire target;
`stop_the_world` also accepts it. `in_place` rejects the changed path/placement of
`b`. `stop_the_world_pop` needs `num_engines: 4` to provide the unused wrapper PE.

The compiler detects a confined boundary from differing `flow_paths` and their
common unchanged ancestors; it does not infer arbitrary policy-only subtree
changes. For policy-only changes, use `full_transitive` or a suitable reset plan.

Copied nodes keep their target `engine_id`, but the compiler chooses fresh
`vpifo_id`s. Target IDs may therefore overlap old IDs in the JSON. vPIFO 0 is the
null sink; the highest ID is reserved for empty-PIFO output. Configured nodes use
IDs `1..num_vpifos-2`, and flow IDs must be below `num_vpifos-1`. The current tree
compiler rejects removal of existing flows. `num_vpifos * fifo_depth` must be a
power of two.

For a policy-only change with unchanged paths, replace the entire `move` object
with this compact form:

```json
{
  "type": "policy_change",
  "mode": "full_transitive",
  "cycle": 100,
  "name": "root-to-rr",
  "changes": {
    "root": {"policy": "RR"}
  }
}
```

`changes` updates named nodes and merges supplied `flow_state` entries with their
old state. Use either `changes` or `target_tree`, never both. The compact form is
supported by `full_transitive` and `stop_the_world_pop`; other modes require an
explicit `target_tree`.

## Compilation and cleanup

The output is a `pifo-transactions-v1` text timeline, not JSON. Compilation emits:

1. **Initialization:** configures `old_tree` before traffic starts.
2. **Installation:** configures the transition and ends with `CommitMapper`.
3. **Cleanup:** guards every retired FIFO, invalidates its old mappings/brain
   state, then commits. It preserves live front-rewrite aliases and shared
   ancestors. Additive and reset moves have no retired nodes and receive a
   commit-only cleanup.
4. **Wrapper reclamation, when needed:** `stop_the_world_pop` adds a third timed
   package after cleanup has published the direct target root.

Timed packages use the move's scheduled cycle and execute in order. `cleanupOf`
links cleanup/reclamation to the preceding package; initialization is separate.
Draining a root does not imply that every downstream FIFO has drained, so cleanup
guards each retired FIFO. Each package ends with exactly one `CommitMapper`.

The simulator consumes the emitted instructions directly. Mapper commits publish
staged pre/post mappings; brain writes are immediate and do not gain transactional
semantics from that commit. See the [direct transaction format and commands](../spinal/rio/sim/README.md#direct-transaction-semantics)
and [hardware transaction protocol](../spinal/README.md#transactional-configuration).

## Traffic JSON

Traffic is independent of the tree move. This complete `pifo-traffic-v1` example
generates flows 1 and 2 before cycle 100, then flows 1, 2, and 3:

```json
{
  "schema": "pifo-traffic-v1",
  "seed": 7,
  "patterns": [
    {
      "name": "before",
      "flows": [1, 2],
      "packets_per_flow": 10,
      "start_cycle": 0,
      "packet_rate": {
        "distribution": "constant",
        "unit": "packets_per_cycle_per_flow",
        "value": 0.1
      },
      "packet_size_bytes": {"distribution": "constant", "value": 64}
    },
    {
      "name": "after",
      "flows": [1, 2, 3],
      "packets_per_flow": 20,
      "start_cycle": 100,
      "packet_rate": {
        "distribution": "constant",
        "unit": "packets_per_cycle_per_flow",
        "value": 0.1
      },
      "packet_size_bytes": {"distribution": "constant", "value": 64}
    }
  ]
}
```

Pattern names must be unique. Patterns may overlap; requests are merged by cycle
and assigned stable IDs. `packets_per_flow` is a per-flow count, and rate units
must be `packets_per_cycle_per_flow`. A constant rate of `0.1` generates one packet
per flow every ten cycles. Newly added flows wait at admission until installation
publishes their mappings.

Rate and size distributions support `constant` (`value`), `uniform` (`min`, `max`),
and `normal` (`mean`, `stddev`, `min`, `max`). Normal values are clamped to their
bounds, packet sizes are rounded to positive bytes, and rate/size use separate
seeded random streams.

## Run the simulator

To run the checked-in confined move compiled above with its existing traffic:

```bash
python3 hw/python/pifo_simulator.py \
  --transactions experiment-results/compiler-example/transactions.txt \
  --traffic experiments/motivating-example/traffic.json \
  --output-dir experiment-results/compiler-example \
  --queue-depth 4096 --link-bytes-per-cycle 16 --max-cycles 100000
```

Simulation needs JDK 17, sbt, and Icarus Verilog. Add `--evaluation-hardware --verilator` for the separate
hardware stop/prefill/copy image; `stop_the_world_pop`
requires it. Full experiment runners select their settings and backend themselves.
See the [simulator guide](../spinal/rio/sim/README.md) for output schemas, direct
commands, live feeding, and trace conversion.

## Core tests

The Python tests use inline inputs and the standard library. They cover tree
compilation, transaction validation/serialization, and request traces, with no
experiment results or plotting dependencies:

```bash
python3 -m unittest discover -s hw/python/tests
```

Hardware checks are documented in the [hardware README](../spinal/README.md#core-hardware-tests).
