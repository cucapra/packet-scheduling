# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem. Running example: a small-office gateway runs `SP(gmail, zoom)` — strictly prioritizing `gmail` traffic over `zoom` traffic. The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `SP(gmail, zoom, spotify)`: just extend the strict-priority list with `spotify` at the lowest rank.
  2. `SP(gmail, RR(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, SOTA would stop the world, drop/recirculate buffered packets, recompile, and reinstall. Costs: dropped or recirculated (read, delayed!) packets, downtime, and a full respawn of nodes that did not need respawning.

- The alternate is to reprogram a scheduler without stopping the world. Let's revisit the examples from earlier.
- Transitioning to `SP(gmail, zoom, spotify)` is actually quite easy. We can achieve the following gold standard:
  - time1: `SP(gmail, zoom)` is running
  - time2: the request to move to `SP(gmail, zoom, spotify)` is received. `SP(gmail, zoom)` is still running
  - time3: we move to `SP(gmail, zoom, spotify)`. Whatever user-observable interaction (push/pop) happened immediately before time3 happened in the `SP(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened in the `SP(gmail, zoom, spotify)` regime. We refer to the transition at time3 as _atomic_.
- What about transitioning to `SP(gmail, RR(zoom, spotify))`? It is not as easy. We atomically enter into a _transitionary period_ during which the scheduler still accepts and emits packets. After some conditions are met, we atomically leave this transitionary period and enter into the user-requested policy.
- It is crucial to note that, although the user never described to us the semantics of the transitionary period, it _is_ in fact a de facto packet scheduling regime with some semantics! It is useful to recognize it as a scheduling policy in its own right (give it the name `link`). There are clearly better and worse transitions from a network operator's point of view. SOTA has more-or-less unintentionally adopted a trivial stop-the-world `link`. Our contributions include both being clear about what `link` is and improving on it.
- Concretely:
  - Obligation 1. Formalize the semantics of `link`. This is tricky because the transition period is motivated by grody hardware-level concerns _on the way to realizing `next`_; it is not meant to have a clean human-readable semantics! We will also give the semantics of SOTA's stop-the-world `link` and use it as a baseline.
  - Obligation 2. Improve upon SOTA. We have a (set of) practical goals that may guide us as we search for the ideal way to transition from some `prev` to some `next`. Can we minimize the length of the transition period? Can we avoid dropped/delayed packets? [AM: more to come here; the cost model is a legit open question!]. We will show that it is always possible to make a transition from any `prev` to any `next`, but it is sometimes possible to make a very efficient transition. We contribute a tool that achieves this and shows [improvements].

## 2. Background & Motivation

### 2.1 Hierarchical scheduling on hardware

- Packet scheduling, PIFOs, and hierarchical scheduling with PIFO trees.
- Mapping a hierarchy onto hardware: each tree depth lands on a PE; siblings and cousins share a PE. This has been known since Sivaraman (SIGCOMM '16).

### 2.2 vPIFO, and the problem it leaves open

- vPIFO (Zhang et al., SIGCOMM 2024) is the closest related work, and it explicitly leaves our problem open.
- What vPIFO does: virtualizes a single physical PIFO into many logical "PIFO instances," with an SDL language + compiler, so the PIFO Visor can _flexibly establish_ hierarchical PIFO trees of arbitrary shape on fixed hardware. Its contribution is the reconfigurable substrate.
- What vPIFO does not do: it treats a policy change as a wholesale table update (Operation Generation Table + Address Table), with no notion of a minimal diff between old and new, no formal semantics, and no account of in-flight packets during a change. Its IR is for _rank computation_ compiled to P4/CPU, quite different from our structural/topological one.
- vPIFO's own §8 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." Their runtime interface (P4-runtime-style) was still under development at publication.
- The relationship to state plainly: we are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer above a substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose!

### 2.3 Revisiting our running examples

- Moving from `SP(gmail, zoom)` to `SP(gmail, zoom, spotify)`. This is the easy case: the diff is a leaf append, and the existing siblings' ranks are preserved. In principle, a substrate could install the new child without a transitionary period — a targeted append to the OGT and Address Table, leaving the rest of the scheduler running. In practice, vPIFO does not do this: its §8 prescribes "pause the packet scheduler, update the OGT and Address Table, and then resume" for _any_ policy change, structural or not. That is, even the easy case stops the world under vPIFO as published. The cheap path is left on the table because vPIFO has no notion of a diff between old and new policies; everything is a wholesale table swap. This is exactly the gap our layer fills: given the (small) diff here, we can drive the substrate to install it in place.
- Moving from `SP(gmail, zoom)` to `SP(gmail, RR(zoom, spotify))` is where the real problem lives. The diff is structural: an internal node appears between `SP` and `zoom`, and `zoom` is re-parented. No single atomic operation realizes this on hardware that maps depth-to-PE; some in-flight packets are already enqueued under the old shape. We must enter a transitionary `link` policy that is well-defined on both old and new arriving packets, drain or migrate the residue, and then atomically jump to the user's request.

## 3. Formalizing the Transition Phase `link`

### 3.1 Review §3 of _Formal Abstractions_.

### 3.2 A Grammar for Tree Diffs

Introduce `compare.ml`, done up as a BNF.

### 3.3 Preserving Wins (claude, you can rename this subsection)

Show how a well-formed tree in _Formal Abstractions_ would absorb a primitive from the BNF in 3.2 and become a new well-formed tree. This is the core proof.

At the IRL meeting on May 20 we agreed to do this proof _at this level_ (where primitives are coarse, like `ArmAdded`, `WeightChanged`, etc). A benefit of this choice is that _we believe that the core proof is tractible at this level!_

After this, there is a simple and mechanical compilation from the tree diff language to the IL-gen language. At the IL-gen language level, primitives look like `spawn`, `adopt`, etc, and _is_ possible to create malformed trees. But we can informally argue that our proof at the tree-diff level carries over to the IL level because:

- We show our command-to-commands compilation, and assert that it's uncontroversial.
- We wrap IL-level commands into transactional commits, so that no user would actually get to see the malformed state (and, key to our point, no push/pop would be able to reach the scheduler when it is malformed).

We reuse this trick to again argue that our proof is preserved from the IL level to the h/w level.

## 4. Identifying Better Transitions

Now that we're on firm ground, we can go back and revisit our diff-sniffer. SOTA is to stop the world. Our give-up case is to do a full-scale drain. Here we show how we can identify diffs more precisely, and thereby contain the transition to a smaller blast zone.

There are lots of examples to show here, and possibly some beefing-up of compare.ml itself.

## 5. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.

## 6. Evaluation

## 7. Related Work

## 8. Conclusion
