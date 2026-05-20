# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.
- The reconfiguration problem. **Running example:** a small-office gateway runs `prev = SP[business, leisure]`, where `business = WFQ[(zoom, 2), (gmail, 1)]` and `leisure = RR[theonion, mastodon]`. The operator wants to add `spotify` to the leisure mix: `leisure' = RR[theonion, mastodon, spotify]`. Intuitive, but SOTA would stop the world, drain/drop, recompile, and reinstall. Costs: dropped or recirculated (read, delayed!) packets, downtime, full respawn of nodes that did not need it.
- The alternate is to reprogram a scheduler without stopping the world. That is, _if_ the diff between policies `prev` and `next` is straightforwardly described, can we make a straightforward adjustment in hardware, sparing ourselves some of the costs that SOTA suffers?
- The change from `prev` to `next` will not, in general, be atomic. We may atomically enter into a _transitionary period_ during which the scheduler still accepts and emits packets. After some conditions are met (say, some queue runs empty for the first time), we may atomically transition out of this transitionary period and into the user-requested policy `next`.
- It is crucial to note that, although the user never described to us the semantics of the transitionary period, it _is_ in fact a packet scheduling regime with some semantics! It is useful to dub it a scheduling policy in its own right and give it the name `link`. Two observations (and contributions) fall out of this.
  - There hasn't been work on pinning down the semantics of the scheduler during a transition. This is excusable: SOTA essentially _turns its scheduler off_ during the transition, so its semantics would be trivial. Since we want on-the-fly transitions, we have a new formalization obligation: write down the semantics of `link`. This is obligation 1. This is not easy; the transition period is motivated by grody hardware-level concerns _on the way to realizing `next`_; it is not meant to have a clean human-readable semantics!
  - We also gain a (set of) practical goals that may guide us as we search for the ideal way to transition from `prev` to `next`. Can we minimize the length of the transition period? Can we avoid dropped/delayed packets? [AM: more to come here; the cost model is a legit open question!]. This is obligation 2. We will show that it is always possible to make a transition from any `prev` to any `next`, but it is sometimes possible to make a very efficient transition. We contribute a tool that achieves this and shows [improvements].

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

### 2.3 Revisiting the running example

- Revisit the example, show more concretely what makes it hard, and point into §3-§6 or whatever. Tbh I think we may need to change the example because the present one probably _can_ be realized totally atomically.
