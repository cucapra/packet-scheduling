# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem. Running example: a small-office gateway runs `SP(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic. The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `SP(gmail, zoom, spotify)`: just extend the strict-priority list with `spotify` having lowest priority.
  2. `SP(gmail, RR(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, SOTA would stop the world, drop/recirculate buffered packets, recompile, and reinstall. Costs: dropped or recirculated (read, delayed!) packets, downtime, and a full respawn of nodes that did not need respawning.

- The alternate is to reprogram a scheduler without stopping the world. Let's revisit the examples from earlier.
- Transitioning to `SP(gmail, zoom, spotify)` is actually quite easy. We can achieve the following gold standard:
  - time1: `SP(gmail, zoom)` is running
  - time2: the request to move to `SP(gmail, zoom, spotify)` is received. `SP(gmail, zoom)` is still running
  - time3: we move to `SP(gmail, zoom, spotify)`. Whatever user-observable interaction (push/pop) happened immediately before time3 happened entirely in the `SP(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened entirely in the `SP(gmail, zoom, spotify)` regime. We therefore refer to the transition at time3 as _atomic_.
  - [AM note: time3 = time2 in this case? I don't want to make that change here since it would conflate atomicity with immediacy, but do we think someone will scratch their head and ask why any time is needed at all?]
- What about transitioning to `SP(gmail, RR(zoom, spotify))`? It is not as easy. We atomically step into a _transitionary period_ during which the scheduler still accepts and emits packets, and once certain well-defined conditions are met we atomically step into the user-requested policy.
- It is crucial to note that, although the user never described to us the semantics of the transitionary period, it _is_ in fact a _de facto_ packet scheduling regime with some semantics! It is useful to recognize it as a scheduling policy in its own right (we give it the name `link`). There are clearly better and worse transitions from a network operator's point of view. SOTA has more-or-less unintentionally adopted a trivial stop-the-world `link`. Our contributions include both being clear about what `link` is and improving on it.
- Concretely:
  - Obligation 1. Formalize the semantics of `link`. This is tricky because the transition period is motivated by grody hardware-level manipulations _on the way to realizing `next`_; it is not designed to have a clean human-readable semantics.
  - Obligation 2. Improve upon SOTA. We will give the semantics of SOTA's stop-the-world `link` and use it as a baseline. We have a (set of) practical goals that may guide us as we search for the ideal way to transition from some `prev` to some `next`. Can we minimize the length of the transition period? Can we avoid dropped/delayed packets? [AM: more to come here; the cost model is a legit open question!]. We will show that it is always possible to make a transition from any `prev` to any `next`, but it is occasionally possible to make a very efficient transition. We contribute a tool that achieves this and shows [improvements].

## 2. Background & Motivation

### 2.1 Hierarchical scheduling on hardware

- Packet scheduling, PIFOs, and hierarchical scheduling with PIFO trees.
- Mapping a hierarchy onto hardware: each tree depth lands on a PE; siblings and cousins share a PE. This has been known since Sivaraman (SIGCOMM '16).

### 2.2 vPIFO, and the problem it leaves open

- vPIFO (Zhang et al., SIGCOMM 2024) is the closest related work, and it explicitly leaves our problem open.
- What vPIFO does. It virtualizes a single physical PIFO into many logical "PIFO instances," with a Scheduling Description Language (SDL) and compiler, so their PIFO Visor can _flexibly establish_ hierarchical PIFO trees of arbitrary shape on fixed hardware. Its contribution is the reconfigurable substrate.
- What vPIFO does not do. As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change. The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- Our two running examples make the gap concrete. The easy append, to get to `SP(gmail, zoom, spotify)`, is exactly the kind of edit vPIFO's substrate _could_ have absorbed in place: a targeted append to the Operation Generation Table and the PIFO Instance Address Table would register the one new PIFO instance and its operations, leaving every running instance untouched. vPIFO as published does not engage with this question; their only option is a full reinitialization. Driving such a substrate to install the edit in place, straight from the (small) diff, is exactly what our layer adds. The harder restructuring, to `SP(gmail, RR(zoom, spotify))`, is not even clearly within reach: it inserts an internal node and re-parents `zoom`, so on a substrate that (correctly) maps tree depth to PEs, some in-flight packets already sit under the old shape, and it is unclear that vPIFO's substrate could realize the change at runtime at all. vPIFO neither claims it can nor explains how it would.
- vPIFO's own §8 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly. We are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer _above_ a PIFO substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose.

## 3. Formalizing the Transition Phase `link`

We recap the PIFO-tree model (§3.1), fix a grammar of atomic policy diffs (§3.2), show that any production of the grammar keeps the live scheduler well-formed (§3.3), and argue that the guarantee survives down to hardware (§3.4).

### 3.1 Background: PIFO trees, formally

We build on the PIFO-tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3], so we review the pieces of their formalism that this paper actually leans on.

#### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO-tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.2 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

#### The two user-observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ user-visible interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of `push`/`pop` observability: every `push`/`pop` happens agains a well-defined scheduler, never a half-edited intermediate scheduler.

#### Well-formedness

A PIFO tree `q` is well-formed (`|- q`, following _Formal Abstractions_' notation) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. Lemma 3.9 of _FA_ shows that `push` always preserves `|- q`, and that `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

#### Control

_FA_ factors the scheduling _policy_ (which, given an arriving packet and the current state, produces the path that `push` will follow) into a _control_ object. A control over `t` is a triple `(s, q, z)`: a current state `s` drawn from some fixed set, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z` that, given a state and a packet, returns a path of the correct shape together with an updated state.

### 3.2 A Grammar for Tree Diffs

We fix a small grammar of structural edits between two well-formed policy trees `prev` and `next`. An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. Paths are interpreted in `next` for additions and in `prev` for removals; for in-place edits the path is unambiguous.

```
diff   ::= AddArm       (path, tree, weight?)
         | Quiesce      (path)
         | Designate    (path, tree)
         | RemoveArm    (path)
         | ChangeWeight (path, weight)
         | GraftInto    (tree_h)

path   ::= []  |  i :: path        // i is a child index
tree   ::= a well-formed policy subtree
tree_h ::= a well-formed policy tree with a single _hole_ (see below)
weight ::= a positive real
```

Every edit in this grammar is _atomic_: it carries a well-formed scheduler to a well-formed scheduler in a single step, with no transitionary period of its own (we make this precise in §3.3.1). This is a deliberate restriction. Edits that would have to destroy structure still holding packets are _not_ in the grammar: our one structural deletion, `RemoveArm`, fires only on an arm that is already empty. The richer reconfigurations a user may want (retiring an occupied arm, replacing an arm in place, pruning down to a subtree) are realized instead as _sequences_ of these atomic edits, separated by _exit conditions_; §3.3.1 makes the sequencing precise and §3.3.3 works the retirement case in full.

A _tree with a hole_ is an ordinary policy tree in which exactly one child slot holds a distinguished _hole_ rather than an arm. The hole is a reserved, empty slot, not an absence of a slot. Importantly, the policy at the _parent_ of the hole has an arity that includes the hole. `RR(A, B, hole)` runs a round-robin policy of arity three whose third slot is vacant; it is different from `RR(A, B)`, which just runs a round-robin policy of arity two. We write `tree_h[s]` for the ordinary, hole-free tree obtained by replacing the unique hole of `tree_h` with the subtree `s`; the fill is total, and well-formed whenever `s` and `tree_h` are well-formed.

Notes on the individual edits:

- `AddArm` carries a `weight` exactly when the slot it edits hangs off a WFQ parent, which needs a weight to schedule the arm; for any other parent the weight is absent.
- `Quiesce (path)` stops routing new traffic to the arm at `path`. It is a transaction-only edit: topology and contents are untouched, so the quiesced arm keeps being served and drains under load. It adds no structure and removes none.
- `Designate (path, tree)` spawns `tree` as a sibling of the subtree that `path` points to. The new tree is the _designated survivor_ of the original subtree: the new tree can receive new packets, but, while the original subtree has packets, the original subtree gets strict preference. Once the original subtree is empty, the `tree` takes its place.
- `RemoveArm (path)` structurally removes the subtree at `path`, dropping its slot and renumbering any higher siblings. The subtree must be _empty_: removing an occupied subtree would leave its parent with dangling indices.
- `ChangeWeight (path, weight)` targets the arm whose root sits at `path`; it overwrites the weight that arm's parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `GraftInto (tree_h)` fires when `prev` is to become a subtree of a larger `next`. The edit spawns `tree_h` and fills its single hole with `prev`, which keeps its in-flight contents. The ancestor nodes of `prev` in `tree_h[prev]` are populated with the (unexciting) indices required to make `tree_h[prev]` well-formed. The rest of the tree is spawned empty.

### 3.3 Edits Preserve Well-Formedness

[TK: pulled out to `closed.md` for reworking; will return here.]

### 3.4 Preserving this proof down to hardware

§3.3 proves soundness at the tree-diff level, where each atomic edit (`AddArm`, `Quiesce`, `RemoveArm`, ...) carries a well-formed tree to a well-formed tree. To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR: `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, `Map`, `Unmap`, `GC`, `Designate`, and the like. A single IR instruction, unlike a whole tree-diff edit, _can_ leave the tree malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, a class may be `Unmap`ped before its old subtree is `GC`ed, and so on.

We do not prove soundness at the IL level, but instead informally make the case for why the §3.3 proof survives the lowering. There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit. We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed. That commit is precisely how the substrate _realizes_ an atomic diff of §3.3.1: a diff was defined as an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant. Every `push`/`pop` therefore still lands on a well-formed control (`prev`, a `link`, or `next`), exactly as §3.3 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.3 proved. The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §5.

## 4. Identifying Better Transitions

§3 established that our grammar is _safe_: every atomic diff is a sound control replacement (§3.3.1), so any sequence the planner emits keeps the live scheduler well-formed at every instant and, once its exit conditions fire, lands in `next`. With safety settled, the question this section takes up is whether the planner wields the grammar _well_.

The transition planner's output is a sequence `diff ; (φ ; diff)*` (§3.3.1). Two things make one sequence better than another: how _confined_ it is, that is, how little of the running scheduler its diffs and intervening `link`s disturb; and how readily its exit conditions `φ` fire, that is, its liveness, which §3.3.1 left for us to pursue here.

There is always a fallback. To reach any `next` from any `prev`, the planner issues `Designate([], next)`, making `next` the survivor of the whole of `prev` (§3.2): new traffic flows to `next` at once, `prev` drains behind it, and a closing `RemoveArm` collapses onto `next`. This is our give-up-entirely option, with the splash zone as large as possible and no part of the scheduler left running undisturbed, but it always works and drops nothing. This section is the story of doing better: localize the change, so the sequence and its `link`s touch only a small subtree and the rest of the scheduler keeps running.

We make no claim that the planner is canonical or minimal. We claim only that whatever sequence it emits is safe (§3.3), and that the work of this section is to make those sequences confined and live.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself. TK.]

## 5. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- [AM: question for Zhiyuan: §3.4 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.3.1 diff. But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 6. Evaluation

## 7. Related Work

## 8. Conclusion
