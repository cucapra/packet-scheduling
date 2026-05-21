# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem. Running example: a small-office gateway runs `SP(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic. The operator wants to add a new `spotify` flow, and has two natural ways to do it:
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
- What vPIFO does not do: it leaves the entire runtime story open. As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change. The configuration tables it relies on (the Operation Generation Table and the PIFO Instance Address Table) are described only for the initialization stage. The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- vPIFO's own §8 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship to state plainly: we are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer above a substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose!

### 2.3 Revisiting our running examples

- Moving from `SP(gmail, zoom)` to `SP(gmail, zoom, spotify)`. This is the easy case: the diff is a leaf append, and the existing siblings' ranks are preserved. In principle, a substrate could install the new child without a transitionary period. In vPIFO's setup, this could be a targeted append to the Operation Generation Table and PIFO Instance Address Table, leaving the rest of the scheduler running. vPIFO as published cannot do this, but for a more basic reason than one might guess: it offers no runtime update mechanism at all, cheap or otherwise. Even when the user's intent is a single leaf append, there is no published path from "new SDL program" to "live hardware" that does anything finer than a full reinitialization. That is what our layer adds: given the (small) diff here, we can drive a compatible substrate to install it in place.
- Moving from `SP(gmail, zoom)` to `SP(gmail, RR(zoom, spotify))` is where the real problem lives. The diff is structural: an internal node appears between `SP` and `zoom`, and `zoom` is re-parented. No single atomic operation realizes this on hardware that maps depth-to-PE; some in-flight packets are already enqueued under the old shape. We must atomically enter a transitionary `link` policy that is well-defined on both old and new arriving packets, drop or recirculate the residue, and then atomically jump to the user's requested policy.

## 3. Formalizing the Transition Phase `link`

### 3.1 Background: PIFO trees, formally

We build on the PIFO-tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3], so we review the pieces of their formalism that this paper actually leans on.

**Topology vs. contents.** A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO-tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation is key to making the diff grammar of §3.2 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

**The two user-observable operations.** `push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when admitting the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, and finally emitting a packet from the leaf. These are the _only_ user-visible interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of them: every push/pop is observed under exactly one of `prev` or `next`, never a half-edited intermediate scheduler.

**Well-formedness.** A PIFO tree `q` is well-formed (`|- q`) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck: an index `i` is enqueued in the parent _exactly when_ there is a packet underneath waiting to be released by it. Lemma 3.9 of _Formal Abstractions_ shows that `push` always preserves `|- q`, and that `pop` does too whenever `q` is non-empty (which is precisely the condition under which `pop` is defined). Our §3.3 proof obligation is the analogous statement one level up: each structural edit in the §3.2 grammar, applied to a well-formed `prev`, yields a well-formed `next`.

**Control.** _FA_ factors the scheduling _policy_ (which, given an arriving packet and the current state, produces the path that `push` will follow) into a _control_ object. A control over `t` is a triple `(s, q, z)`: a current state `s` drawn from some fixed set, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z` that, given a state and a packet, returns a path of the right topology together with an updated state. We inherit this factoring without modification; the reconfiguration problem this paper tackles is the change of `q`'s topology, with the rest of the control updated in lockstep.

### 3.2 A Grammar for Tree Diffs

We fix a small grammar of structural edits between two well-formed policy trees `prev` and `next`. An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. Paths are interpreted in `next` for additions and in `prev` for removals; for in-place edits the path is unambiguous.

```
diff   ::= Same
         | ArmAdded      (path, arm, weight?)
         | ArmRemoved    (path, arm)
         | WeightChanged (path, weight)
         | ArmReplaced   (path, arm, weight?)
         | SuperPol      (path)
         | SubPol        (path)

path   ::= []  |  i :: path        // i is a child index
arm    ::= a well-formed policy subtree
weight ::= a positive real
```

`ArmAdded` and `ArmReplaced` carry a `weight` exactly when the slot they edit hangs off a WFQ parent, which needs a weight to schedule the arm; for any other parent the weight is absent. Our implementation splits each of these into two constructors, one with the weight and one without (`ArmAdded`/`ArmAddedWFQ`, `ArmReplaced`/`ArmReplacedWFQ`), so the field is never an ill-typed optional; we fold them here to keep the presentation light.

A note on the more subtle variants:

- `WeightChanged (path, weight)` targets the arm whose root sits at `path`; it overwrites the weight that arm's parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `SubPol (path)` fires when `next` appears verbatim as a subtree of `prev` at `path`. The transition prunes `prev` down to that subtree and discards everything around it. The retained subtree keeps its in-flight contents; the surrounding structure is garbage-collected.
- `SuperPol (path)` is the converse: it fires when `prev` appears verbatim as a subtree of `next` at `path`. The transition grafts the new surrounding structure around `prev`, which is re-parented at `path` and keeps its in-flight contents.
- `SubPol`/`SuperPol` need neither a constructor nor a description of the displaced/added structure: the patcher recovers everything it needs from `prev` and `next` themselves, which it already has.

Let us revisit the two transitions from §1 and see what the sniffer produces.

- `SP(gmail, zoom)` -> `SP(gmail, zoom, spotify)`. The two roots agree on constructor (`SP`); their child lists differ only in that `spotify` has been appended at index 2. The sniffer returns `ArmAdded { path = [2]; arm = spotify }`. This is the easy case: one well-localized edit, no give-up.
- `SP(gmail, zoom)` -> `SP(gmail, RR(zoom, spotify))`. The roots again agree on constructor, and the child lists differ at exactly one slot: index 1, where the leaf `zoom` has been swapped for the subtree `RR(zoom, spotify)`. The sniffer recurses into the slot and, finding no finer _single_ edit, falls back to a slot-level replacement: `ArmReplaced { path = [1]; arm = RR(zoom, spotify) }`. This is a kind of give-up, but a localized one! The path `[1]` tells the substrate that everything outside child 1 of the root is untouched. The transitionary `link` only needs to handle the residue under `zoom`, so `gmail` and the root `SP` keep running.

Note that this is a _single-edit sniffer._ Every non-`Same` variant describes exactly one structural change at one path. A multi-arm divergence is not expressible directly; the diff-sniffer collapses it to `ArmReplaced` at the closest enclosing path, with `path = []` meaning "I could not localize it any further than the whole tree." Indeed, above we could have "succeeded with": `ArmReplaced { path = []; arm = SP(gmail, RR(zoom, spotify)) }`. In general we can _always_ succeed with `ArmReplaced { path = []; arm = _the whole new tree_}`. This is functionally the give-up token; §4 is in large part the story of pushing give-ups deeper into the tree and therefore minimizing the splash zone of the change.

We make no claim that the sniffer is canonical or minimal; we claim only that whatever it returns is a sound description of the transition from `prev` to `next`, and that §3.3 will show every variant absorbs cleanly into a well-formed tree.

### 3.3 Edits Preserve Well-Formedness

Every transition from `prev` to `next` factors through a third regime that we name `link`:

```
prev  --atomic-->  link  --atomic-->  next
```

We call each atomic regime-change a _snap_. The two arrows are atomic in the §1 sense: every user-observable push/pop happens entirely under `prev`, `link`, or `next`, and never straddles a snap. `link` is itself a packet-scheduling regime over a well-formed PIFO tree, with its own `push`/`pop`. For many edits `link` is _degenerate_: zero-duration, sharing topology and contents with `next` at the instant the entry snap completes, so the two arrows fuse and `prev` abuts `next` directly. For others `link` is substantive, with real duration and stated `push`/`pop`, and the exit snap waits for some stated enabling condition.

Casting the proof in this shape buys us two things.

- First, the well-formedness obligation always has the same three slots: (i) the entry snap preserves the invariant from §3.1, (ii) `link` is itself closed under its own `push`/`pop` over a well-formed tree, and (iii) the exit snap, the moment its condition fires, preserves the invariant on the way into `next`. When `link` is degenerate, (ii) is vacuous and (i) and (iii) merge.
- Second, several edits admit more than one realization. These differ in whether `link` is degenerate or substantive, what `link`'s `push`/`pop` actually do, and what the exit condition is, and they carry different lossiness and latency tradeoffs. That "menu" is part of the contribution (see §3.3.2); SOTA exposes only one option (stop-the-world over the whole tree, which in our vocabulary is the maximally pessimistic `link`).

We use `ArmAdded` as a warm-up, develop the full menu for `ArmRemoved`, and then point out what is interesting about the remaining variants.

#### 3.3.1. `ArmAdded(path, arm, weight?)` (warm-up; degenerate `link`).

Example: `SP(gmail, zoom)` --atomic--> `SP(gmail, zoom, spotify)`.
The diff computed according to the grammar in §3.2 is: `ArmAdded { path = [2]; arm = spotify }`. This is an example where the `link` is degenerate and so we have fused to two snaps into one.

We work entirely with the control triple `(s, q, z)` of §3.1 and develop the new control `(s', q', z')` that the snap installs. Write `c` for the parent node named by `path` (here the root `SP`) and `k` for the index of the new slot (here `2`).

_The tree, `q -> q'`._ At `c = Internal(qs, p)` we splice the new arm into the child list at index `k`: `q' = Internal(qs', p')`, where `qs'` is `qs` with a fresh PIFO tree `a` inserted at index `k`. Here `a` is the _empty_ PIFO tree having whatever topology `arm` described: an empty packet-PIFO if `arm` is a leaf, and empty index-PIFOs throughout if it is internal. The children formerly at indices `>= k` shift up by one to make room (in an append, `k` is last, so nothing shifts). The parent's index-PIFO `p` becomes `p'` only to follow that renumbering: every entry naming an old index `>= k` is bumped up by one. This relabels index _values_ but moves no packet and changes no rank. Immediately after the splice, no entry of `p'` names `k`: in an append because `p` only ever named the old indices `0..k-1`, and in a middle insert because the old `>= k` entries were all bumped past `k`.

_The transaction, `z -> z'`._ `z'` is the scheduling transaction compiled from `next`. It is a _conservative extension_ of `z`: for any packet that does not classify into the new arm it returns the identical decorated path and ranks that `z` did, and for a packet that does classify into the new arm it returns a path whose step at `c` selects `k` and then descends through `a`. Because `k` was not a legal index under `prev`, `z` could never have emitted such a path, so the two transactions differ only on routes that land in the previously-nonexistent slot.

_The state, `s -> s'`._ `s'` agrees with `s` everywhere, except that it records the initial local state for the new slot: the new arm's own scheduling state at its from-scratch value (it is empty), plus whatever per-slot bookkeeping `c`'s discipline keeps (an RR cursor entry, or, for the WFQ flavor, the slot's weight taken from the edit's `weight?` together with a zeroed virtual-finish accumulator). No existing slot's state is disturbed.

_The snap is atomic, and `link` is degenerate._ Two facts do the work. First, `q'` is well-formed: at slot `k` the parent has exactly zero occurrences of `k` (no entry of `p'` names `k`, as established above) and the new arm `a` holds zero packets (it is empty), so the well-formedness obligation (§3.1) reads `0 = 0`, and every other slot inherits its obligation verbatim from `|- q` (the renumbering carried each old slot's matched count of occurrences and packets up together). So `q'` needs no repair. Second, no in-flight packet straddles the snap: every packet resident at the snap instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`. Hence a `pop` issued immediately after the snap returns exactly what a `pop` issued immediately before would have, ranked identically, since the empty new slot contributes nothing; and the first `push` governed by `z'` that routes to `k` is the first packet ever to occupy `a`, which Lemma 3.9 admits while preserving `|- q'`. There is nothing for a transitionary regime to do: the two snaps of the general picture fuse, `link` has zero duration, and we pass directly from `prev`'s control to `next`'s. This is the formal counterpart of the "easy case" of §2.3.

### Preserving this proof down to hardware

After this, there is a simple and mechanical compilation from the tree diff language to the IL-gen language. At the IL-gen language level, primitives look like `spawn`, `adopt`, etc, and it _is_ possible to create malformed trees. But we can informally argue that our proof at the tree-diff level carries over to the IL level because:

- We show our command-to-commands compilation, and assert that it's uncontroversial.
- We wrap IL-level commands into transactional commits, so that no user would actually get to see the malformed state (and, key to our point, no push/pop would be able to reach the scheduler when it is malformed).

We reuse this trick to again argue that our proof is preserved from the IL level to the h/w level.

## 4. Identifying Better Transitions

Now that we're on firm ground, we can go back and revisit our diff-sniffer. Our give-up case is the whole-tree `ArmReplaced { path = []; arm = next }`; §3.3 showed that this routes through the maximally pessimistic `link`, whose realizations span a full-tree drain (lossless, slow) and a stop-the-world drop (lossy); the latter is SOTA. Either way the blast zone is the entire tree. Here we show how we can identify diffs more precisely, and how we can give up at a deeper part of the tree if we need to, so that the transition routes through a `link` confined to a small subtree and leaves the rest of the scheduler running.

There are lots of examples to show here, and possibly some beefing-up of compare.ml itself. TK.

## 5. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.

## 6. Evaluation

## 7. Related Work

## 8. Conclusion
