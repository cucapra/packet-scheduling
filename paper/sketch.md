# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling.
  Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem.
  Running example: a small-office gateway runs `Strict(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic.
  The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `Strict(gmail, zoom, spotify)`: just extend the strict-priority list with `spotify` having lowest priority.
  2. `Strict(gmail, RoundRobin(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, the state of the art would stop the world, drop or recirculate buffered packets, recompile, and reinstall.
  The costs are dropped or recirculated (and so delayed) packets, downtime, and the rebuilding of nodes that did not need to be rebuilt.

- The alternative is to reprogram a scheduler without stopping the world.
  Let's revisit the examples from earlier.
- Transitioning to `Strict(gmail, zoom, spotify)` is actually quite easy.
  We can achieve the strongest property we could ask for:
  - time1: `Strict(gmail, zoom)` is running
  - time2: the request to move to `Strict(gmail, zoom, spotify)` is received.
    `Strict(gmail, zoom)` is still running
  - time3: we move to `Strict(gmail, zoom, spotify)`.
    Whatever user-observable interaction (push/pop) happened immediately before time3 happened entirely in the `Strict(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened entirely in the `Strict(gmail, zoom, spotify)` regime.
    We therefore refer to the transition at time3 as _atomic_.
  - [AM note: time2 = time3 in this case?
    I don't want to say it like that here, since it would conflate atomicity with immediacy, but do we think someone will scratch their head and ask why any time is needed at all?]
- What about transitioning to `Strict(gmail, RoundRobin(zoom, spotify))`?
  It is not as easy.
  We need to atomically step into a _transitionary period_ during which the scheduler still accepts and emits packets, and once certain well-defined conditions are met, we atomically step into the user-requested policy.
- Although the user never specified the semantics of this transitionary period, the period is itself a _de facto_ packet scheduling regime.
  It is worth recognizing as a scheduling policy in its own right; we call it `link`.
  Some transitions are better than others from a network operator's point of view, and the state of the art has, perhaps inadvertently, settled on a trivial stop-the-world `link`.
  Our contributions are to be clear about what `link` is and to improve on it.
- Concretely:
  - Obligation 1.
    Give a semantics to `link`.
    The transition period is shaped by hardware-level manipulations on the way to realizing `next`, and not by a clean human-readable semantics.
    One might naturally worry that it will be hard to pin down the semantics of this transitional regime.
    We develop a grammar of atomic transitions (§4) that carries us from `prev` through `link` to `next`, and any `link` produced by that grammar is itself an ordinary scheduling control in the sense of §3.1: so it _does_ have a human-readable semantics, just as `prev` and `next` do.
  - Obligation 2.
    Improve upon the state of the art.
    We will give the semantics of the stop-the-world `link` and use it as a baseline.
    Several practical goals guide us as we look for better ways to transition from a given `prev` to a given `next`.
    Can we minimize the length of the transition period?
    Can we avoid dropped/delayed packets?
    [AM: more to come here; the cost model is a legit open question!].
    We will show that it is always possible to make a transition from any `prev` to any `next`, but it is occasionally possible to make a very efficient transition.
    We contribute a tool that achieves this and shows [improvements].

## 2. Background & Motivation

### 2.1 Hierarchical scheduling on hardware

- Packet scheduling, PIFOs, and hierarchical scheduling with PIFO trees.
- Mapping a hierarchy onto hardware: each tree depth lands on a PE; siblings and cousins share a PE.
  This has been known since Sivaraman (SIGCOMM '16).

### 2.2 vPIFO, and the problem it leaves open

- vPIFO (Zhang et al., SIGCOMM 2024) is the closest related work, and it explicitly leaves our problem open.
- What vPIFO does.
  It virtualizes a single physical PIFO into many logical "PIFO instances," with a Scheduling Description Language (SDL) and compiler, so their PIFO Visor can _flexibly establish_ hierarchical PIFO trees of arbitrary shape on fixed hardware.
  Its contribution is the reconfigurable substrate.
- What vPIFO does not do.
  As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change.
  The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- Our two running examples make the gap concrete.
  The easy append, to `Strict(gmail, zoom, spotify)`, looks like the kind of edit vPIFO's substrate could absorb in place: a targeted append to the structures that encode the tree shape and per-instance storage (the Operation Generation Table and the PIFO Instance Address Table) would plausibly register the new traffic and its operations while leaving the running instances untouched.
  vPIFO as published does not address this; its only option is a full reinitialization.
  Issuing the small diff to such a substrate, so that it installs the edit in place, is exactly what our layer adds.
  The harder restructuring, to `Strict(gmail, RoundRobin(zoom, spotify))`, lies beyond what the vPIFO substrate can absorb in place: it changes the running tree's shape (inserting a new internal node and re-parenting `zoom` under it), which is not exposed as an in-place edit on the Operation Generation Table or the PIFO Instance Address Table.
  The closest vPIFO comes is a full reinitialization onto the new shape, which is precisely the stop-the-world baseline we want to improve on.
- vPIFO's own §9 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work."
  The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly.
  We are not competing with vPIFO and do not claim a better PIFO substrate.
  We supply the layer _above_ a PIFO substrate (which could well be vPIFO).
  That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics.
  The two layers compose.

## 3. A Grammar of Atomic Policy Diffs

§3.1 recaps the PIFO tree model.
§3.2 defines a small policy DSL and a compiler from it into a runnable _control_, giving us the syntactic handle on controls that the rest of the section needs.
§3.3 fixes a grammar of structural edits (_diffs_) over that DSL, where every diff is atomic by construction.
§3.4 restates atomicity once the operational rewrite `[[δ]]` is in hand and discharges three obligations per production: realization of the pol-level denotation, preservation of well-formedness, and preservation of shared state.
§3.5 argues that all of this survives the lowering to hardware.
Composing diffs into the sequences that realize a full reconfiguration, and the `link` schedulers that arise between them, is deferred to §4.

### 3.1 Background: PIFO trees

The PIFO tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3] is what we build on.
We recap topology, the two observable operations, and well-formedness.

##### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies.
A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`.
A leaf `Leaf(p)` holds a packet-carrying PIFO `p`.
An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`.
This separation between the topology and the carried contents is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

##### The two observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`.
The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue.
At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself.
`pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf.
These are the _only_ observable interactions with a scheduler, which is why our notion of an _atomic_ transition is stated in terms of `push`/`pop` observability.

##### Well-formedness

A PIFO tree `q` is well-formed (written `|- q`) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`.
This is the invariant that keeps `pop` from getting stuck.
`push` always preserves `|- q`, and `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

### 3.2 A Policy DSL

A PIFO tree is a runtime representation, not a programming surface.
To talk about reconfigurations we step up a level: a small policy DSL the operator writes in, and a compiler from DSL terms to runnable PIFO tree _controls_ (defined below).
The transition planner of §4 needs both a way to compile a starting control `C` from an operator's request and a way to compare two such requests; the DSL gives us both.

This is essentially what the vPIFO paper's _Scheduling Description Language_ does informally [cite vPIFO, §4].
They do not pin down a grammar for SDL or formalize the compilation, so our DSL can be read as a formal core of their concrete language.
The compilation targets differ (they target a virtualized PIFO substrate; we target a control), but the strategy is the same: give the operator a syntactic surface, then compile.

##### Policy syntax: `pol`

```
pol    ::= flow                   // leaf, labeled by a flow of traffic
         | D(pol_1, ..., pol_n)   // internal node, discipline D
```

This grammar allows policy trees of arbitrary arity.
`D` ranges over the disciplines (`Strict`, `RoundRobin`, `WFQ`, etc.).
A discipline may attach per-arm metadata that shapes how the arm is scheduled.
`WFQ` takes a positive real weight per arm; `Strict` takes a priority rank per arm; `RoundRobin` takes nothing.
The grammar carries no structural mark of any of this: the operator writes the metadata in the surface syntax (e.g., `WFQ(w_1: pol_1, ..., w_n: pol_n)`, or positional sugar like `Strict(A, B)` which desugars to `Strict(hi: A, lo: B)`), but the metadata lives in the arm's `slot_state` once compiled (see `init_slot_D` below) and `push`/`pop` can only read it.
Arm order in the surface notation is a presentation choice, not a scheduling-meaningful one: `Strict(hi: A, lo: B)` and `Strict(lo: B, hi: A)` describe the same scheduler. §3 stays positional throughout (paths address slots); the §4 sniffer can benefit from this equivalence when comparing `prev` against `next`.
We read the arity off by counting children, so `Strict(gmail, zoom)` is the 2-ary instance.
Each leaf label denotes a flow: a predicate over packets. A `pol` is _valid_ when (a) every discipline is applied at a proper arity (with the per-arm metadata that the discipline requires) and (b) the flows at the leaves are pairwise disjoint, in the sense that every incoming packet is either dropped or is routed to one leaf.
Validity is a condition on the source `pol`, not to be confused with the runtime invariant `|- q`.

##### Discipline compilation: `init_node_D` and `init_slot_D`

Each discipline `D` comes with a mechanical recipe for compiling a node that runs it: (a) a per-node ranking program, (b) an initial `node_state` for the node, and (c) a `slot_state` for each arm attached under the node.
We name the two state-seeding projections; the ranking program is handled by the compilation walk below and needs no separate name.

```
init_node_D : () -> node_state
init_slot_D : node_state × meta? -> slot_state
```

The `meta?` argument is the per-arm metadata that `D` requires (a weight for `WFQ`, a priority rank for `Strict`, absent for `RoundRobin`).
`init_node_D` is called only at _compile time_, once per node, to seed that node's `node_state`.
`init_slot_D` is called in two places.
At _compile time_, walking the source pol, it is called once per child arm to seed that child's `slot_state`, taking the parent's just-seeded `node_state` and the arm's `meta?` as input.
At `Add` (§3.4.1), when a new arm is spliced under an already-running `D`-parent, `init_slot_D` is called once with the parent's _current_ `node_state` and the new arm's `meta?` to produce the arm's `slot_state`.
The function is the same in both cases; only the source of the parent's `node_state` differs.

Choosing `init_slot_D` is a scheduling decision, not just a structural one, since the choice changes how a freshly spliced arm competes with the established arms.
We make the choice to "join the current round".
For example, if we go from `WFQ(A,B)` to `WFQ(A,B,C)`, we do not want the newly added `C` to reap a huge benefit for having been silent all this while; we just want it to join the others with neither a penalty nor an advantage.

To this end: `init_slot_RR` returns the empty tuple (no per-arm bookkeeping to seed); `init_slot_Strict(_, p) = p` (the arm's `slot_state` is just its priority rank, drawn from a dense total order such as the rationals so that a fresh priority can always be slotted strictly between two existing ones).
For `WFQ`, the `node_state` at a parent is the virtual time `vt`, and we set `init_slot_WFQ(vt, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag.
By WFQ's standard finish-time recurrence the first packet on this arm gets a tag of `max(virtual_clock, vt) + 1/w`, which slots it into the round that the established arms are currently in.
At compile time the parent's `vt` is freshly initialized (to zero), so all original arms get `(w, 0)` and the round is "the zeroth"; at `Add` the parent's `vt` is whatever the clock has advanced to.

##### Policy Compilation: `pol` to control

A PIFO tree _control_ `C` is a tree of node-local 4-tuples, one per node of the topology.
At an internal node the 4-tuple is `(D, state, pifo, z)`, where `D` names the discipline running at that node.
At a leaf it is `(flow, state, pifo, z)`, where `flow` is the leaf's flow label.
The tree shape exactly matches that of `pol`, as each node of `C` lines up with a node of the source `pol`.
Each diff of §3.4 acts on a small local neighborhood of these 4-tuples; the whole control is just the assembled tree.

At each node of `pol`'s topology, compilation seeds the four pieces:

- The source-level tag (`D` at an internal node, `flow` at a leaf) is copied verbatim from the source `pol`. It is what `⌊·⌋` (below) reads to recover `pol` from `C`.
- `state` is a pair `(node_state, slot_state list)`.
  The `node_state` carries `D`'s per-node bookkeeping (an `RR` cursor, a `WFQ` global virtual time), seeded by `init_node_D()`.
  The `slot_state` list carries per-arm bookkeeping, one entry per child arm in slot order (a `WFQ` arm's weight and virtual finish; a `Strict` arm's priority rank), each entry seeded by `init_slot_D`.
  Disciplines without per-arm bookkeeping (`RR`) have an empty `slot_state` list.
- `pifo` is an empty PIFO: an index-PIFO at an internal node, a packet-PIFO at a leaf.
- `z` is `D`'s ranking program at the node. It maps the local `state` and an incoming packet to one path segment plus an updated `state`. The shape of that segment differs between internal nodes and leaves:
  - at an internal node, `z : state × Pkt ⇀ (idx × rank) × state`: pick a child index `i` and the rank `r` with which to enqueue `i` at this node's index-PIFO;
  - at a leaf, `z : state × Pkt ⇀ rank × state`: pick the rank `r` for the packet's own PIFO entry.

  When `z` is undefined for a packet, the per-node action is empty: nothing is enqueued at this node and `state` is unchanged. The global consequence (the walk halts, the packet is dropped from the system entirely) is a property of how per-node `z`s are composed, made precise in the FA-compatibility paragraph below.

We address nodes by `path` (§3.3): the local 4-tuple at the node reached by following `path` from `C`'s root is written `C@path`, with fields `C@path.D` (at an internal node) or `C@path.flow` (at a leaf), and `C@path.state`, `C@path.pifo`, `C@path.z`.
We also write `C@path.node_state` and `C@path.slot_states` for the two components of `C@path.state` (with the plural `slot_states` reflecting that it is a list, one entry per child arm).

##### Well-formedness: `|- C`

In §3.1 we defined well-formedness on a PIFO tree, written `|- q`. Now we redefine it, lifting it to act on a control `C`. A control `C` is _well-formed_ (written `|- C`) when, at every internal node of `C`, the `pifo` has, for each legal child index `i`, exactly as many occurrences of `i` as there are packets stored in the leaf pifos of the subtree under the `i`-th child. This is stated directly on `C`: no global PIFO tree need be assembled to check it.

##### Compatibility with Formal Abstractions

FA's controls are a single triple `(s, q, z)` with a state map `s`, a PIFO tree `q`, and a single transaction `z : St × Pkt -> Path(t) × St` (total).
One can flatten our `C` into such a triple by gluing the pieces together. The FA-style tree `q` is the tree of our `pifo` pieces. The FA-style state `s` collects the `state` pieces indexed by path. The FA-style path-emitting scheduling transaction `z` walks the topology applying each `z` piece in turn and appending the emitted path segments into paths. The per-node source tags (`D` and `flow`) are extra metadata we maintain alongside `C` to support `⌊·⌋` below; they have no counterpart in the FA triple and do not participate in the gluing.
The partiality that our per-node `z`s allow shows up as partiality on the FA-style global `z` (a drop anywhere along the descent leaves the global function undefined for that packet).
The rest of the paper has no need for gluing a control together in this way (`|- C` is stated directly per the previous paragraph, and the diff rules of §3.4 act node-locally), but a reader more at home in FA's framing can recover it in this way.

##### The bridge: `⌊·⌋`

Each node of `C` carries enough source-level metadata to recover the corresponding `pol` node directly: the topology comes from the tree shape, the discipline at each internal node from `C@path.D`, the per-arm metadata (weights for `WFQ`, priorities for `Strict`) from `C@path.slot_states`, and the flow label at each leaf from `C@path.flow`.
We write `⌊C⌋` for the `pol` recovered from control `C` and say that `C` _realizes_ `⌊C⌋`.
The correctness condition for the compiler above is then `⌊compile(pol)⌋ = pol`.

The map `⌊·⌋ : control -> pol` is many-to-one: `pol` records only the structural skeleton, while a control also carries the live `state` and `pifo` at each node.
In particular, every control that arises by running `compile(pol)` and then serving traffic for a while still realizes `pol`.
We need this split so that, when we later edit a running control in place, we can formally ask _what `pol` that modified control realizes_.

### 3.3 A Grammar for Tree Diffs

The user writes down `pol` objects, like `prev`.
Say that we compile the user's request `prev` into some control `C`, s.t. `C` realizes `prev` (written `prev = ⌊C⌋`).
Then the user requests a change to a new `pol` object called `next`.
SOTA would compile `next` into some control `C'` and clobber the running `C` with `C'`.
We wish to achieve the same user-observable result while being less disruptive to unaffected parts of the running control.

A change to the live control is _atomic_ when its effect falls between two observable operations: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if the change fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by the pre-change control and `op_{N+1}, ...` are served entirely by the post-change control.
No operation straddles the change, and no operation sees an intermediate state.
§1's running example described this informally.

We fix a small grammar of atomic edits.
Each production denotes a single primitive that acts on a live control `C` and produces a new control.
The grammar in this section is the alphabet from which the transition planner (§4) assembles a sequence whose operational composition takes `C` to a control realizing `next` _without clobbering_.
Most productions are `pol`-visible: their effect shows up in `next`, and a comparison of `prev` against `next` is enough to understand them.
Others are transaction-only: their effect lives entirely in `z`, leaving the `pol`-level skeleton untouched.

An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is.
We write `t@path` for the subtree of `t` reached by following `path` down from `t`'s root.
Depending on the production being used, `t` is instantiated to `prev` or `next`; see below.

```
δ ::= Add          (path, pol)
    | Quiesce      (path)
    | Designate    (path, pol)
    | Undesignate  (path)
    | Remove       (path)
    | ChangeWeight (path, weight)
    | Graft        (ctx)
    | ChangeRoot   (path)

path   ::= []  |  i :: path             // i is a child index
ctx    ::= □                            // the unique hole; takes no children
         | D(pol, ..., ctx, ..., pol)   // n children total; exactly one is itself a context
weight ::= a positive real
```

`pol` is the nonterminal of §3.2.
A _policy context_, written `ctx`, is built like a `pol`, except that exactly one of its slots is the distinguished _hole_ `□` rather than a subtree.
The hole is a reserved slot, not an absence of a slot: the parent of the hole has an arity that includes the hole, e.g., `RoundRobin(A, B, □)` is a 3-ary `RoundRobin` whose third slot is the hole, distinct from the 2-ary `RoundRobin(A, B)`.
We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`.
A `ctx` is _valid_ iff `ctx[s]` is a valid `pol` for some valid `s`.
The plug is total, and for any valid `ctx` and any valid `s`, `ctx[s]` is a valid `pol` whenever the leaf labels of `ctx` and `s` are disjoint.

The grammar is shaped by what we can realize atomically in hardware (§6): each production is exactly an edit for which we have a substrate-level commit.
That commit slips in between two consecutive `push`/`pop` operations.

Edits that would have to destroy structure still holding packets are expressly _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§5) only after ensuring that the subtree being removed is empty.
The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _idioms_: sequences of these atomic edits. §4 makes these precise.

Notes on the individual edits.

- `Add(path, pol)` splices `pol` in as the new slot `next@path`.
  When the target parent's discipline requires per-arm metadata, the operator supplies the new arm's `meta?` alongside the request: a weight for a `WFQ` parent, a priority rank for a `Strict` parent.
  `init_slot_D` reads `meta?` when seeding the new arm's `slot_state` (§3.4.1).
  Add is _non-disturbing_: it writes only the new arm's `slot_state`; every existing arm's `slot_state` is preserved verbatim.
  The dense-order convention on `Strict` priorities (§3.2) is what makes this hold uniformly: whether the new arm goes at the end or strictly between two existing arms in priority order, the operator can always pick a fresh `meta?` that needs no renumbering of existing arms.
  The `WFQ` case is non-disturbing in the same sense (a new arm carries its own weight, leaving the others' weights untouched), and `RR` is trivially so (no per-arm `slot_state` to disturb).
- `Quiesce(path)` prevents `prev@path` from receiving new traffic. The patch lives at the root: the root's `z` becomes undefined on any packet bound for a leaf under `prev@path`. Since every descent starts at the root, this single patch halts those packets before any new enqueue can go through, so no ancestor's `pifo` ever holds a new index pointing toward the quiesced subtree.
  Topology, disciplines, and labels are unchanged; the edit's whole effect is in the root's `z`, so it is `pol`-invisible.
- `Designate(path, pol)` converts `prev@path` into `Strict*(prev@path, pol)` in place.
  That is, we introduce a new node with discipline `Strict*`, with the existing subtree `prev@path` as its high-priority sibling and `pol` as its low-priority sibling.
  `pol` is the _designated survivor_ of `prev@path`.
  We make no assumption about how much overlap there is between `prev@path`'s flows and `pol`'s flows.
  In case there is overlap, we simply use timing information (the moment of the request) to distinguish old `prev@path` traffic from new `pol` traffic.
  This keeps the mid-sequence `pol`'s leaf labels disjoint by construction.
  Literally inserting this `Strict*` node in the middle of the running tree would be expensive, as it would require relocating the entire subtree `prev@path` one PE deeper (because siblings and cousins must share a PE, see §2.1).
  §6 features a new in-place hardware gadget that gives us the `Strict*` semantics described here without incurring that relocation cost.
- `Undesignate(path)` collapses `prev@path`, which must be a `Strict*(A, B)` node where `A` is empty, into `B`.
  The edit is in place, in the sense that `B` inherits `Strict*(A,B)`'s slot and per-arm `meta?` under the parent, and the parent now routes through that slot directly to `B`.
- `Remove(path)` structurally removes `prev@path`, dropping its slot and renumbering any higher siblings.
  The subtree `prev@path` must be empty; §3.4.2 discusses why.
  `Remove`'s precondition rules out `Strict*` targets.
- `ChangeWeight(path, weight)` overwrites the weight that `prev@path`'s parent uses for it.
  Concretely it writes the `weight` field of `prev@path`'s `slot_state` (one of the per-arm fields seeded by `init_slot_WFQ`); `push`/`pop` only read this field, so the only way it changes is via this diff.
  It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft(ctx)` produces `ctx[prev]`: the policy context `ctx` is spawned around `prev`, with `prev` plugged into the context's sole hole.
  `Graft` carries no `path`: if the user wants localized graft-style edits, deeper in the tree, they must be realized as an idiom (§4), not by a path-bearing `Graft`.
- `ChangeRoot(path)` promotes `prev@path` to the new root, discarding every ancestor above it.
  It is well-defined only when `path` is non-empty and each internal node strictly above `prev@path` has a single arm (the one continuing toward `prev@path`), so the discarded ancestor chain carries no traffic of its own.
  The richer reconfiguration of pruning a tree down to a subtree that originally shared ancestors with packet-bearing siblings is realized as an idiom (§4) that first drains and `Remove`s those siblings, reducing the chain above `prev@path` to the unary shape `ChangeRoot` requires.

A `Strict*` node is one introduced by `Designate`, a plain `Strict` one written by the user.
Semantically the two are identical; every push, pop, and well-formedness check treats `Strict*(A, B)` exactly as `Strict(A, B)`.
The star exists only so that `Undesignate`'s precondition ("path lands on a `Strict*`") and the hardware story in §6 ("`Strict*` adds no PE depth") can be stated structurally.
The §3.2 DSL that the operator writes can parse only `Strict`, never `Strict*`, so a `Strict*` is unreachable in any user-written `pol` and arises only in the middle of a planner sequence, between a `Designate` and its eventual `Undesignate`.

When `prev = next` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.4 All Productions of `δ` are Sound

Each `δ` (§3.3) admits two semantic readings, both _partial_.
The operational reading is primary; the pol-level reading is a projection of it.

- The _operational transition_, `[[δ]] : control ⇀ control`, is the live rewrite acting on the control `C`.
  The per-production rules below state where `[[δ]]` is defined; outside that, we say `δ` is _incompatible_ with the input control and `[[δ]](C)` is undefined.
  §4's transition planner only emits a `δ` whose `[[δ]]` is defined on the live `C`.
  The preconditions vary by production, e.g.:
  - For `Add` we require the path's parent prefix to land at an internal node, the path's final index to be a legal insertion slot (at most the parent's current arity), the operator-supplied `meta?` to match what the parent discipline requires (a weight for `WFQ`, a priority rank for `Strict`, absent for `RR`), the new leaf labels to be fresh, and the new classifier predicates to be disjoint from the domain of `C`'s live `z`.
  - For `Remove` we require the target subtree to be empty.
  - For `Undesignate` we require the target node to be a `Strict*(A, B)` with `A`'s subtree empty.
  - For `ChangeWeight` we require the parent at `path`'s prefix to run WFQ.
- The _pol-level denotation_, `den(δ) : pol ⇀ pol`, is the projection of `[[δ]]` through the `⌊·⌋` bridge of §3.2.
  It is partial for the same syntactic reasons (paths must resolve, leaf labels must be fresh, weights must match), but, because `pol` carries no live contents or transaction, it never fails for operational reasons like `Remove`'s emptiness or `Add`'s classifier disjointness.
  It captures the `pol`-visible part of the edit.
  For most primitives (`Add`, `Remove`, `ChangeWeight`, `Graft`, `Designate`'s wrap-and-rename, and `Undesignate`'s collapse onto the survivor), `den(δ)` is non-trivial and matches what a reader would extract from comparing `prev` against `next`.
  `Quiesce`, whose whole effect is in `z`, has `den(Quiesce)` equal to the identity on `pol`.
  The star on a `Strict*` node is invisible to `⌊·⌋` (by design, since the star is operational, not denotational), so at the pol level `den(Designate(path, B))` wraps `pol@path = A` into `Strict(A, B)`, and `den(Undesignate(path))` consumes a `Strict(A, B)` at `path` and yields `B`. Composed in that order they act as a replacement at `path`.
  So `den(δ)` is a purely static map, and effects that live entirely in `z` are invisible to it.
  §4's planner works at this level: it searches for a sequence of edits such that composing their `den`otations together carries `prev` to `next`.

The two readings are linked through `⌊·⌋`: whenever `[[δ]](C)` is defined, so is `den(δ)(⌊C⌋)`, and in fact `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)`.
To put it in notation: `den(δ) : pol -> pol` is defined to be the relation `{ (⌊C⌋, ⌊C'⌋) | [[δ]](C) = C' }`.

In the language of `[[δ]]`, _atomicity_ can be restated crisply: when `δ` is compatible with `C`, `[[δ]]` replaces the live control `C` with `[[δ]](C)` between two consecutive `push`/`pop` operations.
Modeling `[[δ]]` as a single (partial) function `control ⇀ control` bakes indivisibility into the abstraction; the obligations below ensure that the result of this single step is correct.

For each production we discharge three obligations, all assuming `[[δ]](C)` is defined.
Throughout, we write `C` for the pre-edit control and `C'` for the post-edit control.

- _Realization_ constrains the `pol`-visible skeleton: `⌊C'⌋ = den(δ)(⌊C⌋)`.
  For our running example, `δ = Add([2], spotify)`, so `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and realization says `⌊C'⌋ = Strict(gmail, zoom, spotify)` for any `C` with `⌊C⌋ = Strict(gmail, zoom)`.
  The equation pins down the new topology, disciplines, weights, and labels.
  Transaction-only effects (e.g., `Quiesce`'s shrinking of `z`'s domain) lie outside `⌊·⌋` and are fixed by the per-production operational rule below, not by realization.
  Realization says nothing about contents.
- _Soundness_ constrains the live contents: `|- C` must imply `|- C'`.
  Well-formedness (§3.2) is about per-node pifos and the packets stored under them, so this obligation says that any packets and index entries `[[δ]]` drops or adds must leave the counts in balance.
- _State preservation_ constrains each local control's state.
  At every node structurally shared between `prev` and `next` and outside the production's local edit site, the local `state` is preserved verbatim.
  At the edit site, and at every node inside a freshly-spawned subtree, the state is exactly what the production's `init`-rule prescribes, with the specific invocation of `init_node_D` / `init_slot_D` (§3.2) given per-production below.

The denotational rules frequently read, overwrite, and splice child lists.
Let us fix some notation.
We write `ts[i]` for the `i`-th child and `ts[t/i]` for `ts` with its `i`-th child overwritten by `t`; this leaves the arity unchanged.
We mark arity-changing edits with `+` and `-`: `ts[+t/i]` splices `t` in as the new `i`-th child (the old `i`-th and later children shift one place to the right), and `ts[-/i]` drops the `i`-th child (later children shift left).
Indices follow the `path` convention of §3.3.

The operational diffs of §3.4 manipulate two parallel structures at an internal node: a child list `qs` of PIFO tree children, to which the list-manipulation notation introduced above applies verbatim, and an index-PIFO `p` recording child indices.
For `p` the same `[±/k]` notation _renumbers_ rather than splices, since the PIFO holds index _values_ that have to track a shift in the child list.
`p[+/k]` is `p` with every entry `>= k` bumped up by one (opening up slot `k`); `p[-/k]` is `p` with every entry `> k` brought down by one.
Entries below the edit point are left alone.

#### 3.4.1. `Add(path, pol)`

##### Pol-level denotation

`den(Add(path, pol))` is the structural map, defined by recursion on `path`:

```
den(Add(i :: [],   pol)) (D ts) = D ( ts[+pol / i] )
den(Add(i :: rest, pol)) (D ts) = D ( ts[ den(Add(rest, pol)) (ts[i]) / i ] )
```

The base case applies once `path` reaches the new slot's parent: the subtree `pol` is spliced in as the new `i`-th child.
(Recall from §3.3 that `Add`'s `path` is read in `next` and names the new slot, so its final index `i` is the insertion point.)
The recursive case walks down a shared ancestor, recurses into child `i`, and writes the result back in place.
When the parent runs `WFQ`, the operator's request also carries the new arm's weight.
This is what `init_slot_WFQ` reads at the operational level (below), but it does not appear in `den(Add)` itself.

Our running example denotes `den(Add([2], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 2] ) = Strict(gmail, zoom, spotify) = next`, as intended.
In our example we appended a new arm, but a mid-tree edit would be no more complicated: `den(Add([1], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 1] ) = Strict(gmail, spotify, zoom)`, with the old `zoom` sliding from index `1` to index `2`.

##### Operational transition

Let `π` be the path to the new slot's parent (here `π = []`, the root `Strict`) and `k` the new slot's index, so `path = π ++ [k]`.
Let `D` be the discipline at `π`.
The transition `C' = [[Add(path, pol)]](C)` is stated per node.

The topology gains a new arm at `π`, indexed `k`; the old arms at `π` with indices `>= k` shift right by one.
The local controls update as follows.

- _At every node outside the new subtree, other than `π` and its proper ancestors:_ the local control is preserved verbatim.
- _At each proper ancestor of `π` (including the root):_ `node_state`, `slot_states`, and `pifo` are preserved verbatim.
  The local `z` is extended to admit packets that classify into the new subtree, mapping them to whichever child slot at this ancestor lies on the path down to `π`.
- _At `π`:_
  - `C'@π.node_state = C@π.node_state` (unchanged).
  - `C'@π.slot_states = C@π.slot_states[ + init_slot_D(C@π.node_state, meta?) / k ]` (a plain append when `k` is last; `meta?` is the operator-supplied per-arm metadata `D` requires, per §3.2).
    The fresh entry is the per-arm bookkeeping `init_slot_D` of §3.2 prescribes for an arm newly spliced under a running `D`-parent.
  - `C'@π.pifo = C@π.pifo[+/k]`: every entry `>= k` is bumped up by one to track the shift.
    Each pre-existing slot keeps its matched count of entries and packets, now under its new name.
    When `k` is the last index (the append case) nothing is `>= k` and the pifo is untouched.
  - `C'@π.z` extends `C@π.z` in two ways.
    First, it admits packets that classify into the new subtree, mapping them to child index `k` at `π` (with descent handed off to the new subtree's `z` pieces below).
    Second, mirroring the pifo renumbering at the path level, it renumbers any old mapping whose chosen index at `π` was `>= k` to that index plus one.
    No `C` packet was ever routed to `k`: under `C'`, `k` names the new subtree, and the old occupant of `k`, if any, now lives at `k+1`.
- _At every node inside the new subtree:_ the local control is what §3.2's compiler produces for that node.

##### Soundness

We discharge the three obligations of §3.4 in turn (recall `C' = [[Add(path, pol)]](C)`).

- _Realization._
  The updates above leave every pre-existing arm structurally intact (modulo renumbering of `π`'s pifo and the `z` extensions along the ancestor chain, neither of which `⌊·⌋` reads) and add a new arm at `π`'s slot `k` whose subtree is the freshly-compiled `pol`.
  So `⌊C'⌋ = den(Add(path, pol))(⌊C⌋)`: the new shape is exactly what the edit denotes.
- _Soundness._
  `|- C` gives `|- C'`.
  At `π`, `C'@π.pifo = C@π.pifo[+/k]` contains no entry equal to `k`: old entries `< k` stay put, old entries `>= k` were bumped to `>= k+1`.
  The new subtree under slot `k` holds zero packets, so the well-formedness count at slot `k` reads `0 = 0`.
  Every other slot at `π` is the child it was, with the same packets beneath it; its entries in `C'@π.pifo` are the old entries under a possibly-shifted name, so its matched count is inherited verbatim.
  Every other node's pifo is untouched (the ancestor `z` extensions touch no pifo at this instant; they only affect the classification of packets that arrive later).
  Nothing needs repair.
- _State preservation._
  Outside the edit site the local control (and thus its `state`) is preserved verbatim, including at each proper ancestor of `π`, where only `z` changes.
  Inside the new subtree, the state is freshly compiled per §3.2.
  At `π`, `node_state` is unchanged and `slot_states` splices in exactly `init_slot_D(C@π.node_state, meta?)`, as §3.4 prescribes at the edit site.

##### Notes

_Atomicity._
No in-flight packet straddles the diff.
At the diff instant, every packet sits in some pre-existing node's `pifo`, and each such packet survives into the same `pifo` in `C'` unchanged.
The new slot `k` holds nothing.
The renumbering `C@π.pifo[+/k]` relabels index _values_ only; it moves no packets and changes no ranks.
So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
The new slot contributes nothing; the existing subtrees keep their contents and relative priority, just under shifted names.
The first `push` that `C'@π.z` routes to `k` is the first packet ever to occupy the new subtree.

_Deeper paths._
The running example edits the root, but `path` may be any prefix; `Add([1, 2], pol)` adds a slot inside a grandchild of the root.
Nothing in the argument changes.
The descent from the root to `π` passes through ancestors whose only change is the `z` extension above; their `node_state`, `slot_states`, and `pifo` are untouched.
Because the new subtree is empty, no packet is yet routed through any ancestor's `z` extension, so each ancestor's count for the child it forwards through is exactly what it was.
No ancestor pifo is rewritten, and the edit is otherwise confined to `π` and the fresh subtree below it.

#### 3.4.2. `Remove(path)`

`Remove` is the grammar's one structural deletion.
It unhooks the subtree `prev@path` from its parent, drops the vacated slot, and renumbers any higher siblings.
It is defined only when `prev@path` is _empty_.
Retiring or replacing a subtree that still holds packets is therefore not `Remove`'s job alone: it is realized as a _sequence_ that first drains the subtree and only then removes it (see §4).

##### Pol-level denotation

##### Operational transition

##### Soundness

##### Notes

_Why an empty subtree?_
Deleting an _occupied_ child has no canonical local realization.
Consider `P(Q(A, B), C)`.
In the diagram below each internal node is labeled with its index-PIFO and each leaf with the packets it holds.
We draw every PIFO with its most favorably ranked entry, the next one to be popped, on the far right: so the leaf `[a2,a1]` releases `a1` before `a2`.

```
                    P
            [1,2,1,2,1,2,1,2]
               /             \
              Q               C
          [1,1,2,2]    [c4,c3,c2,c1]
            /     \
           A       B
        [a2,a1]   [b2,b1]
```

Here `Q` refers to `A` using the index `1`, and to `B` using `2`.
`P` refers to `Q` using `1` and to `C` using `2`.
The tree is well-formed.
`P` appears to be running a round-robin style policy and `Q` appears to be prioritizing `B` strictly over `A`.
Say the user asks us to delete `B`, dropping its packets.
To restore well-formedness, we need to remove two instances of the index `2` from `Q`'s PIFO and two instances of the index `1` from `P`'s PIFO.

At `Q` the edit is unambiguous.
Well-formedness of the original tree forces `Q`'s PIFO to have exactly two instances of `2`, and we delete those two.
The trouble is one level up.
`P`'s PIFO originally had four instances of `1`, and well-formedness demands that we remove two of them.
But _which_ two?
No entry in `P` carries the meta-information "I was enqueued when a packet was inserted into `B`"; an entry `1` in `P` only means "when this index is popped, recursively ask subtree `Q` to emit the next packet" (see §2.1).
This absence of meta-information is a feature of PIFO trees, not a defect: an entry is deliberately detached from the packet it was enqueued with, which is what lets each node schedule its own discipline in isolation and lets a subtree be reconfigured without rewriting its ancestors.
We could make deletion unambiguous by tagging each entry at `push` with the leaf it is destined for, but that would discard the abstraction: every internal node would then have to track the entire downstream structure, and the composability that makes PIFO trees scale would be lost.

Given this, _we can only pick two instances of `1` arbitrarily_.
Our choice is, in effect, a scheduling decision, as it affects how `P` intermixes `C` and `Q` traffic.
For example:

a. _Keep the two leftmost `1`s_ (strike the two on the right):

```
                          P
           [1,2,1,2,~1~,2,~1~,2]
                  /             \
                 Q               C
               [1,1]      [c4,c3,c2,c1]
                 |
                 A
              [a2,a1]
```

Here `~1~` indicates an instance of `1` that we dropped.
Six pops drain `P` and yield `c1, c2, c3, a1, c4, a2`: `A` is pushed to the back, both its packets emerging only after `C`'s run is mostly spent.

b. _Keep the two rightmost `1`s instead_ (strike the two on the left):

```
                          P
           [~1~,2,~1~,2,1,2,1,2]
                  /             \
                 Q               C
               [1,1]      [c4,c3,c2,c1]
                 |
                 A
              [a2,a1]
```

The same six pops now yield `c1, a1, c2, a2, c3, c4`: this is exactly the round-robin schedule `P` was configured for, alternating `C` and `A` until `A` is exhausted and `C` drains alone.

Same tree, same surviving packets, two different service orders.
The _choice of which_ `1`s to drop _silently reschedules `A` against `C`_, and nothing in `prev` or the deletion request records which the operator wanted.

We do not wish to make a scheduling decision for the operator, so our only other option is to reconstruct the tree as if `B` had never been admitted: drain the tree completely, filter the `B`-packets, and re-push, recomputing every rank and cursor along the way.
This is a stop-the-world rebuild.

Our grammar sidesteps the question entirely.
By draining `B` to empty before removing it (§4), every pop that served a packet from `B` has already removed the matching index-entries from `B`'s ancestors in the ordinary course of `pop`: a packet leaving `B` consumes a `2` at `Q` and a `1` at `P`, bringing each ancestor's count into accord with its post-drain contents.
At the instant `Remove` fires, `Q`'s `2`-count is zero (`B` is empty) and `P`'s `1`-count is exactly the number of packets still under `Q` (here `2`, all attributable to `A`).
There is nothing left to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

### 3.5 Preserving this proof down to hardware

§3.4 proves soundness at the tree-diff level, where each atomic edit (`Add`, `Quiesce`, `Remove`, ...) carries a well-formed tree to a well-formed tree.
To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR: `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, and the like.
A single IR instruction, unlike a whole tree-diff edit, _can_ leave the tree malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, for example.

We do not prove soundness at the IR level, but instead informally make the case for why the §3.4 proof survives the lowering.
There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit.
  We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed.
  That commit is precisely how the substrate _realizes_ the atomicity property of §3: atomicity asked for an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant.
  Every `push`/`pop` therefore still lands on a well-formed control (`prev`, a `link`, or `next`), exactly as §3.4 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.4 proved.
The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §6.

[AM note for Zhiyuan: the §3.5 argument leans on the substrate supporting _atomic transactional commits_: a multi-instruction lowering must install as a single instant from the user's perspective, so that the transiently-malformed intermediate trees inside a commit are never observed.
Our own substrate provides this.
The open question for §6 is whether composition with a third-party substrate (e.g., vPIFO) requires the same property and, if so, whether vPIFO offers it.
Flagged here so the §3.5 claim "the proof survives the lowering" is not read as substrate-independent.]

## 4. Realizing Reconfigurations as Sequences

§3 proved each grammar production a sound atomic diff.
This section composes diffs into _sequences_ `(φ ; δ)*` of `(guard, diff)` pairs, where a guard `φ` is a predicate on the state of the live control `C` (any `φ` may be `true`, meaning the paired diff fires at once; `φ_0 = true` is the common case).
Sequences are the universal substrate of reconfiguration: they realize the changes no single diff can express.

Two authoring modes produce sequences.
In _declarative mode_, the operator writes a `pol` and, to reconfigure, writes a second `pol`; a differ produces the sequence.
The differ is intentionally simple: it sees only pol-level diffs whose translation to a sequence is straightforward, and falls back to the generic `Designate([], next) ; Undesignate([])` pair (§5) for anything richer.
Multi-step reconfigurations with operator choice (e.g., `Retire` vs. `SlowRetire` below), graft-style local edits the differ cannot infer, and other confined strategies are outside its scope.
In _imperative mode_, the operator writes the `(φ; δ)*` sequence directly, possibly using a small vocabulary of _idioms_ (§4.1).
Most operators stay in declarative mode; imperative mode is for the cases where the differ's choices are wrong or insufficient.

The two modes are not formally distinct: the sequences they produce live in the same substrate and discharge the same soundness obligations from §3.4.
Imperative mode buys expressivity, not a different proof obligation.
It admits sequences the differ might not never emit, but fundamentally still emits `(φ; δ)*` sequences.

The headline result of the section is that the transitionary scheduler `link` between two consecutive diffs is itself an ordinary §3.1 control, so the "transitionary period" needs no new semantics: this is Obligation 1 of §1, discharged.

[AM TK: liveness — whether and when a sequence's guards become true — is a nice-to-have to press upon the user. The simplest behavior: if the operator requests a new change before the previous one has fully landed, the new change is queued until the old one finishes.]

### 4.1 Idioms: Named Sequences

Our imperative mode above needs a vocabulary.
The atomic diffs of §3 cover individual edits; many useful reconfigurations are multi-diff.
_Idioms_ are imperative mode's vocabulary: named multi-diff patterns the operator can write directly, just as they can write a single atomic diff.
The operator can also define their own idioms.

An idiom is a macro over the diff grammar (and, recursively, over other idioms).
It expands into a fixed `(φ; δ)*` sequence: a list of atomic diffs with the guards between them spelling out what the system waits for.
Soundness is compositional: each step of the expansion is sound by §3.4, and the sequence inherits the §4 sequence-level reasoning above.
An idiom expansion that would hit an undefined production on the current control is rejected — the soundness checks fire on the expanded sequence just as they would on a hand-written one.

We name four starter idioms.
New ones can be added later without changing the framework, since an idiom is, in the end, just a named `(φ; δ)*` shorthand.

- **`Retire(path)`** = `(true; Quiesce(path)) ; (empty(path); Remove(path))`.
  Quiesces the subtree at `path`, waits for it to drain, then `Remove`s it.
  The operator-facing way to say "tear this subtree down gracefully."

- **`SlowRetire(path)`** = `(empty(path); Remove(path))`.
  Waits for the subtree at `path` to drain, then `Remove`s it.
  The user may use this if the subtree at `path` needs to receive a little more traffic but is generally being phased out.

- **`Replace(path, B)`** =

  ```
  (true; Designate(path, B)) ;
  (true; Quiesce(path ++ [0])) ;
  (empty(path ++ [0]); Undesignate(path))
  ```

  Designates `B` as the survivor of the current `pol@path` (which, after Designate, sits at `path ++ [0]` as the first arm of the inserted `Strict*` node), quiesces it, waits for it to drain, then collapses the `Strict*` onto `B`.
  At the pol level this is the `Replace` of §3.4 (`den(Undesignate) ∘ den(Designate(_, B))`); the operator-facing idiom adds the `Quiesce` + drain in the middle so that the original subtree empties out before the collapse fires.

- **`PruneDownTo(path)`** = `Retire(p_1) ; ... ; Retire(p_m) ; (true; ChangeRoot(path))`, where `p_1, ..., p_m` are the off-path subtrees along the route from the root to `path`.
  Each `Retire` brings one ancestor a step closer to being unary; once all of them have fired, every ancestor along the path is unary, satisfying `ChangeRoot`'s precondition (§3.3).
  The operator-facing way to say "abandon everything except this subtree."

[AM TK: short note on `nextnext`.
This paper's transition planner engages with one `prev -> next` pair at a time.
If the operator submits a follow-up `nextnext` while a `prev -> next` sequence is still mid-flight (i.e., while some guard `φ` has not yet become true), our answer is the simplest possible one: `nextnext` is queued and the transition planner does not begin work on it until the in-flight sequence completes.
See `paper/discussion-separable-nextnext.md` for a stronger possibility.]

## 5. Identifying Better Transitions

§3 gave us a toolkit of atomic diffs and §4 sequenced them through `link`s into full reconfigurations.
With those tools in hand, this section asks how the transition planner can wield them well.
The metric we adopt is _confinement_: a good sequence is one whose diffs and intervening `link`s disturb as little of the running scheduler as possible, leaving the parts of the tree that did not need to change running undisturbed.

There is always a maximally unconfined fallback.
To reach any `next` from any `prev`, the planner can issue `Designate([], next)`, making the whole of `next` the survivor of the whole of `prev`.
All new traffic flows to `next` at once, every `pop` is served by `prev` until `prev` drains, and a closing `Remove` discards `prev` and leaves `next`.
Nothing is dropped and the sequence is safe by §3.4, but no part of the scheduler is left undisturbed.
This is the worst case the planner ever falls back on.
The rest of §5 is about doing better: localizing the change so that the sequence and its `link`s touch only a small subtree, while the rest of the scheduler keeps running undisturbed.

We make no claim that the planner is canonical or minimal.
We claim only that whatever sequence it emits is safe (§3.4) and no worse than this fallback.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself.
TK.]

## 6. Compiling to Hardware

Leaving for Zhiyuan.
We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO).
  This is not the point of the contribution.
  We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- `Designate` (§3.3) realizes the `Strict` wrap as an in-place gadget: a _super-node_ `{A -> B}` that occupies `A`'s slot directly, exposes a pop order strictly favoring `A` over `B`, and adds no PE depth.
  Detail the gadget's representation, its commit sequence, and the `{A -> B} -> B` collapse that `Remove(path)` triggers once `A` is empty.
- [AM: question for Zhiyuan: §3.5 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.4.1 diff.
  But we also claim that we compose with _any_ PIFO substrate.
  So what do we actually require from a substrate?
  Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states?
  Do you know if vPIFO supports this?
  If a substrate cannot hide those states, does composition break?
  What do we genuinely need to assume?]

## 7. Evaluation

## 8. Related Work

## 9. Conclusion
