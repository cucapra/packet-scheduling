# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem. Running example: a small-office gateway runs `Strict(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic. The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `Strict(gmail, zoom, spotify)`: just extend the strict-priority list with `spotify` having lowest priority.
  2. `Strict(gmail, RoundRobin(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, SOTA would stop the world, drop/recirculate buffered packets, recompile, and reinstall. Costs: dropped or recirculated (read, delayed!) packets, downtime, and a full respawn of nodes that did not need respawning.

- The alternate is to reprogram a scheduler without stopping the world. Let's revisit the examples from earlier.
- Transitioning to `Strict(gmail, zoom, spotify)` is actually quite easy. We can achieve the strongest property we could ask for:
  - time1: `Strict(gmail, zoom)` is running
  - time2: the request to move to `Strict(gmail, zoom, spotify)` is received. `Strict(gmail, zoom)` is still running
  - time3: we move to `Strict(gmail, zoom, spotify)`. Whatever user-observable interaction (push/pop) happened immediately before time3 happened entirely in the `Strict(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened entirely in the `Strict(gmail, zoom, spotify)` regime. We therefore refer to the transition at time3 as _atomic_.
  - [AM note: time2 = time3 in this case? I don't want to say it like that here, since it would conflate atomicity with immediacy, but do we think someone will scratch their head and ask why any time is needed at all?]
- What about transitioning to `Strict(gmail, RoundRobin(zoom, spotify))`? It is not as easy. We need to atomically step into a _transitionary period_ during which the scheduler still accepts and emits packets, and once certain well-defined conditions are met, we atomically step into the user-requested policy.
- It is crucial to note that, although the user never described to us the semantics of the transitionary period, it _is_ in fact a _de facto_ packet scheduling regime with some semantics! It is useful to recognize it as a scheduling policy in its own right (we give it the name `link`). There are clearly better and worse transitions from a network operator's point of view. SOTA has more-or-less unintentionally adopted a trivial stop-the-world `link`. Our contributions include both being clear about what `link` is and improving on it.
- Concretely:
  - Obligation 1. Show that `link` is just scheduling. The transition period is motivated by hardware-level manipulations on the way to realizing `next` and is not designed for a clean human-readable semantics, so the natural fear is that we owe the reader a new formalism for it. Our result is the opposite: each `link` is an ordinary scheduling control in the sense of §3.1, no new semantics is required.
  - Obligation 2. Improve upon SOTA. We will give the semantics of SOTA's stop-the-world `link` and use it as a baseline. We have a set of practical goals that may guide us as we search for the ideal way to transition from some `prev` to some `next`. Can we minimize the length of the transition period? Can we avoid dropped/delayed packets? [AM: more to come here; the cost model is a legit open question!]. We will show that it is always possible to make a transition from any `prev` to any `next`, but it is occasionally possible to make a very efficient transition. We contribute a tool that achieves this and shows [improvements].

## 2. Background & Motivation

### 2.1 Hierarchical scheduling on hardware

- Packet scheduling, PIFOs, and hierarchical scheduling with PIFO trees.
- Mapping a hierarchy onto hardware: each tree depth lands on a PE; siblings and cousins share a PE. This has been known since Sivaraman (SIGCOMM '16).

### 2.2 vPIFO, and the problem it leaves open

- vPIFO (Zhang et al., SIGCOMM 2024) is the closest related work, and it explicitly leaves our problem open.
- What vPIFO does. It virtualizes a single physical PIFO into many logical "PIFO instances," with a Scheduling Description Language (SDL) and compiler, so their PIFO Visor can _flexibly establish_ hierarchical PIFO trees of arbitrary shape on fixed hardware. Its contribution is the reconfigurable substrate.
- What vPIFO does not do. As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change. The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- Our two running examples make the gap concrete. The easy append, to get to `Strict(gmail, zoom, spotify)`, looks like the kind of edit vPIFO's substrate is well placed to absorb in place: a targeted append to the structures that encode the tree shape and per-instance storage (the Operation Generation Table and the PIFO Instance Address Table) would plausibly register the new traffic and its operations while leaving the running instances untouched. vPIFO as published does not engage with this question; their only option is a full reinitialization. Driving such a substrate to install the edit in place, straight from the (small) diff, is exactly what our layer adds. The harder restructuring, to `Strict(gmail, RoundRobin(zoom, spotify))`, is beyond what the vPIFO substrate can absorb in place at all: it changes the running tree's shape (inserting a new internal node and re-parenting `zoom` under it), which is not exposed as an in-place edit on the Operation Generation Table or the PIFO Instance Address Table. The closest vPIFO comes is a full reinitialization onto the new shape, which is exactly the stop-the-world baseline we are trying to improve on.
- vPIFO's own §9 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly. We are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer _above_ a PIFO substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose.

## 3. A Grammar of Atomic Policy Diffs

§3.1 recaps the PIFO tree model. §3.2 defines a small policy DSL and a compiler from it into a runnable _control_, giving us the syntactic handle on controls that the rest of the section needs. §3.3 fixes a grammar of structural edits (_diffs_) over that DSL, where every diff is atomic by construction. §3.4 restates atomicity once the operational rewrite `[[δ]]` is in hand and discharges three obligations per production: realization of the pol-level denotation, preservation of well-formedness, and preservation of shared state. §3.5 argues that all of this survives the lowering to hardware. Composing diffs into the sequences that realize a full reconfiguration, and the `link` schedulers that arise between them, is deferred to §4.

### 3.1 Background: PIFO trees

The PIFO tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3] is what we build on. We recap topology, the two observable operations, and well-formedness.

##### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

##### The two observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ observable interactions with a scheduler, which is why our notion of an _atomic_ transition is stated in terms of `push`/`pop` observability.

##### Well-formedness

A PIFO tree `q` is well-formed (written `|- q`) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. `push` always preserves `|- q`, and `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

### 3.2 A Policy DSL

A PIFO tree is a runtime representation, not a programming surface. To engage with reconfigurations we lift one level: a small policy DSL the operator writes in, and a compiler from DSL terms to runnable _controls_ (defined below). The transition planner of §4 needs both a way to materialize a starting control `C` from an operator's wish and a way to compare two wishes; the DSL gives us both.

This is morally what the vPIFO paper's _Scheduling Description Language_ does informally [cite vPIFO, §4]. They do not pin down a grammar for SDL or formalize the compilation, so our DSL can be read as a formal core of their concrete language. The compiled object differs (they target a virtualized PIFO substrate; we target a control) but the strategy is similar: give the operator a syntactic handle, then compile.

##### Policy syntax: `pol`

```
pol    ::= flow                                          // leaf, labeled by a flow of traffic
         | D(pol_1, ..., pol_n)                          // internal node, unweighted discipline D
         | W((w_1, pol_1), ..., (w_n, pol_n))            // internal node, weighted discipline W
```

This grammar naturally admits policy trees with unbounded arity. `D` ranges over the unweighted disciplines (`Strict`, `RoundRobin`, ...) and `W` over the weighted ones (just `WFQ` for now); each `w_i` is a positive real. We read the arity off by counting children, so `Strict(gmail, zoom)` is the 2-ary instance. A `pol` is _valid_ when every discipline is applied at a proper arity and leaf labels are distinct. Validity is a purely syntactic condition on the source, not to be confused with the invariant `|- q`.

##### Discipline compilation: `init_node_D` and `init_slot_D`

Each discipline `D` comes with a mechanical recipe for compiling a node that runs it: (a) a per-node ranking program (the node's `z`), (b) an initial `node_state` for the node, and (c) a `slot_state` for each arm attached under the node. We name the two state-seeding projections; the ranking program is handled by the compilation walk below and needs no separate name.

```
init_node_D : () -> node_state
init_slot_D : node_state × weight? -> slot_state
```

`init_node_D` is called only at _compile time_, once per node, to seed that node's `node_state`. `init_slot_D` is called in two places. At _compile time_, walking the source pol, it is called once per child arm to seed that child's `slot_state`, taking the parent's just-seeded `node_state` as input. At `Add` (§3.4.1), when a new arm is spliced under an already-running `D`-parent, `init_slot_D` is called once with the parent's _current_ `node_state` to produce the new arm's `slot_state`. The function is the same in both cases; only the source of the parent's `node_state` differs.

Choosing `init_slot_D` is a scheduling decision, not just a structural one, since the choice changes how a freshly spliced arm competes with the established arms. Our plan is to "join the current round". For example, if we go from `WFQ(A,B)` to `WFQ(A,B,C)`, the newly added `C` does not reap a huge benefit for having been silent all this while; it just joins the others with neither a huge penalty nor a huge advantage.

To this end: `init_slot_Strict` and `init_slot_RR` return the empty tuple (no per-arm bookkeeping to seed). For `WFQ`, the `node_state` at a parent is the scalar virtual time `vt`, and we set `init_slot_WFQ(vt, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag. By WFQ's standard finish-time recurrence the first packet on this arm posts a tag of `max(virtual_clock, vt) + 1/w`, which slots it into the round the established arms are currently in. At compile time the parent's `vt` is freshly initialized (to zero), so all original arms get `(w, 0)` and the round is "the zeroth"; at `Add` the parent's `vt` is whatever the clock has advanced to.

##### Compilation: `pol` to control

A _control_ `C` is a tree of node-local triples `(state, pifo, z)`, one per node of the topology. The tree shape exactly matches that of `pol`, as each node of `C` lines up with a node of the source `pol`. The diffs of §3.4 act on one (occasionally a few) of these node-local triples; the whole control is just the assembled tree.

At each node of `pol`'s topology, running discipline `D`, compilation seeds the three pieces:

- `state` is a pair `(node_state, slot_state list)`. The `node_state` carries `D`'s per-node bookkeeping (an `RR` cursor, a `WFQ` global virtual time), seeded by `init_node_D()`. The `slot_state` list carries per-arm bookkeeping, one entry per child arm in slot order (a `WFQ` per-arm virtual finish, a `WRR` per-arm credit, the arm's weight under `WFQ`), each entry seeded by `init_slot_D`. Disciplines without per-arm bookkeeping (`Strict`, pure `RR`) have an empty `slot_state` list.
- `pifo` is an empty PIFO: an index-PIFO at an internal node, a packet-PIFO at a leaf.
- `z` is `D`'s ranking program at the node: at an internal node, it picks a child index and the rank to enqueue at that index; at a leaf, it picks the rank for the packet's own PIFO entry. `z` may be partial: a packet for which `z` is undefined is dropped on the spot, with no descent and no state change.

We address nodes by `path` (§3.3): the local triple at the node reached by following `path` from `C`'s root is written `C@path`, with fields `C@path.state`, `C@path.pifo`, and `C@path.z`. We also write `C@path.node_state` and `C@path.slot_state` for the two components of `C@path.state`.

##### Well-formedness: `|- C`

A control `C` is _well-formed_ (written `|- C`) when, at every internal node of `C`, the `pifo` has, for each legal child index `i`, exactly as many occurrences of `i` as there are packets stored in the leaf pifos of the subtree under the `i`-th child. This is the per-node lift of `|- q` from §3.1 and is stated directly on `C`: no global PIFO tree need be assembled to check it.

##### Compatibility with Formal Abstractions

FA's controls are a single triple `(s, q, z)` with a state map `s`, a PIFO tree `q`, and a single transaction `z : St × Pkt -> Path(t) × St` (total). One can flatten our `C` into such a triple by `glue(C)`: `glue(C).q` is the tree of our `pifo` pieces, `glue(C).s` collects the `state` pieces indexed by path, and `glue(C).z` walks the topology applying each `z` piece in turn. The partiality our per-node `z`s allow shows up as partiality on `glue(C).z` (a drop anywhere along the descent leaves the global function undefined for that packet). The rest of the paper has no need for `glue(C)` — `|- C` is stated directly per the previous paragraph, and the diff rules of §3.4 act node-locally — but a reader more at home in FA's framing can recover it this way.

##### The bridge: `⌊·⌋`

Reading the topology, disciplines, weights, and leaf labels off a control `C` recovers a `pol`. We write `⌊C⌋` for the `pol` recovered from control `C` and say that `C` _realizes_ `⌊C⌋`. The correctness condition for the compiler above is then `⌊compile(pol)⌋ = pol`.

The map `⌊·⌋ : control -> pol` is many-to-one: `pol` records only the structural skeleton, while a control also carries the live `state` and `pifo` at each node. In particular, every control that arises by running `compile(pol)` and then serving traffic for a while still realizes `pol`. We need this split so that, when we later edit a running control in place, we can formally ask _what `pol` that modified control realizes_.

### 3.3 A Grammar for Tree Diffs

The user writes down `pol` objects, like `prev`. Say that we compile the user's request `prev` into some control `C`, s.t. `C` realizes `prev` (written `prev = ⌊C⌋`). Then the user requests a change to a new `pol` object called `next`. SOTA would compile `next` into some control `C'` and clobber the running `C` with `C'`. We wish to achieve the same result while being less disruptive to unaffected parts of the running control.

A change to the live control is _atomic_ when its effect falls between two observable operations: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if the change fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by the pre-change control and `op_{N+1}, ...` are served entirely by the post-change control. No operation straddles the change, and no operation sees an intermediate state. §1's running example described this informally.

We fix a small grammar of atomic edits. Each production denotes a single primitive that acts on a live control `C` and produces a new control. The grammar discussed in this section is the alphabet using which the transition planner (§4) assembles a sequence whose operational composition takes `C` to a control realizing `next` _without clobbering_. Most productions are `pol`-visible: their effect shows up in `next`, and a comparison of `prev` against `next` is enough to understand them. Others are transaction-only: their effect lives entirely in `z`, leaving the `pol`-level skeleton untouched.

An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. We write `t@path` for the subtree of `t` reached by following `path` down from `t`'s root. Depending on the production being used, `t` is instantiated to `prev` or `next`; see below.

```
δ      ::= Add          (path, pol, weight?)
         | Quiesce      (path)
         | Designate    (path, pol)
         | Remove       (path)
         | ChangeWeight (path, weight)
         | Graft        (ctx)

path   ::= []  |  i :: path                       // i is a child index
ctx    ::= □                                      // the unique hole; takes no children
         | D(pol, ..., ctx, ..., pol)             // n children total; exactly one is itself a context
weight ::= a positive real
```

`pol` is the nonterminal of §3.2. A _policy context_, written `ctx`, is built like a `pol`, except that exactly one of its slots is the distinguished _hole_ `□` rather than a subtree. The hole is a reserved slot, not an absence of a slot: the parent of the hole has an arity that includes the hole, e.g., `RoundRobin(A, B, □)` is a 3-ary `RoundRobin` whose third slot is the hole, distinct from the 2-ary `RoundRobin(A, B)`; both are valid. We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`. A `ctx` is _valid_ iff `ctx[s]` is a valid `pol` for some valid `s`. The plug is total, and for any valid `ctx` and any valid `s`, `ctx[s]` is a valid `pol` whenever their leaf labels are disjoint.

The grammar is shaped by what we can realize atomically in hardware (§6): each production is exactly an edit for which we have a substrate-level commit. That commit slips in between two consecutive `push`/`pop` operations.

Edits that would have to destroy structure still holding packets are _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§5) only after ensuring that the subtree being removed is empty. The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _sequences_ of these atomic edits; §4 makes the sequencing precise.

Notes on the individual edits.

- `Add(path, pol, weight?)` splices `pol` in as the new slot `next@path`. The production carries a `weight` exactly when the slot it edits hangs off a WFQ parent.
- `Quiesce(path)` prevents `prev@path` from receiving new traffic: incoming traffic that used to classify into its leaves is dropped, and the subtree drains under ordinary service of its already-buffered packets. Topology, disciplines, and labels are unchanged; the edit's whole effect is in `z`, so it is `pol`-invisible.
- `Designate(path, pol)` converts `prev@path` into `Strict(prev@path, pol)` in place. The existing subtree `prev@path` becomes the high-priority sibling (set to drain), and `pol` becomes the low-priority _designated survivor_. Operationally, `z` is rewired so that traffic that used to classify into `prev@path` is now offered to the survivor instead: the survivor's own classifier decides what to do with each packet, accepting any it has a leaf for and dropping the rest via the partial-`z` mechanism of §3.2. We make no assumption about how much overlap there is between `prev@path`'s flows and the survivor's. If the survivor has a leaf whose label coincides with one under `prev@path`, the differ names that survivor leaf with a fresh primed label (`A'` is read as "packets that would have gone to `A` before the edit") so the post-edit `pol` has, by construction, disjoint leaf labels and remains valid. The production does not need to carry a weight even when the parent at `path`'s prefix runs WFQ, since the new `Strict` node inherits `prev@path`'s slot and weight. Literally inserting this `Strict` node in the middle of the running tree would be expensive, as it would require relocation the entire subtree `prev@path` one PE deeper (because siblings and cousins must share a PE, see §2.1). §6 features a new in-place hardware gadget that gives us the `Strict` semantics described here without incurring that relocation cost.
- `Remove(path)` structurally removes `prev@path`, dropping its slot and renumbering any higher siblings. The subtree must be empty; §3.4.2 discusses why.
- `ChangeWeight(path, weight)` overwrites the weight that `prev@path`'s parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft(ctx)` produces `ctx[prev]`: the policy context `ctx` is spawned around `prev`, with `prev` plugged into the context's sole hole. `Graft` carries no `path`: if the user wants localized graft-style edits, deeper in the tree, they must be realized as a sequence (§4), not by a path-bearing `Graft`.

When `prev = next` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.4 All Productions of `δ` are Sound

Each `δ` (§3.3) admits two semantic readings, both _partial_. The operational reading is primary; the pol-level reading is a projection of it.

- The _operational transition_, `[[δ]] : control ⇀ control`, is the live rewrite acting on the control `C`. The per-production rules below state where `[[δ]]` is defined; in other cases we say `δ` is _incompatible_ with the input control, and `[[δ]](C)` returns nothing. §4's transition planner only emits a `δ` whose `[[δ]]` is defined on the live `C`. The preconditions vary by production, e.g.:
  - For `Add` we require the path to land at an internal node, a weight to be present iff the parent runs WFQ, the new leaf labels to be fresh, and the new classifier predicates to be disjoint from the domain of `C`'s live `z`.
  - For `Remove` we require the target subtree to be empty.
  - For `ChangeWeight` we require the parent at `path`'s prefix to run WFQ.
- The _pol-level denotation_, `den(δ) : pol ⇀ pol`, is the projection of `[[δ]]` through the `⌊·⌋` bridge of §3.2. It is partial for the same syntactic reasons (paths must resolve, leaf labels must be fresh, weights must match), but, because `pol` carries no live contents or transaction, it never fails for operational reasons like `Remove`'s emptiness or `Add`'s classifier disjointness. It captures the pol-visible part of the edit. For most primitives (`Add`, `Remove`, `ChangeWeight`, `Graft`, and `Designate`'s wrap-and-rename), `den(δ)` is non-trivial and matches what a reader would extract from comparing `prev` against `next`. `Quiesce`, whose whole effect is in `z`, has `den(Quiesce)` equal to the identity on `pol`. The z-rewiring inside `Designate` lives only in `[[Designate]]`, not in `den(Designate)`. So `den(δ)` is a purely static map, and effects that live entirely in `z` are invisible to it. §4's planner works at this level: it searches for a sequence of edits such that composing their `den`otations together carries `prev` to `next`.

The two readings are linked through `⌊·⌋`: whenever `[[δ]](C)` is defined, so is `den(δ)(⌊C⌋)`, and in fact `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)`. To put it in notation: `den(δ) : pol -> pol` is defined to be the relation `{ (⌊C⌋, ⌊C'⌋) | [[δ]](C) = C' }`.

In the language of `[[δ]]`, _atomicity_ can be redefined crisply: when `δ` is compatible with `C`, `[[δ]]` replaces the live control `C` with `[[δ]](C)` between two consecutive `push`/`pop` operations. Modeling `[[δ]]` as a single (partial) function `control ⇀ control` builds the indivisibility into the abstraction; the obligations below ensure that the result of this single step is correct.

For each production we discharge three obligations, all implicitly conditioned on `[[δ]](C)` being defined.

- _Realization_ constrains the `pol`-visible skeleton: `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)`. The `pol` summary of the post-edit control must equal the pol-level denotation applied to the `pol` summary of the pre-edit control. For our running example, `δ = Add([2], spotify)`, so `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and realization says `⌊[[δ]](C)⌋ = Strict(gmail, zoom, spotify)` for any `C` with `⌊C⌋ = Strict(gmail, zoom)`. The equation pins down the new topology, disciplines, weights, and labels. Transaction-only effects (`Quiesce`'s shrinking of `z`'s domain, `Designate`'s rerouting) lie outside `⌊·⌋` and are pinned down by the per-production operational rule below, not by realization. Realization says nothing about contents.
- _Soundness_ constrains the live contents: `|- C` must imply `|- [[δ]](C)`. Well-formedness (§3.2) is about per-node pifos and the packets stored under them, so this is the obligation that the packets and index entries that `[[δ]]` drops or adds must still satisfy the counts.
- _State preservation_ constrains each local control's state. Writing `C` for the pre-edit control and `C'` for the post-edit control, at every node structurally shared between `prev` and `next` and outside the production's local edit site, the local `state` is preserved verbatim. At the edit site and at every node inside a freshly-spawned subtree, the state is exactly what the production's `init`-rule prescribes, where the `init`-rule is the per-discipline `init_node_D` / `init_slot_D` of §3.2 invoked as specified per-production below.

The denotational rules frequently read, overwrite, and splice child lists. Let us fix some notation. We write `ts[i]` for the `i`-th child and `ts[t/i]` for `ts` with its `i`-th child overwritten by `t`; this leaves the arity unchanged. The two arity-changing edits carry a sign: `ts[+t/i]` is `ts` with `t` spliced in as the new `i`-th child (the old `i`-th and later children shift one place to the right), and `ts[-/i]` is `ts` with its `i`-th child dropped (later children shift left). Indices follow the `path` convention of §3.3.

The operational diffs of §3.4 manipulate two parallel structures at an internal node: a child list `qs` of PIFO tree children, to which the list-manipulation notation introduced above applies verbatim, and an index-PIFO `p` recording child indices. For `p` the same `[±/k]` notation _renumbers_ rather than splices, because we often need to open up or close a slot by shifting indices. `p[+/k]` is `p` with every entry `>= k` bumped up by one (opening up slot `k`); `p[-/k]` is `p` with every entry `> k` brought down by one. Entries below the edit point are left alone.

#### 3.4.1. `Add(path, pol, weight?)`

##### Pol-level denotation

`den(Add(path, pol))` is the structural map, defined by recursion on `path`:

```
den(Add(i :: [],   pol)) (P ts) = P ( ts[+pol / i] )
den(Add(i :: rest, pol)) (P ts) = P ( ts[ den(Add(rest, pol)) (ts[i]) / i ] )
```

The base case fires once `path` reaches the new slot's parent: the subtree `pol` is spliced in as the new `i`-th child. (Recall from §3.3 that `Add`'s `path` is read in `next` and names the new slot, so its final index `i` is the insertion point.) The recursive case walks down a shared ancestor, recurses into child `i`, and writes the result back. A WFQ parent also needs the new slot's weight; the syntactic diff is `Add(path, pol, w)` in this case. With children zipped as `(weight, subtree)` pairs `cs`, the weight enters only at the insertion site, and descent leaves the weights untouched:

```
den(Add(i :: [],   pol, w)) (WFQ cs) = WFQ ( cs[ +(w, pol) / i ] )
den(Add(i :: rest, pol, w)) (WFQ cs) = WFQ ( cs[ (w_i, den(Add(rest, pol, w)) pol_i) / i ] )
                                                                     where (w_i, pol_i) = cs[i]
```

Our running example denotes `den(Add([2], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 2] ) = Strict(gmail, zoom, spotify) = next`, as intended. In our example we appended a new arm, but a mid-tree edit would be no more complicated: `den(Add([1], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 1] ) = Strict(gmail, spotify, zoom)`, with the old `zoom` sliding from index `1` to index `2`.

##### Operational transition

We write `c` for the parent node of the new slot (here the root `Strict`) and `k` for the new slot's index, so `path` lands at `c`'s `k`-th child. Let `D` be `c`'s discipline. The transition `C' = [[Add(path, pol, weight?)]](C)` is stated per node.

The topology gains a new arm at `c`, indexed `k`; the old arms at `c` with indices `>= k` shift right by one. The local controls update as follows.

- _At every node outside the new subtree, other than `c`:_ the local control is preserved verbatim.
- _At `c`:_
  - `C'@c.node_state = C@c.node_state` (unchanged).
  - `C'@c.slot_state = C@c.slot_state[ + init_slot_D(C@c.node_state, weight?) / k ]` (a plain append when `k` is last). The fresh entry is the per-arm bookkeeping `init_slot_D` of §3.2 prescribes for an arm newly spliced under a running `D`-parent.
  - `C'@c.pifo = C@c.pifo[+/k]`: every index-PIFO entry naming an old index `>= k` is bumped up by one, tracking the shift, so each pre-existing slot keeps its matched count of entries and packets under its new name. When `k` is the last index (the append case) nothing is `>= k` and the pifo is untouched.
  - `C'@c.z` extends `C@c.z` in two ways. First, it admits packets that classify into the new subtree, mapping them to child index `k` at `c` (with descent handed off to the new subtree's `z` pieces below). Second, it renumbers any old mapping whose chosen index at `c` was `>= k` to that index plus one, the path-level image of the pifo renumbering above. No `C` packet was ever routed to `k`: under `C'`, `k` names the new subtree, and the old occupant of `k`, if any, now lives at `k+1`.
- _At every node inside the new subtree:_ the local control is the freshly-compiled one §3.2's compilation rule prescribes for that node.

##### Soundness

We discharge the three obligations of §3.4 in turn (recall `C' = [[Add(path, pol, weight?)]](C)`).

- _Realization._ The updates above leave every pre-existing arm structurally intact (modulo renumbering of `c`'s pifo, which is invisible to `⌊·⌋`) and add a new arm at `c`'s slot `k` whose subtree is the freshly-compiled `pol`. So `⌊C'⌋ = den(Add(path, pol, weight?))(⌊C⌋)`: the new shape is exactly what the edit denotes.
- _Soundness._ `|- C` gives `|- C'`. At `c`, `C'@c.pifo = C@c.pifo[+/k]` by construction names no `k` (old entries `< k` stay put, old entries `>= k` were bumped to `>= k+1`), and the new subtree under slot `k` holds zero packets, so the well-formedness count at slot `k` reads `0 = 0`. Every other slot at `c` is the child it was, with the same packets beneath it; its entries in `C'@c.pifo` are the old entries under a possibly-shifted name, so its matched count is inherited verbatim. Every other node's pifo is untouched. Nothing needs repair.
- _State preservation._ The first and third bullets of the operational transition above discharge it directly: outside the edit site, the local control (and thus its `state`) is preserved verbatim; inside the new subtree, the state is freshly compiled per §3.2. At `c`, `node_state` is unchanged and `slot_state` splices in exactly `init_slot_D(C@c.node_state, weight?)` as §3.4 demands at the edit site.

##### Notes

_Atomicity._ No in-flight packet straddles the diff: every packet resident at the diff instant lives in the `pifo` of some pre-existing node, carried into that same node's `pifo` in `C'` unchanged, and none lives under the new slot `k`. The renumbering `C@c.pifo[+/k]` relabels index _values_ only; it moves no packets and changes no ranks. So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have, the empty new slot contributing nothing and the existing subtrees keeping their contents and relative priority under shifted index names. The first `push` that `C'@c.z` routes to `k` is the first packet ever to occupy the new subtree.

_Deeper paths._ The running example edits the root, but `path` may be any prefix; `Add([1, 2], pol)` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes whose local control is preserved verbatim, and, because the new subtree is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor pifo is rewritten, and the edit is confined to `c` and the fresh subtree below it.

#### 3.4.2. `Remove(path)`

`Remove` is the grammar's one structural deletion. It unhooks the subtree `prev@path` from its parent, drops the vacated slot, and renumbers any higher siblings. It is defined only when `prev@path` is _empty_. Retiring or replacing a subtree that still holds packets is therefore not `Remove`'s job alone: it is realized as a _sequence_ that first drains the subtree and only then removes it (see §4).

##### Pol-level denotation

##### Operational transition

##### Soundness

##### Notes

_Why an empty subtree?_ Deleting an _occupied_ child has no canonical local realization. Consider `P(Q(A, B), C)`. In the diagram below each internal node is labeled with its index-PIFO and each leaf with the packets it holds. We draw every PIFO with its most favorably ranked entry, the next one to be popped, on the far right: so the leaf `[a2,a1]` releases `a1` before `a2`.

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

Here `Q` refers to `A` using the index `1`, and to `B` using `2`. `P` refers to `Q` using `1` and to `C` using `2`. The tree is well-formed. `P` appears to be running a round-robin style policy and `Q` appears to be prioritizing `B` strictly over `A`. Say the user asks us to delete `B`, dropping its packets. To restore well-formedness, we need to remove two instances of the index `2` from `Q`'s PIFO and two instances of the index `1` from `P`'s PIFO.

At `Q` the edit is unambiguous. Well-formedness of the original tree forces `Q`'s PIFO to have exactly two instances of `2`; we just find and delete them. The trouble is one level up. `P`'s PIFO originally had four instances of `1`, and well-formedness demands that we remove two of them. But _which_ two? No entry in `P` remembers the meta-information "I was enqueued when a packet was inserted into `B`"; an entry `1` in `P` just means "when this index is popped, recursively ask subtree `Q` to emit the next packet" (see §2.1). This forgetting of meta-information is a feature of PIFO trees, not a defect: an entry must be detached from the packet it was enqueued with; this is exactly what lets each node schedule its own discipline alone, and lets a subtree be reconfigured without rewriting its ancestors. We could facilitate an unambiguous deletion by tagging each entry at `push` with the leaf it is destined for, but that throws the abstraction away: every internal node would then have to track the entire downstream structure, and the composability that makes PIFO trees scale would be lost.

Given this, _we can only pick two instances of `1` arbitrarily_. Our choice is silently a scheduling decision, as it affects how `P` intermixes `C` and `Q` traffic. For example:

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

Here `~1~` indicates an instance of `1` that we dropped. Six pops drain `P` and yield `c1, c2, c3, a1, c4, a2`: `A` is pushed to the back, both its packets emerging only after `C`'s run is mostly spent.

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

Same tree, same surviving packets, two different service orders. The _choice of which_ `1`s to drop _silently reschedules `A` against `C`_, and nothing in `prev` or the deletion request records which the operator wanted.

We do not wish to make a scheduling decision for the operator, so our only other option is to reconstruct the tree as if `B` had never been admitted: drain everything out, filter the `B`-packets, and re-push, recomputing every rank and cursor along the way. This is a stop-the-world rebuild.

Our grammar sidesteps the question entirely. By draining `B` to empty before removing it (§4), every pop that served a packet from `B` has already removed the matching index-entries from `B`'s ancestors as ordinary `pop` requires: a packet leaving `B` consumes a `2` at `Q` and a `1` at `P`, bringing each ancestor's count into accord with its post-drain contents. At the instant `Remove` fires, `Q`'s `2`-count is zero (`B` is empty) and `P`'s `1`-count is exactly the number of packets still under `Q` (here `2`, all attributable to `A`). There is nothing left to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

### 3.5 Preserving this proof down to hardware

§3.4 proves soundness at the tree-diff level, where each atomic edit (`Add`, `Quiesce`, `Remove`, ...) carries a well-formed tree to a well-formed tree. To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR: `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, and the like. A single IR instruction, unlike a whole tree-diff edit, _can_ leave the tree malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, for example.

We do not prove soundness at the IR level, but instead informally make the case for why the §3.4 proof survives the lowering. There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit. We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed. That commit is precisely how the substrate _realizes_ the atomicity property of §3: atomicity asked for an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant. Every `push`/`pop` therefore still lands on a well-formed control (`prev`, a `link`, or `next`), exactly as §3.4 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.4 proved. The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §6.

[AM note for Zhiyuan: the §3.5 argument leans on the substrate supporting _atomic transactional commits_: a multi-instruction lowering must install as a single instant from the user's perspective, so that the transiently-malformed intermediate trees inside a commit are never observed. Our own substrate provides this. The open question for §6 is whether composition with a third-party substrate (e.g., vPIFO) requires the same property and, if so, whether vPIFO offers it. Flagged here so the §3.5 claim "the proof survives the lowering" is not read as substrate-independent.]

## 4. Realizing Reconfigurations as Sequences

[AM TK: stub. §3 proved each grammar production a sound atomic diff. This section composes diffs into _sequences_ `(φ ; δ)*` of `(guard, diff)` pairs, where a guard `φ` is a predicate on the state of the live control `C` (any `φ` may be `true`, meaning the paired diff fires at once; `φ_0 = true` is the common case), to realize reconfigurations no single diff can express. The headline result will be that the transitionary scheduler `link` between two consecutive diffs is itself an ordinary §3.1 control, so the "transitionary period" needs no new semantics: this is Obligation 1 of §1, discharged. The section also considers _liveness_: whether and when a sequence's guards become true. We discussed IRL that this will be a nice-to-have that we press upon the user. If the user requests a new change while the old change has not fully landed, then their new change will just be postponed until the old change lands.]

[AM TK: short note on `nextnext`. This paper's transition planner engages with one `prev -> next` pair at a time. If the operator submits a follow-up `nextnext` while a `prev -> next` sequence is still mid-flight (i.e., while some guard `φ` has not yet become true), our answer is the simplest possible one: `nextnext` is queued and the transition planner does not begin work on it until the in-flight sequence completes. See `paper/discussion-separable-nextnext.md` for a stronger possibility.]

## 5. Identifying Better Transitions

§3 gave us a toolkit of atomic diffs and §4 sequenced them through `link`s into full reconfigurations. With those tools in hand, this section asks how the transition planner can wield them well. The metric we adopt is _confinement_: a good sequence is one whose diffs and intervening `link`s disturb as little of the running scheduler as possible, leaving the parts of the tree that did not need to change running undisturbed.

There is always a maximally unconfined fallback. To reach any `next` from any `prev`, the planner can issue `Designate([], next)`, making the whole of `next` the survivor of the whole of `prev`. All new traffic flows to `next` at once, every `pop` is served by `prev` until `prev` drains, and a closing `Remove` throws away `prev` and leaves `next`. Nothing is dropped, the sequence is safe by §3.4, and no part of the scheduler is left running undisturbed. This is the floor. The rest of §5 is the story of doing better: localizing the change so that the sequence and its `link`s touch only a small subtree while the rest of the scheduler keeps running.

We make no claim that the planner is canonical or minimal. We claim only that whatever sequence it emits is safe (§3.4) and no worse than this fallback.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself. TK.]

## 6. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- `Designate` (§3.3) realizes the `Strict` wrap as an in-place gadget: a _super-node_ `{A -> B}` that occupies `A`'s slot directly, exposes a pop order strictly favoring `A` over `B`, and adds no PE depth. Detail the gadget's representation, its commit sequence, and the `{A -> B} -> B` collapse that `Remove(path)` triggers once `A` is empty.
- [AM: question for Zhiyuan: §3.5 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.4.1 diff. But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 7. Evaluation

## 8. Related Work

## 9. Conclusion
