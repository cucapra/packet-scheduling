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

§3.1 recaps the PIFO tree model. §3.2 defines a small policy DSL and a compiler from it into an _FA_-style control, giving us the syntactic handle on controls that the rest of the section needs. §3.3 fixes a grammar of structural edits (_`diff`s_) over that DSL, where every `diff` is atomic by construction. §3.4 restates atomicity once the operational rewrite `[[δ]]` is in hand and discharges three obligations per production: realization of the pol-level denotation, preservation of well-formedness, and preservation of shared state. §3.5 argues that all of this survives the lowering to hardware. Composing diffs into the sequences that realize a full reconfiguration, and the `link` schedulers that arise between them, is deferred to §4.

### 3.1 Background: PIFO trees, formally

We build on the PIFO tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3]. §3.1.1 recaps the pieces of _FA_'s formalism that this paper actually leans on; §3.1.2 lays out two small extensions to that formalism.

#### 3.1.1 From _Formal Abstractions_

##### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

##### The two observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ observable interactions with a scheduler, which is why our notion of an _atomic_ transition is stated in terms of `push`/`pop` observability.

##### Well-formedness

A PIFO tree `q` is well-formed (written `|- q`, following _Formal Abstractions_' notation) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. _FA_ shows that `push` always preserves `|- q`, and that `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

##### Control

_FA_ introduces a _control_ `(s, q, z)`: a current state `s` drawn from some fixed set `St`, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z : St × Pkt -> Path(t) × St` that, given a state and a packet, returns a path of the correct shape together with an updated state. That path pins down both _where_ a packet goes (via its index sequence) and _with what priority_ it competes for service at every level (via its rank decorations), so `z` is the sole locus of routing and ranking.

#### 3.1.2 Our extensions to _FA_'s model

##### Dropping packets: `z` made partial

We extend _FA_'s `z` from total to partial: `z : St × Pkt ⇀ Path(t) × St`, where an undefined entry means the packet is dropped (no path, no state update, `q` untouched). This means that our `z` is also in charge of _packet admission_. _FA_'s total typing is the special case where the domain is all of `St × Pkt`, and every _FA_ result we lean on carries through verbatim because a dropped packet never reaches `q`.

##### Local state, made explicit

[AM note: it's not done here, but one option is to explode open FA's (s,q,z) triple into a decorated tree structure with per-node s and per-node z. It would be uncontroversial and reversible.]

We commit to slightly more structure on `s` than _FA_ does. We treat `s` as a partial map from positions in the topology to local state, `s : path ⇀ LocalState`, where a position is a (possibly empty) sequence of child indices addressing a node from the root (the same `path` syntax §3.3 uses for the diff grammar), and `s(p)` is defined exactly when `p` reaches a node of `t`. A `LocalState` is a pair `(node_state, slot_state list)`: a `node_state` for the discipline's per-node bookkeeping (an `RR` cursor, a `WFQ` global virtual time), and a list of `slot_state` entries (one per arm, in slot order) for per-arm bookkeeping (a `WFQ` per-arm virtual finish, a `WRR` per-arm credit, the arm's weight under `WFQ`). Disciplines without per-arm bookkeeping (`Strict`, pure `RR`) have an empty `slot_state` list. _FA_ leaves all of this abstract; making it explicit lets the diff rules of §3.4 write equations on `s` instead of prose.

### 3.2 A Policy DSL

_FA_ gives us a representation of in-flight PIFO trees, but offers no "constructor" that takes an operator's wish (e.g., `Strict(gmail, zoom)`) and emits a control with the appropriate state variables, a PIFO tree with the right topology and empty queues, and a scheduling transaction that actually implements strict prioritization via the paths it emits. Nor, given two controls, does _FA_ offer any way to compare them: `z` is just a function, and one cannot pattern-match on a function. The transition planner of §4 needs both: a way to materialize a starting `C` from an operator's wish, and a way to compare an operator's old wish against their new wish. We supply both by lifting one level above _FA_: a small policy DSL whose terms compile straightforwardly into _FA_-style controls.

This is morally what the vPIFO paper's _Scheduling Description Language_ does informally [cite vPIFO, §4]. They do not pin down a grammar for SDL or formalize the compilation, so our DSL can be read as a formal core of their concrete language. The compiled object differs (they target a virtualized PIFO substrate; we target an _FA_-style control), but strategy is similar: give the operator a syntactic handle, then compile.

##### Policy syntax: `pol`

```
pol    ::= flow                                   // leaf, labeled by a flow of traffic
         | D_n(pol_1, ..., pol_n)                 // internal node, n-ary discipline D
```

This grammar naturally admits policy trees with unbounded arity. `D` ranges over a known set of scheduling disciplines (`Strict`, `RoundRobin`, `WFQ`, ...). The subscript `n` is the arity, which we elide when it is clear from counting children: we write `Strict(gmail, zoom)` for `Strict_2(gmail, zoom)`. WFQ pairs each child with a weight, elided from the grammar above. A `pol` is _valid_ when every discipline is applied at a proper arity, a child carries a weight exactly when its parent runs `WFQ`, and leaf labels are distinct. Validity is a purely syntactic condition on the source, not to be confused with the invariant `|- q`.

##### Discipline compilation: `init_node_D` and `init_slot_D`

Each discipline `D` comes with a mechanical recipe for compiling a node that runs it: (a) a per-node ranking program contributing to `z`, (b) an initial `node_state` for the node, and (c) a `slot_state` for each arm attached under the node. We name the two state-seeding projections; the ranking program is handled by the compilation walk below and needs no separate name.

```
init_node_D : () -> node_state
init_slot_D : node_state × weight? -> slot_state
```

`init_node_D` is called only at _compile time_, once per node, to seed that node's `node_state`. `init_slot_D` is called in two places. At _compile time_, walking the source pol, it is called once per child arm to seed that child's `slot_state`, taking the parent's just-seeded `node_state` as input. At `Add` (§3.4.1), when a new arm is spliced under an already-running `D`-parent, `init_slot_D` is called once with the parent's _current_ `node_state` to produce the new arm's `slot_state`. The function is the same in both cases; only the source of the parent's `node_state` differs.

Choosing `init_slot_D` is a scheduling decision, not just a structural one, since the choice changes how a freshly spliced arm competes with the established arms. Our plan is to "join the current round". For example, if we go from `WFQ(A,B)` to `WFQ(A,B,C)`, the newly added `C` does not reap a huge benefit for having been silent all this while; it just joins the others with neither a huge penalty nor a huge advantage.

To this end: `init_slot_Strict` and `init_slot_RR` return the empty tuple (no per-arm bookkeeping to seed). For `WFQ`, the `node_state` at a parent is the scalar virtual time `vt`, and we set `init_slot_WFQ(vt, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag. By WFQ's standard finish-time recurrence the first packet on this arm posts a tag of `max(virtual_clock, vt) + 1/w`, which slots it into the round the established arms are currently in. At compile time the parent's `vt` is freshly initialized (to zero), so all original arms get `(w, 0)` and the round is "the zeroth"; at `Add` the parent's `vt` is whatever the clock has advanced to.

##### Compilation: `pol` to control

With `init_node_D` and `init_slot_D` in hand, a `pol` gives us a control `(s, q, z)` straightforwardly.

- Erase the labels, and what remains is the topology. Instantiate that topology with an empty PIFO at every node, and you have the tree `q`.
- Walk the policy.
  - At every node running `D`, emit `D`'s ranking program into the global `z`.
  - Call `init_node_D()` to seed the node's `node_state`, and call `init_slot_D` once per child arm to seed that arm's `slot_state`. Union the per-node and per-arm seeds into the global `s`.

##### The bridge: `⌊·⌋`

We write `⌊C⌋` for the `pol` from which control `C` was compiled. We say that `C` _realizes_ `⌊C⌋`. This `pol`/`control` split is what structures §3.4: a syntactic diff (§3.3) admits two semantic readings:

- A `pol`-level denotation `den(δ) : pol -> pol` that fixes the static skeleton it produces.
- An operational transition `[[δ]] : control -> control` that does the live rewrite.

`⌊·⌋` is the bridge that lets us state when the two semantic readings agree.

[AM note: `⌊C⌋` as defined here works only on freshly-compiled controls. Once `C` has been edited in place, the live `C'` is no longer the output of `compile` on any single `pol`, so statement "the pol from which `C` was compiled" stops being well-defined. §3.4's realization equation `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)` nonetheless treats `⌊·⌋` as defined on post-edit controls. Discuss IRL.]

### 3.3 A Grammar for Tree Diffs

The user writes down `pol` objects, like `prev`. Say that we compile the user's request `prev` into some control `C`, s.t. `C` realizes `prev` (written `prev = ⌊C⌋`). Then the user requests a change to a new `pol` object called `next`. SOTA would compile `next` into some control `C'` and clobber the running `C` with `C'`. We wish to achieve the same result while being less disruptive to unaffected parts of the running control.

A change to the live control is _atomic_ when its effect falls between two observable operations: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if the change fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by the pre-change control and `op_{N+1}, ...` are served entirely by the post-change control. No operation straddles the change, and no operation sees an intermediate state. §1's running example described this informally.

We fix a small grammar of atomic edits. Each production is a single primitive that acts on a live control `C = (s, q, z)` and produces a new control. The grammar discussed in this section is the alphabet using which the transition planner (§4) assembles a sequence whose operational composition takes `C` to a control realizing `next` _without clobbering_. Most productions are `pol`-visible: their effect shows up in `next`, and a comparison of `prev` against `next` is enough to understand them. Others are transaction-only: their effect lives entirely in `z`, leaving the `pol`-level skeleton untouched.

An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. We write `t@path` for the subtree of `t` reached by following `path` down from `t`'s root. Depending on the production being used, `t` is instantiated to `prev` or `next`; see below.

```
diff   ::= Add          (path, pol, weight?)
         | Quiesce      (path)
         | Designate    (path, pol)
         | Remove       (path)
         | ChangeWeight (path, weight)
         | Graft        (ctx)

path   ::= []  |  i :: path                       // i is a child index
ctx    ::= □                                      // the unique hole; takes no children
         | D_n(pol, ..., ctx, ..., pol)           // n children total; exactly one is itself a context
weight ::= a positive real
```

`pol` is the nonterminal of §3.2. A _policy context_, written `ctx`, is built like a `pol`, except that exactly one of its slots is the distinguished _hole_ `□` rather than a subtree. The hole is a reserved slot, not an absence of a slot: the parent of the hole has an arity that includes the hole, e.g., `RoundRobin_3(A, B, □)` is distinct from `RoundRobin_2(A, B)`; both are valid. We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`. A `ctx` is _valid_ iff `ctx[s]` is a valid `pol` for some valid `s`. The plug is total, and for any valid `ctx` and any valid `s`, `ctx[s]` is a valid `pol` whenever their leaf labels are disjoint.

The grammar is shaped by what we can realize atomically in hardware (§6): each production is exactly an edit for which we have a substrate-level commit. That commit slips in between two consecutive `push`/`pop` operations.

Edits that would have to destroy structure still holding packets are _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§5) only after ensuring that the subtree being removed is empty. The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _sequences_ of these atomic edits; §4 makes the sequencing precise.

Notes on the individual edits.

- `Add(path, pol, weight?)` splices `pol` in as the new slot `next@path`. The production carries a `weight` exactly when the slot it edits hangs off a WFQ parent.
- `Quiesce(path)` prevents `prev@path` from receiving new traffic: incoming traffic that used to classify into its leaves is dropped (via the partial-`z` extension of §3.1.2), and the subtree drains under ordinary service of its already-buffered packets. Topology, disciplines, and labels are unchanged; the edit's whole effect is in `z`, so it is `pol`-invisible.
- `Designate(path, pol)` converts `prev@path` into `Strict(prev@path, pol)` in place. The existing subtree `prev@path` becomes the high-priority sibling (set to drain), and `pol` becomes the low-priority _designated survivor_. Operationally, `z` is rewired so that traffic that used to classify into `prev@path` is now offered to the survivor instead: the survivor's own classifier decides what to do with each packet, accepting any it has a leaf for and dropping the rest via the partial-`z` extension of §3.1.2. We make no assumption about how much overlap there is between `prev@path`'s flows and the survivor's. If the survivor has a leaf whose label coincides with one under `prev@path`, the differ names that survivor leaf with a fresh primed label (`A'` is read as "packets that would have gone to `A` before the edit") so the post-edit `pol` has, by construction, disjoint leaf labels and remains valid. The production does not need to carry a weight even when the parent at `path`'s prefix runs WFQ, since the new `Strict` node inherits `prev@path`'s slot and weight. Literally inserting this `Strict_2` node in the middle of the running tree would be expensive, as it would require relocation the entire subtree `prev@path` one PE deeper (because siblings and cousins must share a PE, see §2.1). §6 features a new in-place hardware gadget that gives us the `Strict` semantics described here without incurring that relocation cost.
- `Remove(path)` structurally removes `prev@path`, dropping its slot and renumbering any higher siblings. The subtree must be empty; §3.4.2 discusses why.
- `ChangeWeight(path, weight)` overwrites the weight that `prev@path`'s parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft(ctx)` produces `ctx[prev]`: the policy context `ctx` is spawned around `prev`, with `prev` plugged into the context's sole hole. `Graft` carries no `path`: if the user wants localized graft-style edits, deeper in the tree, they must be realized as a sequence (§4), not by a path-bearing `Graft`.

When `prev = next` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.4 All Productions of `diff` are Sound

A `diff` (§3.3) is a syntactic object: we write `δ` for a generic syntactic diff. Before assigning `δ` any semantics, we cut down to the policies against which the syntax actually makes sense.

We say `δ` is _compatible with_ a live control `C` (where `prev = ⌊C⌋`) when:

- every `path` is consistent with `prev`'s shape: resolving to an existing slot for `Quiesce` / `Designate` / `Remove` / `ChangeWeight`, and to a slot of an existing internal node for `Add`;
- any `weight?` is present exactly when the edited slot's parent runs WFQ;
- the parent at `path`'s prefix runs WFQ when `δ = ChangeWeight(...)`;
- any new leaf labels introduced by an `Add`'s `pol` or a `Graft`'s `ctx` are disjoint from `prev`'s surviving labels.

`Add` carries one further side condition that the bullets above cannot express, because it is semantic rather than syntactic: the classifier predicates routing into `pol`'s leaves must be disjoint from the domain of `C`'s live `z`. This is what lets us require, informally, that an `Add` introduces "fresh traffic that intersects nothing `prev` was already serving" (§3.3), and what lets the operational transition of §3.4.1 extend `z`'s domain without colliding with `C`'s routes.

The rest of §3.4 assumes `δ` is compatible with `C`. §4's transition planner discharges this assumption: it only ever emits a `δ` that is compatible with the live `C` at the moment of emission.

Each `δ` admits two semantic readings, and the rest of the paper leans on both. The operational reading is primary; the pol-level reading is a projection of it that is total but lossy.

- The _operational transition_, `[[δ]] : control -> control`, is the live rewrite. It acts on the full control tuple `(s, q, z)`: state, tree, and transaction together. This is the reading the substrate actually runs, the meaning carried by the syntactic edits the planner emits, and the object every soundness obligation in this section is stated against.
- The _pol-level denotation_, `den(δ) : pol -> pol`, is the projection of `[[δ]]` through the `⌊·⌋` bridge of §3.2. It captures the pol-visible part of the edit. For most primitives (`Add`, `Remove`, `ChangeWeight`, `Graft`, and `Designate`'s wrap-and-rename), `den(δ)` is non-trivial and matches what a reader would extract from comparing `prev` against `next`. `Quiesce`, whose whole effect is in `z`, has `den(Quiesce) = id_pol`. The z-rewiring inside `Designate` lives only in `[[Designate]]`, not in `den(Designate)`. A `pol` carries only topology, disciplines, weights, and leaf labels (no live contents and no transaction), so `den(δ)` is a purely static map, and effects that live entirely in `z` are invisible to it. §4's planner works at this level: it searches for a sequence of edits such that composing their `den`otations together carries `prev` to `next`.

The two readings are linked through `⌊·⌋`. Concretely, the following three mean the same thing:

- `⌊[[δ]](C)⌋`
- `den(δ)(⌊C⌋)`
- "If control `C` is running and edit `δ` is requested, then what `pol` does the post-edit control realize?"

In the language of `[[δ]]`, _atomicity_ can be redefined crisply: `[[δ]]` replaces the live control `C` with `[[δ]](C)` between two consecutive `push`/`pop` operations. Modeling `[[δ]]` as a single function `control -> control` builds the indivisibility into the abstraction; the obligations below ensure that the result of this single step is correct.

For each production we discharge three obligations.

- _Realization_ constrains the `pol`-visible skeleton: `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)`. The `pol` summary of the post-edit control must equal the pol-level denotation applied to the `pol` summary of the pre-edit control. For our running example, `δ = Add([2], spotify)`, so `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and realization says `⌊[[δ]](C)⌋ = Strict(gmail, zoom, spotify)` for any `C` with `⌊C⌋ = Strict(gmail, zoom)`. The equation pins down the new topology, disciplines, weights, and labels. Transaction-only effects (`Quiesce`'s shrinking of `z`'s domain, `Designate`'s rerouting) lie outside `⌊·⌋` and are pinned down by the per-production operational rule below, not by realization. Realization says nothing about contents.
- _Soundness_ constrains the live contents: `|- C` must imply `|- [[δ]](C)`. Well-formedness (§3.1) is a property of `q`'s contents alone, so this is the obligation that the packets and index entries that `[[δ]]` drops or adds must still satisfy `|-`.
- _State preservation_ constrains the live bookkeeping `s`. At every position structurally shared between `prev` and `next` and outside the production's local edit site, `s'(p) = s(p)`. At the edit site and at every position inside a freshly-spawned subtree, `s'` is exactly what the production's `init`-rule prescribes, where the `init`-rule is the per-discipline `init_node_D` / `init_slot_D` of §3.2 invoked as specified per-production below.

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

We write `c` for the parent node of the new slot (here the root `Strict`), `path_c` for the path to `c` from the root, and `k` for the new slot's index, so `path = path_c ++ [k]`. The transition function `[[Add(path, pol, weight?)]]` rewrites the live control `(s, q, z)` componentwise into `(s', q', z')`:

- _The state, `s -> s'`._ Let `D` be `c`'s discipline. With `s` structured as in §3.1, the update splits by position into three cases:
  - _At every position outside the new subtree, other than `path_c` itself:_ `s'(p) = s(p)`.
  - _At `path_c`:_ the parent's `node_state` is unchanged, while its `slot_state` vector splices in a fresh entry at index `k`,
    ```
    s'(path_c).node_state = s(path_c).node_state
    s'(path_c).slot_state = s(path_c).slot_state[ + init_slot_D(s(path_c).node_state, weight?) / k ]
    ```
    (a plain append when `k` is last). The fresh entry is whatever per-arm bookkeeping `D` decided a freshly spliced arm receives, via the `init_slot_D` of §3.2.
  - _At every position inside the new subtree:_ for every position `pos` in `pol`'s topology, `s'(path_c ++ [k] ++ pos) = s_pol(pos)`, where `(s_pol, q_pol, z_pol)` is the control compiled from `pol`.

  No existing slot's state is disturbed.

- _The tree, `q -> q'`._ At `c = Internal(qs, p)` we splice the new subtree in at index `k`: `q' = Internal(qs[+a/k], p[+/k])`, where `a` is the empty PIFO tree compiled from the inserted `pol` (the topology of `pol` with an empty PIFO at every node). Splicing at `k` shifts the old children at indices `>= k` one place to the right, and renumbering `p` to `p[+/k]` tracks exactly that shift: every entry naming an old index `>= k` is bumped up by one, so each old slot keeps its matched count of occurrences and packets, now under a new name. When `k` is the last index (the append case) nothing is `>= k`, so `qs[+a/k] = qs ++ [a]` and `p[+/k] = p`: no child shifts and the parent's index-PIFO is untouched.
- _The transaction, `z -> z'`._ `z'` extends `z` in two ways: it admits the new subtree's flows on a fresh domain, and it renumbers paths at `c` to track the index shift. On the existing domain of `z`, a packet gets the same path that `z` would have emitted, with its step at `c` bumped by one whenever that step named an old index `>= k` (the path-level image of `p[+/k]`). On the freshly admitted domain (packets that classify into the new subtree, formerly undefined for `z`), `z'` emits a path whose step at `c` selects `k` and then descends through the new tree. No `prev` packet was ever routed to `k`: under `next`, `k` names the new subtree, and the old occupant of `k`, if any, now lives at `k+1`.

##### Soundness

We discharge the three obligations of §3.4 in turn, writing `C_next = [[Add(path, pol, weight?)]](C_prev)`.

- _Realization._ The tree step above sets `q'` to `Internal(qs[+a/k], p[+/k])`, which is `prev`'s tree with an empty `pol`-shaped slot spliced in at `k`, and `z'` is compiled from `next`, so `⌊C_next⌋ = den(Add(path, pol, weight?))(⌊C_prev⌋)`: the new shape and transaction are together exactly what the edit denotes.
- _Soundness._ `|- C_prev` gives `|- C_next`. The parent `c` has exactly zero occurrences of `k`: its index-PIFO is now `p[+/k]`, which by construction names no `k` (old entries `< k` stay put, old entries `>= k` were bumped to `>= k+1`). The new subtree `a` holds zero packets or indices, so the well-formedness obligation at slot `k` reads `0 = 0`. Every other slot is a child it was in `q`, with the same packets beneath it; its entries in `p[+/k]` are the old entries under a possibly-shifted name, so its matched count is inherited verbatim. Nothing needs repair.
- _State preservation._ At every position outside the new subtree and other than `path_c`, `s'(pos) = s(pos)` by the first case of the state update; this discharges the "structurally shared and outside the edit site" clause of §3.4. At `path_c` the `node_state` is unchanged and the `slot_state` vector splices in `init_slot_D(s(path_c).node_state, weight?)` at index `k`; this is exactly the `init_slot_D` invocation §3.2 prescribes for splicing a fresh arm under a running `D`-parent, matching the "at the edit site" clause. Inside the new subtree, `s'` is the freshly-compiled `s_pol`, which is just `init_node_D` and `init_slot_D` applied node-by-node through `pol` per §3.2's compilation rule, matching the "inside a freshly-spawned subtree" clause.

##### Notes

_Atomicity._ No in-flight packet straddles the diff: every packet resident at the diff instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`. The renumbering `p[+/k]` relabels index _values_ only; it moves no packets and changes no ranks. So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have, the empty new slot contributing nothing and the existing subtrees keeping their contents and relative priority under shifted index names. The first `push` that `z'` routes to `k` is the first packet ever to occupy `a`.

_Deeper paths._ The running example edits the root, but `path` may be any prefix; `Add([1, 2], pol)` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes that `q` and `q'` share verbatim, and, because the new subtree is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor PIFO is rewritten, and the edit is confined to `c` and the fresh subtree below it.

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

## 5. Identifying Better Transitions

§3 gave us a toolkit of atomic diffs and §4 sequenced them through `link`s into full reconfigurations. With those tools in hand, this section asks how the transition planner can wield them well. The metric we adopt is _confinement_: a good sequence is one whose diffs and intervening `link`s disturb as little of the running scheduler as possible, leaving the parts of the tree that did not need to change running undisturbed.

There is always a maximally unconfined fallback. To reach any `next` from any `prev`, the planner can issue `Designate([], next)`, making the whole of `next` the survivor of the whole of `prev`. All new traffic flows to `next` at once, every `pop` is served by `prev` until `prev` drains, and a closing `Remove` throws away `prev` and leaves `next`. Nothing is dropped, the sequence is safe by §3.4, and no part of the scheduler is left running undisturbed. This is the floor. The rest of §5 is the story of doing better: localizing the change so that the sequence and its `link`s touch only a small subtree while the rest of the scheduler keeps running.

We make no claim that the planner is canonical or minimal. We claim only that whatever sequence it emits is safe (§3.4) and no worse than this fallback.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself. TK.]

## 6. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- `Designate` (§3.3) realizes the `Strict_2` wrap as an in-place gadget: a _super-node_ `{A -> B}` that occupies `A`'s slot directly, exposes a pop order strictly favoring `A` over `B`, and adds no PE depth. Detail the gadget's representation, its commit sequence, and the `{A -> B} -> B` collapse that `Remove(path)` triggers once `A` is empty.
- [AM: question for Zhiyuan: §3.5 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.4.1 diff. But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 7. Evaluation

## 8. Related Work

## 9. Conclusion
