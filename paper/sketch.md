# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling. Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem. Running example: a small-office gateway runs `Strict(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic. The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `Strict(gmail, zoom, spotify)`: just extend the strict-priority list with `spotify` having lowest priority.
  2. `Strict(gmail, RoundRobin(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, SOTA would stop the world, drop/recirculate buffered packets, recompile, and reinstall. Costs: dropped or recirculated (read, delayed!) packets, downtime, and a full respawn of nodes that did not need respawning.

- The alternate is to reprogram a scheduler without stopping the world. Let's revisit the examples from earlier.
- Transitioning to `Strict(gmail, zoom, spotify)` is actually quite easy. We can achieve the following gold standard:
  - time1: `Strict(gmail, zoom)` is running
  - time2: the request to move to `Strict(gmail, zoom, spotify)` is received. `Strict(gmail, zoom)` is still running
  - time3: we move to `Strict(gmail, zoom, spotify)`. Whatever user-observable interaction (push/pop) happened immediately before time3 happened entirely in the `Strict(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened entirely in the `Strict(gmail, zoom, spotify)` regime. We therefore refer to the transition at time3 as _atomic_.
  - [AM note: time2 = time3 in this case? I don't want to make that change here since it would conflate atomicity with immediacy, but do we think someone will scratch their head and ask why any time is needed at all?]
- What about transitioning to `Strict(gmail, RoundRobin(zoom, spotify))`? It is not as easy. We atomically step into a _transitionary period_ during which the scheduler still accepts and emits packets, and once certain well-defined conditions are met, we atomically step into the user-requested policy.
- It is crucial to note that, although the user never described to us the semantics of the transitionary period, it _is_ in fact a _de facto_ packet scheduling regime with some semantics! It is useful to recognize it as a scheduling policy in its own right (we give it the name `link`). There are clearly better and worse transitions from a network operator's point of view. SOTA has more-or-less unintentionally adopted a trivial stop-the-world `link`. Our contributions include both being clear about what `link` is and improving on it.
- Concretely:
  - Obligation 1. Show that `link` is just scheduling. The transition period is motivated by hardware-level manipulations on the way to realizing `next` and is not designed for a clean human-readable semantics, so the natural fear is that we owe the reader a new formalism for it. Our result is the opposite: each `link` is an ordinary scheduling control in the sense of §3.1, no new semantics is required.
  - Obligation 2. Improve upon SOTA. We will give the semantics of SOTA's stop-the-world `link` and use it as a baseline. We have a (set of) practical goals that may guide us as we search for the ideal way to transition from some `prev` to some `next`. Can we minimize the length of the transition period? Can we avoid dropped/delayed packets? [AM: more to come here; the cost model is a legit open question!]. We will show that it is always possible to make a transition from any `prev` to any `next`, but it is occasionally possible to make a very efficient transition. We contribute a tool that achieves this and shows [improvements].

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

We recap the PIFO tree model (§3.1), fix a grammar of atomic policy diffs (§3.2), show that each production of the grammar carries a well-formed scheduler to a well-formed scheduler (§3.3), and argue that this guarantee survives down to hardware (§3.4). Composing diffs into the sequences that realize a full reconfiguration, and the `link` schedulers that arise between them, is deferred to §4.

### 3.1 Background: PIFO trees, formally

We build on the PIFO tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3]. §3.1.1 recaps the pieces of _FA_'s formalism that this paper actually leans on; §3.1.2 lays out the small extensions our diff grammar requires.

#### 3.1.1 From _Formal Abstractions_

##### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.2 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

##### The two observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ observable interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of `push`/`pop` observability: every `push`/`pop` happens against a well-defined scheduler, never a half-edited intermediate scheduler.

##### Well-formedness

A PIFO tree `q` is well-formed (`|- q`, following _Formal Abstractions_' notation) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. _FA_ shows that `push` always preserves `|- q`, and that `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

##### Control

_FA_ introduces a _control_ `(s, q, z)`: a current state `s` drawn from some fixed set `St`, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z : St × Pkt -> Path(t) × St` that, given a state and a packet, returns a path of the correct shape together with an updated state. That path pins down both _where_ a packet goes (via its index sequence) and _with what priority_ it competes for service at every level (via its rank decorations), so `z` is the sole locus of routing and ranking.

#### 3.1.2 Our extensions

_FA_ describes how a control _runs_, but the diff grammar of §3.2 needs four things _FA_ does not provide: a way to _drop_ packets at `z`, a syntactic source from which a control is _built_ (and against which two controls can be _compared_), an explicit structure on the state `s` so that diff rules can write equations on it, and a small per-discipline hook for splicing fresh arms into a running parent. We fix these here, then close with the bridge `⌊·⌋` that links syntactic source and live control.

##### Dropping packets: `z` made partial

We extend _FA_'s `z` from total to partial: `z : St × Pkt ⇀ Path(t) × St`, where an undefined entry means the packet is dropped (no path, no state update, `q` untouched). This means that our `z` is also in charge of _packet admission_. _FA_'s total typing is the special case where the domain is all of `St × Pkt`, and every _FA_ result we lean on carries through verbatim because a dropped packet never reaches `q`.

##### Policies: a syntactic source for controls

_FA_ never says how to _build_ a control. There is no "constructor" that takes an operator's wish (e.g., `Strict(gmail, zoom)`) and emits a control with the appropriate state variables, a PIFO tree with the right topology and empty queues, and a scheduling transaction that actually implements strict prioritization via the paths that it emits. Nor, given two controls, does _FA_ offer any way to compare them: `z` is just a function, and one cannot pattern-match on a function. These are the same gap. So before we can either construct the controls _FA_ reasons about or diff one against another, as the transition planner of §4 must, we make the syntactic source explicit.

```
pol    ::= flow                                   // leaf, labeled by a flow of traffic
         | D_n(pol_1, ..., pol_n)                 // internal node, n-ary discipline D
```

This yields a tree-shaped structure with unbounded arity. `D` ranges over scheduling disciplines (`Strict`, `RoundRobin`, `WFQ`, ...). The subscript `n` is the arity, which we drop when it is clear from the children: `Strict(gmail, zoom)` for `Strict_2(gmail, zoom)`. WFQ pairs each child with a weight, elided from the grammar above. A `pol` is _valid_ when every discipline is applied at a proper arity, a child carries a weight exactly when its parent runs `WFQ`, and leaf labels are distinct. Validity is a purely syntactic condition on the source, not to be confused with the invariant `|- q`.

##### Compilation: `pol` to control

A `pol` gives us a control `(s, q, z)` straightforwardly.

- Erase the labels, and what remains is the topology. Instantiate that topology with an empty PIFO at every node, and you have the tree `q`.
- Read the disciplines of the policy, and they mechanically show how to:
  - Generate a short program per node that determines how to rank incoming packets at that node. These node-local choices glue together into the single `z`.
  - Seed each node's local bookkeeping with initial state variables, e.g. a `RoundRobin` cursor at the first child, a `WFQ` virtual-finish accumulator at zero, and so on. This gives the state `s`.

##### Local state, made explicit

We commit to slightly more structure on `s` than _FA_ does. We treat `s` as a partial map from positions in the topology to local state, `s : path ⇀ LocalState`, where a position is a (possibly empty) sequence of child indices addressing a node from the root (the same `path` syntax §3.2 uses for the diff grammar), and `s(p)` is defined exactly when `p` reaches a node of `t`. A `LocalState` is a pair `(node_state, slot_state list)`: a `node_state` for the discipline's per-node bookkeeping (an `RR` cursor, a `WFQ` global virtual time), and a list of `slot_state` entries (one per arm, in slot order) for per-arm bookkeeping (a `WFQ` per-arm virtual finish, a `WRR` per-arm credit, the arm's weight under `WFQ`). Disciplines without per-arm bookkeeping (`Strict`, pure `RR`) have an empty `slot_state` list. _FA_ leaves all of this abstract; making it explicit lets the diff rules of §3.3 write equations on `s` instead of prose.

##### Slot-seeding: `init_D`

Each discipline `D` also fixes a _slot-seed function_, `init_D : node_state × pol × weight? -> slot_state`, that decides what per-arm bookkeeping a fresh arm receives when it is spliced into a running `D`-parent. This becomes load-bearing in §3.3.1, when `Add` splices a new arm under an existing `D`-parent: at that moment, `init_D` reads the parent's current `node_state` and the new arm's `pol` and weight and returns the new `slot_state` entry. Choosing `init_D` is a scheduling decision, not a structural one, since the choice changes how the new arm competes with the established arms. We commit throughout to a _join-the-current-round_ reading. `init_Strict` and `init_RR` return the empty tuple (no per-arm bookkeeping to seed). For `WFQ`, the `node_state` at a parent is the scalar virtual time `vt`, and we set `init_WFQ(vt, _, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag. By WFQ's standard finish-time recurrence the first packet on this arm posts a tag of `max(virtual_clock, vt) + 1/w`, which slots it into the round the established arms are currently in.

##### The bridge: `⌊·⌋`

We write `⌊C⌋` for the `pol` from which control `C` was compiled. We say that `C` _realizes_ `⌊C⌋`. This `pol`/`control` split is what structures §3.3: a syntactic diff (§3.2) admits two semantic readings, a `pol`-level denotation `den(δ) : pol -> pol` that fixes the static skeleton it produces and an operational transition `[[δ]] : control -> control` that does the live rewrite, and `⌊·⌋` is the bridge that lets us state when the two agree.

### 3.2 A Grammar for Tree Diffs

We fix a small grammar of structural edits between the valid `pol` `prev` and the valid `pol` `next`. An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. We write `t@path` for the subtree of `t` reached by following `path` down from `t`'s root. Depending on the production being used, `t` is instantiated to `prev` or `next`; see below.

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

`pol` is the nonterminal of §3.1. A _policy context_, written `ctx`, is built like a `pol`, except that exactly one of its slots is the distinguished _hole_ `□` rather than a subtree. The hole is a reserved slot, not an absence of a slot: the parent of the hole has an arity that includes the hole, e.g `RoundRobin_3(A, B, □)` is distinct from `RoundRobin_2(A, B)`; both are valid. We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`. The plug is total, and `ctx[s]` is a valid `pol` whenever `s` and `ctx` are individually valid and their leaf labels are disjoint.

Every edit in this grammar is _atomic_: it carries a well-formed scheduler to a well-formed scheduler in a single step. The grammar is shaped by what we can realize atomically in hardware (§6): each production is exactly an edit for which we have a substrate-level commit. Edits that would have to destroy structure still holding packets are _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§5) only after ensuring that the subtree being removed is empty. The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _sequences_ of these atomic edits; §4 makes the sequencing precise.

Notes on the individual edits:

- `Add` carries a `weight` exactly when the slot it edits hangs off a WFQ parent, which needs a weight to schedule the subtree; for any other parent the weight is absent. It adds the subtree `pol` at the new slot `next@path`, with empty queues throughout. The new subtree carries fresh traffic that intersects nothing `prev` was already serving: packets that classify into `pol` lay outside the domain of `prev`'s `z` and were being dropped, and `Add` extends the domain so they route into the new subtree.
- `Quiesce (path)` shrinks the domain of `z` to exclude packets that would have classified into `prev@path`: those packets are dropped, no new traffic enters the subtree, and the quiesced subtree keeps being served pops until it eventually drains to empty. It is a transaction-only edit: topology and contents are untouched, and it adds no structure and removes none. In the framing of §3.3, `den(Quiesce) = id_pol` while `[[Quiesce]]` rewrites the transaction `z`. [AM note: this last sentence is misplaced here, but I'm leaving it here for a sec since we want to discuss it. Eventually it belongs in a sub-sub-section in 3.3.]
- `Designate (path, pol)` semantically replaces `prev@path` with `Strict(prev@path, pol)`. That is, the subtree at `prev@path` gets high priority and the new `pol` is spawned as its low-priority sibling. Standard strict-priority semantics deliver exactly the property we want: `prev@path` must drain completely before any pop returns a packet from `pol`. We call `pol` the _designated survivor_ of `prev@path` because, if `prev@path` drains and then receives no new pushes, only `pol` remains under the parent's slot. The hardware does not literally insert a Strict node here, since that would push all of `prev@path` one PE level deeper and force a re-mapping of an otherwise-untouched subtree; §6 describes the in-place gadget that realizes the same semantics without changing PE depth.
- `Remove (path)` structurally removes `prev@path`, dropping its slot and renumbering any higher siblings. The subtree must be _empty_: removing an occupied subtree would leave its parent with dangling indices.
- `ChangeWeight (path, weight)` targets `prev@path`; it overwrites the weight that `prev@path`'s parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft (ctx)` spawns `ctx` and plugs its hole with `prev`, which keeps its in-flight contents. The ancestor nodes of `prev` in `ctx[prev]` are populated with the indices required to make `ctx[prev]` well-formed. The rest of the tree is spawned empty. `Graft` carries no `path`: it always wraps the whole of `prev` at the root, sliding `prev` into `ctx`'s hole. Localized graft-style edits, deeper in the tree, are realized as a sequence (§4), not by a path-bearing `Graft`.

When `prev = next` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.3 All Productions of `diff` are Sound

A `diff` (§3.2) is a syntactic object: we write `δ` for a generic syntactic diff. Before assigning `δ` any semantics, we cut down to the policies against which the syntax actually makes sense.

We say `δ` is _well-formed against_ `prev` when every `path` is consistent with `prev`'s shape (resolving to an existing slot for `Quiesce`/`Designate`/`Remove`/`ChangeWeight`, and to a fresh slot of an existing internal node for `Add`); any `weight?` is present exactly when the edited slot's parent runs WFQ; the parent at `path`'s prefix runs WFQ when `δ = ChangeWeight(...)`; any new leaf labels introduced by an `Add`'s `pol` or a `Graft`'s `ctx` are disjoint from `prev`'s surviving labels; and, for an `Add`, the classifier domain of `pol`'s compiled `z` is disjoint from the domain of `prev`'s `z`. The last condition is a semantic side condition on the compiled `z`, not enforceable by labels alone: it is what licenses the prose-level claim that an `Add` introduces "fresh traffic that intersects nothing `prev` was already serving" (§3.2), and it is what lets the operational transition of §3.3.1 extend `z`'s domain without colliding with `prev`'s routes. This is just the natural lifting of `pol`-validity (§3.1) from a single tree to an edit between two. Throughout §3.3, the two semantic readings of `δ` developed below are stated on `(δ, prev)` pairs that satisfy this condition; §4's transition planner emits only such pairs by construction.

Each `δ` admits two semantic readings, and the rest of the paper leans on both.

- The _pol-level denotation_, `den(δ) : pol -> pol`, is defined by recursion on `path` and acts on the policies against which `δ` is well-formed. It says what `next` shape `δ` produces from a given such `prev`, and nothing more. A `pol` carries only topology, disciplines, and labels (no live contents and no state), so `den(δ)` is a purely static map. This is the reading §4's transition planner manipulates: the planner only ever sees static policies, and the syntactic diff it emits between `prev` and `next` is correct exactly when `den(δ)(prev) = next`.
- The _operational transition_, `[[δ]] : control -> control`, is the live rewrite. It acts on the full control tuple `(s, q, z)`: state, tree, and transaction together. This is the reading the substrate actually runs, and it is the object every soundness obligation in this section is stated against.

The two readings are linked through the `⌊·⌋` bridge of §3.1, which strips a live control down to its source `pol`. So `⌊[[δ]](C)⌋` and `den(δ)(⌊C⌋)` are two ways of asking the same question (_what `pol` does the post-edit control realize?_), and they had better agree.

An _atomic diff_ is one whose `[[δ]]` replaces the live control `C` with `[[δ]](C)` in the gap between two `push`/`pop` operations. Concretely: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if `δ` fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by `C` and `op_{N+1}, ...` entirely by `[[δ]](C)`. No operation straddles the edit, and no operation sees an intermediate state. This is the property §1's running example flagged informally; the rest of this section makes it precise for each production of §3.2.

For each production we discharge three obligations, one per component of the control.

- _Realization_ constrains the static skeleton: `⌊[[δ]](C)⌋ = den(δ)(⌊C⌋)`. The `pol` of the post-edit control must equal the pol-level denotation applied to the `pol` of the pre-edit control. For our running example, `δ = Add([2], spotify)`, so `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and realization says `⌊[[δ]](C)⌋ = Strict(gmail, zoom, spotify)` for any `C` with `⌊C⌋ = Strict(gmail, zoom)`. Because a `pol` fixes both the topology of `q` and the transaction `z`, this one equation constrains the new tree's _shape_ and the new transaction together, and says nothing about contents.
- _Soundness_ constrains the live contents: `|- C` must imply `|- [[δ]](C)`. Well-formedness (§3.1) is a property of `q`'s contents alone, so this is the obligation that the packets and index entries `[[δ]]` leaves behind or introduces still satisfy `|-`.
- _State preservation_ constrains the live bookkeeping `s`. At every position structurally shared between `prev` and `next` and outside the production's local edit site, `s'(p) = s(p)`. At the edit site and at every position inside a freshly-spawned subtree, `s'` is exactly what the production's `init`-rule (the per-discipline `init_D` of §3.1, threaded as written out in each §3.3.x) prescribes.

The denotational rules frequently read, overwrite, and splice child lists. Let us fix some notation. We write `ts[i]` for the `i`-th child and `ts[t/i]` for `ts` with its `i`-th child overwritten by `t`; this leaves the arity unchanged. The two arity-changing edits carry a sign: `ts[+t/i]` is `ts` with `t` spliced in as the new `i`-th child (the old `i`-th and later children shift one place to the right), and `ts[-/i]` is `ts` with its `i`-th child dropped (later children shift left). Indices follow the `path` convention of §3.2.

The operational diffs of §3.3 manipulate two parallel structures at an internal node: a child list `qs` of PIFO tree children, to which the list-manipulation notation introduced above applies verbatim, and an index-PIFO `p` recording child indices. For `p` the same `[±/k]` notation _renumbers_ rather than splices, because we often need to open up or close a slot by shifting indices. `p[+/k]` is `p` with every entry `>= k` bumped up by one (opening up slot `k`); `p[-/k]` is `p` with every entry `> k` brought down by one. Entries below the edit point are left alone.

#### 3.3.1. `Add(path, pol, weight?)`

##### Pol-level denotation

`den(Add(path, s))` is the structural map, defined by recursion on `path`:

```
den(Add(i :: [],   s)) (P ts) = P ( ts[+s / i] )
den(Add(i :: rest, s)) (P ts) = P ( ts[ den(Add(rest, s)) (ts[i]) / i ] )
```

The base case fires once `path` reaches the new slot's parent: the subtree `s` is spliced in as the new `i`-th child. (Recall from §3.2 that `Add`'s `path` is read in `next` and names the new slot, so its final index `i` is the insertion point.) The recursive case walks down a shared ancestor, recurses into child `i`, and writes the result back. A WFQ parent also needs the new slot's weight; the syntactic diff is `Add(path, s, w)` in this case. With children zipped as `(weight, subtree)` pairs `cs`, the weight enters only at the insertion site, and descent leaves the weights untouched:

```
den(Add(i :: [],   s, w)) (WFQ cs) = WFQ ( cs[ +(w, s) / i ] )
den(Add(i :: rest, s, w)) (WFQ cs) = WFQ ( cs[ (w_i, den(Add(rest, s, w)) s_i) / i ] )
                                                                     where (w_i, s_i) = cs[i]
```

Our running example denotes `den(Add([2], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 2] ) = Strict(gmail, zoom, spotify) = next`, as intended. In our example we appended a new arm, but a mid-tree edit would be no more complicated: `den(Add([1], spotify)) Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 1] ) = Strict(gmail, spotify, zoom)`, with the old `zoom` sliding from index `1` to index `2`.

##### Operational transition

We write `c` for the parent node of the new slot (here the root `Strict`), `path_c` for the path to `c` from the root, and `k` for the new slot's index, so `path = path_c ++ [k]`. The transition function `[[Add(path, pol, weight?)]]` rewrites the live control `(s, q, z)` componentwise into `(s', q', z')`:

- _The state, `s -> s'`._ Let `D` be `c`'s discipline. With `s` structured as in §3.1, the update splits by position into three cases:
  - _At every position outside the new subtree, other than `path_c` itself:_ `s'(p) = s(p)`.
  - _At `path_c`:_ the parent's `node_state` is unchanged, while its `slot_state` vector splices in a fresh entry at index `k`,
    ```
    s'(path_c).node_state = s(path_c).node_state
    s'(path_c).slot_state = s(path_c).slot_state[ + init_D(s(path_c).node_state, pol, weight?) / k ]
    ```
    (a plain append when `k` is last). The fresh entry is whatever per-arm bookkeeping `D` decided a freshly spliced arm receives, via the `init_D` of §3.1.
  - _At every position inside the new subtree:_ for every position `q` in `pol`'s topology, `s'(path_c ++ [k] ++ q) = s_pol(q)`, where `(s_pol, q_pol, z_pol)` is the control compiled from `pol`.

  No existing slot's state is disturbed.

- _The tree, `q -> q'`._ At `c = Internal(qs, p)` we splice the new subtree in at index `k`: `q' = Internal(qs[+a/k], p[+/k])`, where `a` is the empty PIFO tree compiled from the inserted `pol` (the topology of `pol` with an empty PIFO at every node). Splicing at `k` shifts the old children at indices `>= k` one place to the right, and renumbering `p` to `p[+/k]` tracks exactly that shift: every entry naming an old index `>= k` is bumped up by one, so each old slot keeps its matched count of occurrences and packets, now under a new name. When `k` is the last index (the append case) nothing is `>= k`, so `qs[+a/k] = qs ++ [a]` and `p[+/k] = p`: no child shifts and the parent's index-PIFO is untouched.
- _The transaction, `z -> z'`._ `z'` is the scheduling transaction compiled from `next`. It both _extends_ `z`'s domain and _renumbers_ paths at `c`. On the existing domain of `z`, a packet gets the same path that `z` would have emitted, with its step at `c` bumped by one whenever that step named an old index `>= k` (the path-level image of `p[+/k]`). On the freshly admitted domain (packets that classify into the new subtree, formerly undefined for `z`), `z'` emits a path whose step at `c` selects `k` and then descends through the new tree. No `prev` packet was ever routed to `k`: under `next`, `k` names the new subtree, and the old occupant of `k`, if any, now lives at `k+1`.

##### Soundness

Recall that we have two obligations.

- _Realization:_ writing `C_next = [[Add(path, pol, weight?)]](C_prev)`, the tree step above sets `q'` to `Internal(qs[+a/k], p[+/k])`, which is `prev`'s tree with an empty `pol`-shaped slot spliced in at `k`, and `z'` is compiled from `next`, so `⌊C_next⌋ = den(Add(path, pol, weight?))(⌊C_prev⌋)`: the new shape and transaction are together exactly what the edit denotes.
- _Soundness:_ `|- C_prev` gives `|- C_next`. The parent `c` has exactly zero occurrences of `k`: its index-PIFO is now `p[+/k]`, which by construction names no `k` (old entries `< k` stay put, old entries `>= k` were bumped to `>= k+1`). The new subtree `a` holds zero packets or indices, so the well-formedness obligation at slot `k` reads `0 = 0`. Every other slot is a child it was in `q`, with the same packets beneath it; its entries in `p[+/k]` are the old entries under a possibly-shifted name, so its matched count is inherited verbatim. Nothing needs repair.

##### Notes

_Atomicity._ No in-flight packet straddles the diff: every packet resident at the diff instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`. The renumbering `p[+/k]` relabels index _values_ only; it moves no packets and changes no ranks. So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have, the empty new slot contributing nothing and the existing subtrees keeping their contents and relative priority under shifted index names. The first `push` that `z'` routes to `k` is the first packet ever to occupy `a`.

_Deeper paths._ The running example edits the root, but `path` may be any prefix; `Add { path = [1, 2]; pol = ... }` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes that `q` and `q'` share verbatim, and, because the new subtree is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor PIFO is rewritten, and the edit is confined to `c` and the fresh subtree below it.

#### 3.3.2. `Remove(path)`

`Remove` is the grammar's one structural deletion. It unhooks the subtree `prev@path` from its parent, drops the vacated slot, and renumbers any higher siblings. It is defined only when `prev@path` is _empty_. Retiring or replacing a subtree that still holds packets is therefore not `Remove`'s job alone: it is realized as a _sequence_ that first drains the subtree and only then removes it (see §4).

##### Discussion

Why do we insist that `Remove` fire only on an empty subtree? Because deleting an _occupied_ child has no canonical local realization. Consider `P(Q(A, B), C)`. In the diagram below each internal node is labeled with its index-PIFO and each leaf with the packets it holds. We draw every PIFO with its most favorably ranked entry, the next one to be popped, on the far right: so the leaf `[a2,a1]` releases `a1` before `a2`.

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

Our grammar sidesteps the question entirely. By draining `B` to empty before removing it (§4), every ancestor's count of entries for `B` is already zero at the instant `Remove` fires, so there is nothing to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

##### Pol-level denotation

##### Operational transition

##### Soundness

### 3.4 Preserving this proof down to hardware

§3.3 proves soundness at the tree-diff level, where each atomic edit (`Add`, `Quiesce`, `Remove`, ...) carries a well-formed tree to a well-formed tree. To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR: `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, and the like. A single IR instruction, unlike a whole tree-diff edit, _can_ leave the tree malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, for example.

We do not prove soundness at the IR level, but instead informally make the case for why the §3.3 proof survives the lowering. There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit. We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed. That commit is precisely how the substrate _realizes_ an atomic diff of §3.3: a diff was defined as an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant. Every `push`/`pop` therefore still lands on a well-formed control (`prev`, a `link`, or `next`), exactly as §3.3 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.3 proved. The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §6.

## 4. Realizing Reconfigurations as Sequences

[AM TK: stub. §3 proved each grammar production a sound atomic diff. This section composes diffs into _sequences_ `δ ; (φ ; δ)*`, separated by _exit conditions_ `φ`, to realize reconfigurations no single diff can express. It formalizes the transitionary scheduler `link` as an ordinary §3.1 control (the "transitionary period is just scheduling" theorem), and it considers _liveness_: whether and when a sequence's exit conditions fire.]

## 5. Identifying Better Transitions

§3 established that our grammar is _safe_: every atomic diff is a sound control replacement (§3.3), so any sequence the planner emits (§4) keeps the live scheduler well-formed at every instant and, once its exit conditions fire, lands in `next`. With safety settled, the question this section takes up is whether the planner wields the grammar _well_.

The transition planner's output is a sequence `diff ; (φ ; diff)*` (§4). What makes one sequence better than another is how _confined_ it is: how little of the running scheduler its diffs and intervening `link`s disturb. [AM TK: liveness now lives in §4; this section is about confinement. Reconcile the framing.]

There is always a fallback. To reach any `next` from any `prev`, the planner issues `Designate([], next)`, making `next` the survivor of the whole of `prev` (§3.2). All new traffic goes to `next`, all `pop`s are served by `prev` until `prev` runs out of packets. Then `Remove` clears `prev` and only `next` is left standing, and pushes and pops are both serviced by `next`. This is our give-up-entirely option, with the splash zone as large as possible and no part of the scheduler left running undisturbed, but it always works and drops nothing. This section is the story of doing better: localize the change, so the sequence and its `link`s touch only a small subtree and the rest of the scheduler keeps running.

We make no claim that the planner is canonical or minimal. We claim only that whatever sequence it emits is safe (§3.3), and is no worse than the worst-case full retirement option.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself. TK.]

## 6. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- `Designate` (§3.2) is described semantically as inserting a `Strict_2` above `prev@path`. A literal in-place Strict-wrap would push `prev@path` one PE level deeper, forcing a re-mapping of the entire subtree below it. We realize `Designate` via an in-place gadget that delivers the same pop ordering without changing PE depth. Write this up here.
- [AM: question for Zhiyuan: §3.4 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.3.1 diff. But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 7. Evaluation

## 8. Related Work

## 9. Conclusion
