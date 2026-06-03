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
- Our two running examples make the gap concrete. The easy append, to get to `Strict(gmail, zoom, spotify)`, looks like the kind of edit vPIFO's substrate is well placed to absorb in place: a targeted append to the structures that encode the tree shape and per-instance storage (the Operation Generation Table and the PIFO Instance Address Table) would plausibly register the new traffic and its operations while leaving the running instances untouched. vPIFO as published does not engage with this question; their only option is a full reinitialization. Driving such a substrate to install the edit in place, straight from the (small) diff, is exactly what our layer adds. The harder restructuring, to `Strict(gmail, RoundRobin(zoom, spotify))`, may not be realizable using vPIFO at all.
- vPIFO's own §9 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly. We are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer _above_ a PIFO substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose.

## 3. A Grammar of Atomic Policy Diffs

We recap the PIFO-tree model (§3.1), fix a grammar of atomic policy diffs (§3.2), show that each production of the grammar carries a well-formed scheduler to a well-formed scheduler (§3.3), and argue that this guarantee survives down to hardware (§3.4). Composing diffs into the sequences that realize a full reconfiguration, and the `link` schedulers that arise between them, is deferred to §4.

### 3.1 Background: PIFO trees, formally

We build on the PIFO-tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3], so we review the pieces of their formalism that this paper actually leans on.

#### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO-tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.2 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

#### The two user-observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ user-visible interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of `push`/`pop` observability: every `push`/`pop` happens against a well-defined scheduler, never a half-edited intermediate scheduler.

#### Well-formedness

A PIFO tree `q` is well-formed (`|- q`, following _Formal Abstractions_' notation) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. _FA_ shows that `push` always preserves `|- q`, and that `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

#### Control

_FA_ introduces a _control_ `(s, q, z)`: a current state `s` drawn from some fixed set, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z` that, given a state and a packet, returns a path of the correct shape together with an updated state.

#### Policies

_FA_ tells us how a control _runs_, but never how to _build_ one. There is no "constructor" that takes a user's wish (e.g., `Strict(gmail, zoom)`) and emits a control with the appropriate state variables, a PIFO tree with the right topology and empty queues, and a scheduling transaction that actually implements strict prioritization via the paths that it emits. Nor, given two controls, does _FA_ offer any way to compare them: `z` is just a function, and one cannot pattern-match on a function. These are the same gap. So before we can either construct the controls _FA_ reasons about or diff one against another, as the transition planner of §4 must, we take a small step back and make that syntactic source explicit.

A `policy` is a labeled tree: every internal node carries a _scheduling discipline_ over its children (`Strict`, `RoundRobin`, `WFQ` with a weight per child, etc.) and every leaf carries a flow label. We write `P(ts)` for a non-leaf `policy` with discipline `P` over a child list `ts` of sub-policies; this is the `Strict(gmail, zoom)` notation we have already been using informally. A `policy` is _valid_ when it is syntactically sensible: every discipline is applied at its proper arity, a child carries a weight exactly when its parent runs `WFQ`, and leaf labels are distinct. This is a purely syntactic condition on the source, not to be confused with the invariant `|- q`.

A `policy` gives us a control `(s, q, z)` straightforwardly.

- Erase the labels, and what remains is the topology. Instantiate that topology with an empty PIFO at every node, and you have the tree `q`.
- Read the disciplines of the policy, and they mechanically show how to:
  - Generate a short program per node that determines how to rank incoming packets at that node. These node-local choices glue together into the single `z`.
  - Seed each node's local bookkeeping with initial state variables, e.g. a `RoundRobin` cursor at the first child, a `WFQ` virtual-finish accumulator at zero, and so on. This gives the state `s`.

We write `⌊C⌋` for the `policy` from which control `C` was compiled. We say that `C` _realizes_ `⌊C⌋`. The grammar of §3.2 and the denotations of §3.3 act on policies, the operational diffs of §3.3 act on controls, and `⌊·⌋` is the bridge that lets us state when a diff realizes its denotation.

### 3.2 A Grammar for Tree Diffs

We fix a small grammar of structural edits between the valid `policy` `prev` and the valid `policy` `next`. An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is. We write `t@path` for the subtree of `t` reached by following `path` down from `t`'s root. Depending on the production being used, `t` is instantiated to `prev` or `next`; see below.

```
diff   ::= Add          (path, tree, weight?)
         | Quiesce      (path)
         | Designate    (path, tree)
         | Remove       (path)
         | ChangeWeight (path, weight)
         | Graft        (ctx)

path   ::= []  |  i :: path                       // i is a child index
tree   ::= flow                                   // leaf, labeled by a flow of traffic
         | D(tree, ..., tree)                     // internal node, discipline D over its children
ctx    ::= □                                      // the unique hole; takes no children
         | D(tree, ..., ctx, ..., tree)           // exactly one child slot is itself a context
weight ::= a positive real
```

`D` ranges over the scheduling disciplines of §3.1 (`Strict`, `RoundRobin`, `WFQ`, ...); WFQ pairs each child with a weight, which is elided from the grammar above. A `tree` is a valid `policy` in the sense of §3.1. A _policy tree context_, written `ctx`, is a policy tree with exactly one distinguished _hole_ `□`.

Every edit in this grammar is _atomic_: it carries a well-formed scheduler to a well-formed scheduler in a single step. This is a deliberate restriction. Edits that would have to destroy structure still holding packets are _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§5) only after ensuring that the subtree being removed is empty. The richer reconfigurations a user may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _sequences_ of these atomic edits; §4 makes the sequencing precise.

A few words on contexts. The hole is a reserved, leaf-shaped slot, not an absence of a slot: the policy at the _parent_ of the hole has an arity that includes the hole. The node `RoundRobin(A, B, □)` runs a round-robin policy of arity three whose third slot is vacant; it is different from `RoundRobin(A, B)`, which just runs a round-robin policy of arity two. We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`; the plug is total, and valid whenever `s` and `ctx` are valid.

Notes on the individual edits:

- `Add` carries a `weight` exactly when the slot it edits hangs off a WFQ parent, which needs a weight to schedule the subtree; for any other parent the weight is absent. It adds subtree `tree` at the new slot `next@path`. The queues of `tree` are empty. The new subtree carries fresh traffic that intersects nothing `prev` was already serving (it was being rejected thus far, say).
- `Quiesce (path)` stops routing any new traffic to `prev@path`. It is a transaction-only edit: topology and contents are untouched, so the quiesced subtree keeps being served and drains under load. It adds no structure and removes none.
- `Designate (path, tree)` spawns `tree` as a sibling of `prev@path`. The new tree is the _designated survivor_ of `prev@path`: the new tree can receive new packets, but, while `prev@path` has packets, the new tree cannot be popped.
- `Remove (path)` structurally removes `prev@path`, dropping its slot and renumbering any higher siblings. The subtree must be _empty_: removing an occupied subtree would leave its parent with dangling indices.
- `ChangeWeight (path, weight)` targets `prev@path`; it overwrites the weight that `prev@path`'s parent uses for it. It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft (ctx)` spawns `ctx` and plugs its hole with `prev`, which keeps its in-flight contents. The ancestor nodes of `prev` in `ctx[prev]` are populated with the indices required to make `ctx[prev]` well-formed. The rest of the tree is spawned empty.

### 3.3 All Productions of `diff` are Sound

Each production of §3.2 is an _atomic diff_: a transformation `δ` that, applied to the live control `C`, replaces it with another control `δ(C)` between two user `push`/`pop` operations, so that every operation is served by exactly one control. We require a diff to be _sound_: `|- C` must imply `|- δ(C)`. In this section we prove that this holds for all the productions of `diff`.

The operational diff `δ` is the object that does the work: it rewrites all of `(s, q, z)` at once. It is useful to snap it open and look at a much lighter object that makes `δ` work. This is the edit's _denotation_: a partial function on policies (§3.1), `policy -> policy`, defined by recursion on the `path`, that says which `next` the edit produces from a given `prev`. This denotation is purely static: a `policy` carries only topology, disciplines, and labels, and no live contents or state.

These two objects pin down complementary parts of the control `δ` produces, and each gives the section one obligation.

- _Realization_ constrains the static skeleton. The `policy` denoted by `δ(C)` must be the edit's denotation applied to the `policy` that `C` denotes. An example in mathematical notation: `⌊δ(C)⌋ = Add(path, tree)(⌊C⌋)`. That same example in a more human-readable form: `Strict(gmail, zoom, spotify) = Add(path = [2], tree = spotify) Strict(gmail, zoom)`. Because a `policy` fixes both the topology of `q` and the transaction `z`, this one equation constrains the new tree's _shape_ and the new transaction together, and says nothing about contents.
- _Soundness_ constrains the live contents. `|- C` must imply `|- δ(C)`. Well-formedness (§3.1) is a property of `q`'s contents alone, so this is the obligation that the packets and index entries `δ` leaves behind or introduces still satisfy `|-`.

State `s` is bookkeeping threaded along by `δ` and is governed by neither obligation directly. The denotation also doubles as the correctness statement for the transition planner of §4, which only ever sees static policies: the edit it emits between `prev` and `next` is correct exactly when the edit's denotation carries `prev` to `next`.

The denotational rules frequently read, overwrite, and splice child lists. Let us fix some notation. We write `ts[i]` for the `i`-th child and `ts[t/i]` for `ts` with its `i`-th child overwritten by `t`; this leaves the arity unchanged. The two arity-changing edits carry a sign: `ts[+t/i]` is `ts` with `t` spliced in as the new `i`-th child (the old `i`-th and later children shift one place to the right), and `ts[-/i]` is `ts` with its `i`-th child dropped (later children shift left). Indices follow the `path` convention of §3.2.

The operational diffs of §3.3 manipulate two parallel structures at an internal node: a child list `qs` of PIFO-tree children, to which the list-manipulation notation introduced above applies verbatim, and an index-PIFO `p` recording child indices. For `p` the same `[±/k]` notation _renumbers_ rather than splices, because we often need to open up or close a slot by shifting indices. `p[+/k]` is `p` with every entry `>= k` bumped up by one (opening up slot `k`); `p[-/k]` is `p` with every entry `> k` brought down by one. Entries below the edit point are left alone.

#### 3.3.1. `Add(path, tree, weight?)`

##### Denotation

`Add` is the structural map

```
Add : path -> tree -> policy -> policy
Add (i :: [])   s  (P ts) = P ( ts[+s / i] )
Add (i :: rest) s  (P ts) = P ( ts[ (Add rest s ts[i]) / i ] )
```

The base case fires once `path` reaches the new slot's parent: the subtree `s` is spliced in as the new `i`-th child. (Recall from §3.2 that `Add`'s `path` is read in `next` and names the new slot, so its final index `i` is the insertion point.) The recursive case walks down a shared ancestor, recurses into child `i`, and writes the result back. A WFQ parent also needs the new slot's weight; with children zipped as `(weight, subtree)` pairs `cs`, the weight enters only at the insertion site, and descent leaves the weights untouched:

```
Add (i :: [])   (w, s) (WFQ cs) = WFQ ( cs[ +(w, s) / i ] )
Add (i :: rest) arg    (WFQ cs) = WFQ ( cs[ (w_i, Add rest arg s_i) / i ] )   where (w_i, s_i) = cs[i]
```

Our running edit denotes `Add [2] spotify Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 2] ) = Strict(gmail, zoom, spotify) = next`, as intended. A mid-tree edit is no different: `Add [1] spotify Strict(gmail, zoom) = Strict( (gmail, zoom)[+spotify / 1] ) = Strict(gmail, spotify, zoom)`, with the old `zoom` sliding from index `1` to index `2`.

##### The diff

We write `c` for the parent of the new slot (here the root `Strict`) and `k` for the new slot's index. The operational diff rewrites the live control `(s, q, z)` componentwise into `(s', q', z')`:

- _The state, `s -> s'`._ `s'` agrees with `s` everywhere, except that it records the initial local state for the new slot: the new subtree's own scheduling state, plus whatever per-slot bookkeeping `c`'s scheduler keeps (a RoundRobin cursor, the slot's weight taken from the edit's `weight?`, a virtual-finish accumulator, etc.). No existing slot's state is disturbed.
- _The tree, `q -> q'`._ At `c = Internal(qs, p)` we splice the new subtree in at index `k`: `q' = Internal(qs[+tree/k], p[+/k])`. Here `tree` enters as a PIFO tree with empty queues at every node. Splicing at `k` shifts the old children at indices `>= k` one place to the right, and renumbering `p` to `p[+/k]` tracks exactly that shift: every entry naming an old index `>= k` is bumped up by one, so each old slot keeps its matched count of occurrences and packets, now under a new name. When `k` is the last index (the append case) nothing is `>= k`, so `qs[+tree/k] = qs ++ [tree]` and `p[+/k] = p`: no child shifts and the parent's index-PIFO is untouched.
- _The transaction, `z -> z'`._ `z'` is the scheduling transaction compiled from `next`. It extends `z` up to the renumbering at `c`: a packet that does not classify into the new subtree gets the same path that `z` would have emitted, with its step at `c` bumped by one whenever that step named an old index `>= k` (the path-level image of `p[+/k]`). A packet that _does_ classify into the new subtree gets a path whose step at `c` selects `k` and then descends through the new tree. No `prev` packet was ever routed to `k`: under `next`, `k` names the new subtree, and the old occupant of `k`, if any, now lives at `k+1`.

##### Soundness

Recall that we have two obligations.

- _Realization:_ the tree step above sets `q'` to `Internal(qs[+tree/k], p[+/k])`, which is `prev`'s tree with an empty `tree`-shaped slot spliced in at `k`, and `z'` is compiled from `next`, so `⌊C_next⌋ = Add(path, tree)(⌊C_prev⌋)`: the new shape and transaction are together exactly what the edit denotes.
- _Soundness:_ `|- C_prev` gives `|- C_next`. The parent `c` has exactly zero occurrences of `k`: its index-PIFO is now `p[+/k]`, which by construction names no `k` (old entries `< k` stay put, old entries `>= k` were bumped to `>= k+1`). The new subtree `tree` holds zero packets or indices, so the well-formedness obligation at slot `k` reads `0 = 0`. Every other slot is a child it was in `q`, with the same packets beneath it; its entries in `p[+/k]` are the old entries under a possibly-shifted name, so its matched count is inherited verbatim. Nothing needs repair.

##### Notes

_Atomicity._ No in-flight packet straddles the diff: every packet resident at the diff instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`. The renumbering `p[+/k]` relabels index _values_ only; it moves no packets and changes no ranks. So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have, the empty new slot contributing nothing and the existing subtrees keeping their contents and relative priority under shifted index names. The first `push` that `z'` routes to `k` is the first packet ever to occupy `tree`.

_Deeper paths._ The running example edits the root, but `path` may be any prefix; `Add { path = [1, 2]; tree = ... }` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes that `q` and `q'` share verbatim, and, because the new subtree is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor PIFO is rewritten, and the edit is confined to `c` and the fresh subtree below it.

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

Same tree, same surviving packets, two different service orders. The _choice of which_ `1`s to drop\_ silently reschedules `A` against `C`, and nothing in `prev` or the deletion request records which the user wanted.

We do not wish to make a scheduling decision for the user, so our only other option is to reconstruct the tree as if `B` had never been admitted: drain everything out, filter the `B`-packets, and re-push, recomputing every rank and cursor along the way. This is a stop-the-world rebuild.

Our grammar sidesteps the question entirely. By draining `B` to empty before removing it (§4), every ancestor's count of entries for `B` is already zero at the instant `Remove` fires, so there is nothing to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

##### Denotation

##### The diff

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
- [AM: question for Zhiyuan: §3.4 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes an atomic §3.3.1 diff. But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 7. Evaluation

## 8. Related Work

## 9. Conclusion
