# Live Reconfiguration of Hierarchical Packet Schedulers

## 1. Introduction

- Programmable packet scheduling.
  Emphasize that policies are often hierarchical, and that clients demand line rate.

- The reconfiguration problem.
  Running example: a small-office gateway runs `Strict(gmail, zoom)`, i.e., strictly prioritizing `gmail` traffic over `zoom` traffic.
  The operator wants to add a new `spotify` flow, and has two natural ways to do it:
  1. `Strict(gmail, spotify, zoom)`: extend the strict-priority list with `spotify` slotting between `gmail` and `zoom` in priority.
  2. `Strict(gmail, RoundRobin(zoom, spotify))`: keep `gmail` on top, but have `zoom` and `spotify` share the lower tier via round-robin.

  In either of these cases, the state of the art would stop the world, drop or recirculate buffered packets, recompile, and reinstall.
  The costs are dropped or recirculated (and so delayed) packets, downtime, and the rebuilding of nodes that did not need to be rebuilt.

- The alternative is to reprogram a scheduler without stopping the world.
  Let's revisit the examples from earlier.
- Transitioning to `Strict(gmail, spotify, zoom)` is actually quite easy.
  We can achieve the strongest property we could ask for:
  - time1: `Strict(gmail, zoom)` is running
  - time2: the request to move to `Strict(gmail, spotify, zoom)` is received.
    `Strict(gmail, zoom)` is still running
  - time3: we move to `Strict(gmail, spotify, zoom)`.
    Whatever user-observable interaction (push/pop) happened immediately before time3 happened entirely in the `Strict(gmail, zoom)` regime, and whatever push/pop happened immediately after time3 happened entirely in the `Strict(gmail, spotify, zoom)` regime.
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
    The transition period is shaped by hardware-level manipulations on the way to realizing `p2`, and not by a clean human-readable semantics.
    One might naturally worry that it will be hard to pin down the semantics of this transitional regime.
    We develop a grammar of atomic transitions (§4) that carries us from `p1` through `link` to `p2`, and any `link` produced by that grammar is itself an ordinary scheduling control in the sense of §3.1: so it _does_ have a human-readable semantics, just as `p1` and `p2` do.
  - Obligation 2.
    Improve upon the state of the art.
    We will give the semantics of the stop-the-world `link` and use it as a baseline.
    Several practical goals guide us as we look for better ways to transition from a given `p1` to a given `p2`.
    Can we minimize the length of the transition period?
    Can we avoid dropped/delayed packets?
    [AM: more to come here; the cost model is a legit open question!].
    We will show that it is always possible to make a transition from any `p1` to any `p2`, but it is occasionally possible to make a very efficient transition.
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
  The easy append, to `Strict(gmail, spotify, zoom)`, looks like the kind of edit vPIFO's substrate could absorb in place: a targeted append to the structures that encode the tree shape and per-instance storage (the Operation Generation Table and the PIFO Instance Address Table) would plausibly register the new traffic and its operations while leaving the running instances untouched.
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
§3.2 defines a small policy DSL and a compiler from the DSL into a runnable _control_, giving us the syntactic handle on controls that the rest of the section needs.
§3.3 fixes a grammar `δ` of structural edits over that DSL, where every production of `δ` is, by construction, atomically realizable in hardware.
§3.4 proves each production sound: for every production of `δ`, we say what running it does to the live tree, show that this matches the policy-level edit we were expecting, and check that the rest of the tree is left alone, with unrelated nodes keeping their state and the live packets staying accounted for.
§3.5 argues that this per-production soundness survives the lowering to hardware.
The productions of `δ` need to be arranged in guarded sequences in order to realize a full reconfiguration; this is deferred to §4.

### 3.1 Background: PIFO trees

The PIFO tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3] is what we build on.
We recap topology, the two observable operations, and well-formedness.

##### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies.
A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`.
The data is of two forms.
A leaf `Leaf(p)` holds a packet-carrying PIFO `p`.
An internal node `Internal(qs, p)` itself carries two kinds of data: a list `qs` of well-formed PIFO tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`.
This separation between the topology and the carried contents is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

##### The two observable operations

`push(q, pkt, pt)` enqueues `pkt` into tree `q` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`.
The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue.
The trailing element is asymmetric: a bare rank `r_{n+1}` with no slot index, since it tells the leaf's PIFO what rank to use when enqueuing the packet itself.
`pop(q)` returns the most favorably ranked packet in the tree by popping the tree's root to yield a child index, recursing into that child, until finally emitting a packet from the leaf.
These are the _only_ observable interactions with a scheduler, which is why our notion of an _atomic_ transition is stated in terms of `push`/`pop` observability.

##### Well-formedness

A PIFO tree `q` is well-formed (written `|- q`) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`.
This is the invariant that keeps `pop` from ever getting stuck.
`push` always preserves `|- q`, and `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

### 3.2 A Policy DSL

_FA_ treats PIFO trees as fully-formed runtime representations; there is no "constructor" for a network operator to create an _FA_-style formal object.
To talk about reconfigurations, we need to expose a programming interface for the network operator.
We design a small policy DSL `pol` using which the operator can specify their desired policy, and a compiler from `pol` terms to runnable PIFO tree _controls_ (defined below).
The transition planner of §4 needs both a way to compile a starting control `C` from an operator's request and a way to compare two such requests; `pol` is the common syntactic surface both rely on.

This is essentially what the vPIFO paper's _Scheduling Description Language_ does informally [cite vPIFO, §4].
They do not pin down a grammar for SDL or formalize the compilation, so our DSL can be read as a formal core of their concrete language.
The compilation targets differ: they compile straight to a virtualized PIFO substrate, whereas we compile first to a control (the abstraction that this section reasons over) and only then lower to a substrate (§6).
The strategy, though, is the same: give the operator a syntactic surface, then compile.

##### Policy syntax: `pol`

```
pol    ::= flow                   // leaf, labeled by a flow of traffic
         | D(pol_1, ..., pol_n)   // internal node, discipline D
```

This grammar allows policy trees of arbitrary arity.
`D` ranges over the disciplines (`Strict`, `RoundRobin`, `WFQ`, etc.).
A discipline may need per-arm metadata that shapes how the arm is scheduled.
`WFQ` takes a positive real weight per arm; `Strict` takes a priority rank per arm; `RoundRobin` takes nothing.
The grammar carries no structural mark of any of this.
The operator writes the metadata in the surface syntax (e.g., `WFQ(w_1: pol_1, ..., w_n: pol_n)`, or positional sugar like `Strict(A, B)` which desugars to `Strict(hi: A, lo: B)`).
Once compiled, the metadata lives in the arm's `slot_state` (see `init_slot_D` below), where `push`/`pop` can read it but not modify it.
We read the arity off by counting children, so `Strict(gmail, zoom)` is the 2-ary instance.
Each leaf label denotes a flow: a predicate over packets.

Arm order in the surface notation is a presentation choice, not a scheduling-meaningful one: `Strict(hi: A, lo: B)` and `Strict(lo: B, hi: A)` describe the same scheduler.
We formalize this below as the _reorder-congruence_ `=R` on `pol`, the smallest congruence under which permuting siblings at any internal node is a no-op.
From §3 onward we use `=R` as a degree of freedom: the compiler is free to pick any representative of an `=R`-class when laying out or editing a control.

A `pol` is _valid_ when (a) every discipline is applied at a proper arity, (b) every discipline is provided with the per-arm metadata that the discipline requires, and (c) the flows at the leaves are pairwise disjoint, in the sense that every incoming packet is either dropped or is routed to exactly one leaf.
Validity is a condition on the source `pol`, not to be confused with the runtime invariant `|- q`.

##### Discipline compilation: `init_node_D` and `init_slot_D`

Each discipline `D` comes with a mechanical recipe for compiling a node that runs it: (a) a per-node scheduling transaction, (b) an initial `node_state` for the node, and (c) a `slot_state` for each child of the node.
We name the two state-seeding projections:

```
init_node_D : () -> node_state
init_slot_D : node_state × meta? -> slot_state
meta?  ::= ε  |  priority-rank  |  weight
```

The `meta?` argument is the per-arm metadata that `D` requires (a weight for `WFQ`, a priority rank for `Strict`, absent for `RoundRobin`).
`init_node_D` is called only at _compile time_, once per node, to seed that node's `node_state`.
`init_slot_D` is called in two places.
At _compile time_, walking the source pol, it is called once per child arm to seed that child's `slot_state`, taking the parent's just-seeded `node_state` and the arm's `meta?` as input.
When a new arm is spliced under an already-running `D`-parent (§3.4.1), `init_slot_D` is called once with the parent's _current_ `node_state` and the new arm's `meta?` to produce the new arm's `slot_state`.

Choosing `init_slot_D` is a scheduling decision, not just a structural one, since the choice changes how a freshly spliced arm competes with the established arms.
We make the choice to "join the current round".
For example, if we go from `WFQ(A,B)` to `WFQ(A,B,C)`, we do not want the newly added `C` to reap a huge benefit for "having been silent all this while"; we just want it to join the others with neither a penalty nor an advantage.
To this end: `init_slot_RoundRobin` returns the empty tuple (no per-arm bookkeeping to seed); `init_slot_Strict(_, p) = p` (the arm's `slot_state` is just its priority rank, drawn from a dense total order such as the rationals so that a fresh priority can always be slotted strictly between two existing ones).
For `WFQ`, the `node_state` at a parent is the virtual time `vt`, and we set `init_slot_WFQ(vt, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag.
This is enough: any standard WFQ tagging that derives the next packet's tag from the slot's last-finish tag and the parent's `vt` (e.g., the GPS-style `max(virtual_clock, vt) + 1/w`) will slot the first packet on this arm into the round that the established arms are currently in.
At compile time the parent's `vt` is freshly initialized (to zero), so all original arms get `(w, 0)` and the round is "the zeroth"; at `Add` the parent's `vt` is whatever the clock has advanced to.

##### Policy Compilation: `pol` to control

A PIFO tree _control_ `C` is a tree of triples `(state, pifo, z)`, one per node of the topology.
The tree shape exactly matches that of `pol`, as each node of `C` lines up with a node of the source `pol`.

We write `⌈p⌉` for the control compiled from `pol` `p`; the compile rule fills in the local triple at each node of `p`'s topology as follows:

- `state` is a pair `(node_state, slot_state list)`.
  The `node_state` is seeded by `init_node_D()`.
  The `slot_state` list carries per-arm bookkeeping, each entry seeded by `init_slot_D`.
  Disciplines without per-arm bookkeeping (e.g., `RR`) have an empty `slot_state` list.
- `pifo` is an empty PIFO: an index-PIFO at an internal node, a packet-PIFO at a leaf.
- `z` is `D`'s _scheduling transaction_ at the node.
  It examines the local `state` and the incoming packet and produces a path segment and an updated `state`.
  We write `⇀` for partial functions; `z` is partial because not every packet is admitted at every node.
  The shape of the path segment differs between internal nodes and leaves:
  - at an internal node, `z : state × Pkt ⇀ (idx × rank) × state`: pick a child index `i` and the rank `r` with which to enqueue `i` at this node's index-PIFO;
  - at a leaf, `z : state × Pkt ⇀ rank × state`: pick the rank `r` for the packet's own PIFO entry.

  When `z` is undefined for a packet, this node's contribution to the global `push` is empty: nothing is enqueued at this node and `state` is unchanged.
  It is important for well-formedness that, when `z` is defined (resp. undefined) for a packet, it is defined (resp. undefined) along the entire path from leaf to root.
  This global property is not a concern of node-local `z`s.

We address node-local controls by a `path`, a (possibly empty) sequence of child indices read from the root.
The local triple at the node reached by following `path` from control `C`'s root is written `C@path`, with fields `C@path.state`, `C@path.pifo`, `C@path.z`.
We also write `C@path.node_state` and `C@path.slot_states` for the two components of `C@path.state`.

##### Well-formedness: `|- C`

In §3.1 we defined well-formedness on a PIFO tree, written `|- q`.
Now we redefine it, lifting it to act on a control `C`.
A control `C` is _well-formed_ (written `|- C`) when, at every internal node of `C`, the `pifo` has, for each legal child index `i`, exactly as many occurrences of `i` as there are packets stored in the leaf pifos of the subtree under the `i`-th child.
This is stated the same well-formedness property as before, and maintaining it has the same effect (preventing `pop`s from getting stuck).
We just state it directly on `C` so that no global PIFO tree needs to be assembled to check it.

##### Compatibility with Formal Abstractions

_FA_ also has the notion of a control triple, but theirs is a monolithic control that is attached to the entire PIFO tree, not an individual node.
The control `(s, q, z)` has a state map `s`, a PIFO tree `q`, and a single transaction `z : St × Pkt -> Path(t) × St`.
Our control, which is distributed to nodes, can easily be flattened into an _FA_-style triple.
The FA-style tree `q` is the tree of our `pifo` pieces; the FA-style state `s` collects the `state` pieces indexed by path; the FA-style scheduling transaction `z` walks the topology applying each per-node `z` in turn and appending the emitted path segments into paths.
The partiality that our per-node `z`s allow shows up as partiality on the FA-style global `z` (a drop anywhere along the descent leaves the global function undefined for that packet).
The rest of the paper has no need for gluing a control together in this way (`|- C` is stated directly per the previous paragraph, and the diff rules of §3.4 act node-locally), but the reader who prefers FA's framing can recover the global triple by this gluing.

##### Equivalence modulo pushes and pops: `~`

`push` and `pop` change the live `pifo`s and `state`s of a control but leave its structural skeleton untouched.
We write `C ~ C'` for the equivalence relation on well-formed controls that identifies any two controls related by a finite sequence of `push` / `pop` operations.

##### Reorder-congruence on `pol`: `=R`

We write `p =R p'` for the smallest congruence on `pol` such that, at any internal `D`-node, permuting the child arms gives a congruent pol: `D(p_a, ..., p_z) =R D(p_{σ(a)}, ..., p_{σ(z)})` for any permutation `σ`.
The per-arm metadata that discipline `D` requires (a `WFQ` weight, a `Strict` priority rank) travels with its arm under the permutation; the metadata is what carries the scheduling-meaningful content, so a permutation does not change the scheduler.

##### Equivalence on controls modulo presentation: `~R`

Two controls whose child lists at some internal node are permutations of one another (with the parent's `pifo` and `z` renumbered accordingly) present different positional layouts but realize the same scheduler.
We write `C ~R C'` for the equivalence obtained by closing `~` under such sibling permutations.
Every `~`-equivalent pair is `~R`-equivalent; the converse fails.

##### The bridge: `⌊·⌋`

We write `⌊C⌋` to mean "the `pol` that `C` realizes".
`⌊·⌋` is pinned down by three rules:

1. _Base case (compilation)._ `⌊⌈p⌉⌋ =R p`.
   The compiler is free to pick any sibling order when laying out `pol` `p` as a control, which is why we need the flexibility that `=R` affords us.
2. _Closure under pushes and pops._ If `C ~ C'`, then `⌊C⌋ = ⌊C'⌋`.
   Pushes and pops touch only live `state` and `pifo` contents; they leave the topology and `z` of every node verbatim, so the pol-level skeleton that `⌊·⌋` names is untouched.
3. _Closure under diffs._ Each grammar diff `δ` has two readings, both defined per-production in §3.4: an operational rewrite on the live control, written `[[δ]] : control ⇀ control`, and a closed-form pol-level effect, written `den(δ) : pol ⇀ pol`.
   §3.3 fixes the grammar; for now, take it on faith that for every `δ` we will define both readings.
   Rule 3 says the two readings agree: `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`.
   The `=R` slack is needed here because the operational rewrite has arm-order freedom, just as the compiler does.

The three rules together let us propagate `⌊·⌋` from any `⌈p⌉` along any sequence of pushes, pops, and diffs.
This is how we will discharge Obligation 1 of §1: telling the operator what `pol` is running even when no user has explicitly requested the `pol`.
Note that `⌊·⌋` is defined here only by closure from the base case `⌈p⌉`; controls that arise outside this closure (e.g., a transiently-malformed intermediate produced by an IR-level lowering, §3.5) do not have a `⌊·⌋` reading at the control level, and we recover their `pol`-level meaning only at the next commit boundary.

The interplay of the three rules is captured by the following diagram.

```
       p1  -------------den(δ)------------> p2
       |                                     |
      ⌈·⌉                                   ⌈·⌉
       |                                     |
       v                                     v
       C1 ~ C1'  -------[[δ]]------> C2' ~R C2
```

Let us study this diagram with an eye to the user's experience.
Our final goal will be to correctly relate `C2'` and `C2`.

- The operator writes `p1`. Then `C1 := ⌈p1⌉`, with `⌊C1⌋ =R p1` by rule 1.
- We echo `p1' := ⌊C1⌋` back to the user. This is not shown in the diagram but will become important shortly. `p1'` faithfully represents the arm ordering that the compiler may have done.
- Push and pop operations carry `C1` to `C1'`.
  `C1 ~ C1'` by the definition of `~`, and `⌊C1'⌋ = ⌊C1⌋` by rule 2, so the live `C1'` still realizes both `p1` and `p1'`.
- The operator writes `p2`; the sniffer (§4) produces a `δ` such that `den(δ)(p1') =R p2`.
  It is key that we work in the frame of the actually-running representative `p1'` rather than the operator's original `p1`, since `den` is stated using semantically meaningful paths.
- Applying `[[δ]]` to `C1'` brings us to control `C2'`, and by rule 3 `⌊C2'⌋ =R den(δ)(p1')`. Further, we can chain this with the fact `den(δ)(p1') =R p2` (established just above) to get `⌊C2'⌋ =R p2`.
- We again echo `p2 := ⌊C2'⌋` to the user.

The transformation is complete at this point, but we need to ground ourselves.
`C2 := ⌈p2⌉` is the control we _would have built_ had we taken the SOTA stop-the-world path: the state-of-the-art response to a `p1 -> p2` request is to drain `C1'`, throw it away, and freshly compile `⌈p2⌉` (§1).
We do not actually construct it, but it is the correct reference point and it is crucial that we now relate `C2'` (which we have just produced after a fashion) to `C2` (which SOTA would have produced).
By rule 1, `⌊C2⌋ =R p2`, hence `⌊C2'⌋ =R ⌊C2⌋`.
But we would like to relate the controls directly, not just their `pol`-level projections.
The relation we write is `C2' ~R C2`, which absorbs two gaps at once:

- `C2'` carries the live `pifo`/`state` accumulated since `C1`, while `C2` is freshly compiled and bare. The `~` component covers this.
- `[[δ]]` and `⌈·⌉` are free to pick different sibling orders at internal nodes, so `C2'` and `C2` may also differ in child arrangements. The R-closure covers this.

The diagram thus commutes at the level of `⌊·⌋` modulo `=R`: both routes from `p1` to a control realizing `p2` land in the same `~R`-class.
We do not actually construct the right-hand side `C2`; the commutation is asserted against a virtual reference point whose `⌊C2⌋ =R p2` holds by Rule 1.
The commutation holds verbatim for `pol`-visible diffs.
For `pol`-invisible ones like `Quiesce`, the commutation is vacuous on the SOTA side: a freshly compiled `C2 = ⌈p2⌉` carries no `z`-domain restriction, so there is nothing on that side to match against.

When writing `p2`, the operator would do well to state their request against `p1'`, the actually-running `pol` that we echoed back to them.
This keeps the diff small and aligns paths with what is running.
The operator may state `p2` against the unprimed `p1` they originally wrote, but at their own risk.
The risks are of two flavors:

- The diff sniffer may infer a larger diff than necessary, or may give up.
- The more serious issue is that the user may use Imperative Mode (§4) to directly specify what edits to make, and if they use the unprimed `p1` to base their paths, they may inadvertently edit the wrong node or provide a malformed path.

##### A worked example

- The operator initially requests `p1 = Strict(gmail_hi, zoom_lo)`.
- The compiler uses its degree of freedom to yield control `C1` such that `⌊C1⌋ = Strict(zoom_lo, gmail_hi)`. Note that the children have been reordered for some compiler-internal reason.
- We echo back `p1' := ⌊C1⌋ = Strict(zoom_lo, gmail_hi)`. We know that `p1 =R p1'`, so the scheduler the operator gets is the one they asked for; only the slot numbering differs.
- As the control serves pushes and pops, it transforms into `C1'`.
- Later, the operator requests `p2 = Strict(zoom_lo, spotify_mid, gmail_hi)`. Note that the operator has cleverly written `p2` based on `p1'`, not `p1`.
- The runtime can again use its freedom. Instead of literally splicing `spotify` in between running arms `zoom` and `gmail`, it chooses to append `spotify` to the end. This converts the running control `C1'` into control `C2'` such that `⌊C2'⌋ = Strict(zoom_lo, gmail_hi, spotify_mid)`.
- We echo back `p2' := ⌊C2'⌋ = Strict(zoom_lo, gmail_hi, spotify_mid)` to the user.
- Now the operator changes to Imperative Mode (§4) and writes the path-bearing edit `(True, Quiesce([2]))`. It is not worth getting distracted by the syntax or the semantics; the key thing is that the operator has requested an edit and has identified the target via a path `[2]`. Paths are interpreted against the _actually running_ representative `p2'`, so we `Quiesce` the subtree `spotify_mid` (`p2'`'s third slot), not `zoom_lo` (`p2`'s third slot). If the operator had based the path on `p2` they would have edited the wrong arm; the system has no way to detect or recover from that.

### 3.3 A Grammar for Tree Diffs

We fix a small grammar `δ` of atomic edits (we use "atomic" informally here; §3.4 pins it down formally).
Each production of the grammar has two readings, both defined per-production in §3.4: an operational rewrite `[[δ]] : control ⇀ control`, and a closed-form pol-level effect `den(δ) : pol ⇀ pol`.
The grammar is shaped by two demands.

- **Hardware-realizable.** A production is admitted into the grammar iff `[[δ]]` can be committed atomically by the hardware substrate (§6).
- **Pol-explainable.** Every production has a `den(δ)`; for transaction-only diffs (whose effect lives entirely in `z`), `den(δ)` is the identity on `pol`. This is what lets §4 sniff, check, and echo back the `pol`-level meaning of the running control to the user.

§3.4's per-production soundness theorem ties the two readings together: `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`.

Each edit names _where_ in the tree the change lands and _what_ the change is; we write `p1@path` for the subtree of `p1` reached by following `path` down from its root.

```
δ ::= Add          (path, pol, meta?)
    | ChangeWeight (path, weight)
    | Quiesce      (path)
    | Remove       (path)
    | Designate    (path, pol)
    | Undesignate  (path)
    | ChangeRoot   (path)
    | Graft        (ctx)

path   ::= []  |  i :: path             // i is a child index
ctx    ::= □                            // the unique hole
         | D(pol, ..., ctx, ..., pol)   // exactly one child is itself a context
weight ::= a positive real
```

`pol` is the nonterminal of §3.2.
A _policy context_, written `ctx`, is built like a `pol`, except that exactly one of its slots is the distinguished _hole_ `□` rather than a subtree.
The hole is a reserved slot, not an absence of a slot: the parent of the hole has an arity that includes the hole, e.g., `RoundRobin(A, B, □)` is a 3-ary `RoundRobin` whose third slot is the hole, distinct from the 2-ary `RoundRobin(A, B)`.
We write `ctx[s]` for the ordinary, hole-free tree obtained by plugging the hole of `ctx` with the subtree `s`.
A `ctx` is _valid_ iff `ctx[s]` is a valid `pol` for some valid `s`.
The plug is total, and for any valid `ctx` and any valid `s`, `ctx[s]` is a valid `pol` whenever the leaf labels of `ctx` and `s` are disjoint.

Edits that would have to destroy structure still holding packets are expressly _not_ in the grammar: our one structural deletion, `Remove`, is emitted by our transition planner (§4) only after ensuring that the subtree being removed is empty.
The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized as _sequences_ of these productions (§4).

Brief notes on each production:

- `Add(path, pol, meta?)` appends `pol` as a new child of `p1@path`. `meta?` carries per-arm bookkeeping for the new arm, if `p1@path` requires it.
- `ChangeWeight(path, weight)` overwrites the WFQ weight assigned to the arm at `path`.
- `Quiesce(path)` prevents the subtree at `path` from receiving any new traffic.
- `Remove(path)` removes `p1@path`; the subtree at `path` must be empty.
- `Designate(path, pol)` wraps `p1@path` into `Strict*(p1@path, pol)` in place, making `pol` the _designated survivor_ of `p1@path`. The need for the distinguished discipline `Strict*` is explained in §3.4.5.
- `Undesignate(path)` collapses the `Strict*(A, B)` that lives at `p1@path` into `B`, with `B` inheriting the slot and per-arm `meta?`. `A` must be empty.
- `ChangeRoot(path)` promotes `p1@path` to the new root, discarding the ancestor chain above it. The ancestor chain must be a unary "vine".
- `Graft(ctx)` produces `ctx[p1]` by plugging the running `p1` into `ctx`'s hole.

When `p1 =R p2` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.4 Discharging the Per-Production Obligations

The two demands that we stated at a high level in §3.3 turn into five concrete obligations that we must discharge for every production of the grammar.

- **Hardware-realizable** creates four obligations.
  For the substrate to install `[[δ]]` atomically, the per-production rule must specify what `[[δ]]` does, produce a target that is a valid control with fully specified state, and leave the user's `pop` stream undisturbed. Each of these is a concrete obligation:
  - _Definition._ Specify `[[δ]] : control ⇀ control` together with the preconditions under which it is defined; outside that region `δ` is _incompatible_ with `C` and `[[δ]](C)` is undefined.
  - _Preservation of `|-`._ `|- C` implies `|- C'`.
    Any packets or index entries `[[δ]]` drops or adds must leave the per-node pifo and packet counts in balance.
  - _Preservation of state._ At every node structurally shared between `C` and `C'` and outside the production's local edit site, the local `state` is preserved verbatim.
    At the edit site, and at every node of a freshly-spawned subtree, the state is exactly what the production's `init_node_D` / `init_slot_D` (§3.2) invocation prescribes.
  - _Preservation of observation._ Modeling `[[δ]]` as a function pins down what it means to be "atomic": the substrate commits the rewrite specified by `[[δ]]` _between two consecutive `push`/`pop` operations_, so there is no intermediate state for downstream sections to reason about. We defer to §6 that the substrate can slip this change in. In this section, we must show per production that the commit is invisible to the user's `pop` stream. Concretely: every in-flight packet in `C` sits in some `pifo` that survives verbatim into `C'`, and no live `pifo` entry is rewritten, so a `pop` immediately after the diff returns exactly what a `pop` immediately before would have returned.
- **Pol-explainable** creates one obligation:
  - _Characterization._ Give `den(δ)` in closed form and prove `⌊C'⌋ =R den(δ)(⌊C⌋)`.
    The equation is up to `=R`, not literal `=`: `den` returns a definite representative of an `=R`-class, and `[[δ]]` is free to land on any other representative of the same class.
    Transaction-only effects (e.g., `Quiesce`'s shrinking of `z`'s domain) sit outside `den`; they are fixed by the operational rule, not by this equation.

Write `C` for the pre-edit control and `C'` for the post-edit control.
The last four obligations assume `[[δ]](C)` is defined; outside the region the production's Definition carves out, `δ` is _incompatible_ with `C` and `[[δ]](C)` is undefined.
We do not repeat this boilerplate at every production.

For transaction-only productions (those whose effect lives entirely in some `z`, like `Quiesce` and `ChangeWeight`), `den(δ)` is the identity on `pol` and Characterization reduces to `⌊C'⌋ =R ⌊C⌋` by inspection: no node's `node_state`, `slot_states`, `pifo`, or child list moves, and `z` is not visible at `pol`-level.
We still state `den` explicitly at each production for uniformity.

The denotational rules below frequently read, overwrite, and splice child lists.
Let us fix some notation.
We write `ts[i]` for the `i`-th child and `ts[t/i]` for `ts` with its `i`-th child overwritten by `t`; this leaves the arity unchanged.
We mark arity-changing edits with `+` and `-`: `ts[+t/i]` splices `t` in as the new `i`-th child (the old `i`-th and later children shift one place to the right), and `ts[-/i]` drops the `i`-th child (later children shift left).
Indices follow the `path` convention of §3.3.

The operational diffs of §3.4 manipulate two parallel structures at an internal node: a child list `qs` of PIFO tree children, to which the list-manipulation notation introduced above applies verbatim, and an index-PIFO `p` recording child indices.
For `p` the same `[±/k]` notation _renumbers_ rather than splices, since the PIFO holds index _values_ that have to track a shift in the child list.
`p[+/k]` is `p` with every entry `>= k` bumped up by one (opening up slot `k`); `p[-/k]` is `p` with every entry `> k` brought down by one.
Entries below the edit point are left alone.

Several productions edit a single arm relative to its parent.
When the per-production rule hinges on this relation, we destruct the diff's path as `π ++ [k]`, with `π` the parent's path and `k` the local index by which that parent reaches the target.
§6 reuses this destructuring when lowering the same diffs to hardware.

As a warm-up, we discharge the five obligations in some detail for the production `Add`.
The remaining productions reuse the same obligations and arguments, so for them we present only what differs in substance: the closed-form `den`, the operationally interesting bits of the per-node rule, and the points where any of the preservation arguments departs from `Add`'s.

#### 3.4.1. `Add(parent, newpol, meta?)`

We rename §3.3's positional arguments for clarity: `parent : path` is the path to the node under which the new arm goes, and `newpol : pol` is the policy of the new arm; `meta?` is the discipline-dependent per-arm metadata of §3.3.

##### Definition

We must define `[[Add(parent, newpol, meta?)]] : control ⇀ control` together with the preconditions on the already running control under which `Add` is defined.

Say the presently running control is `C`.
Intuitively, `Add` inserts a freshly compiled subtree `⌈newpol⌉` as the new last child of the subtree at `C@parent`.
It also extends the scheduling transaction `z` at each proper ancestor of `⌈newpol⌉`, so that traffic can reach `⌈newpol⌉`.

`[[Add(parent, newpol, meta?)]](C)` is defined when:

- `parent` resolves in `C` to an internal node,
- `meta?` matches the slot-initialization schema of the discipline at that node (present iff `init_slot_D` for that discipline requires it), and
- `newpol`'s leaf labels are disjoint from those of `C`.

The transition `C' = [[Add(parent, newpol, meta?)]](C)` is stated per node.
Let `k = |C@parent.slot_states|` be the new arm's slot index.

- _At each proper ancestor of `parent`:_
  - `node_state`, `slot_states`, and `pifo` are preserved verbatim.
  - The local `z` is extended to admit packets that classify into the new subtree (the new leaf labels were outside every ancestor `z`'s domain in `C`, by the disjointness precondition), mapping them to whichever child slot lies on the path down to `parent`.
- _At `parent`:_
  - `node_state` is unchanged.
  - `C'@parent.slot_states = C@parent.slot_states ++ [ init_slot_D(C@parent.node_state, meta?) ]`. In English: we call `init_slot_D` to find the per-arm bookkeeping that is needed for a new arm under `D`, and we append that bookkeeping in at the end. Our arm-order freedom (§3.2) lets us simply append the new child.
  - `pifo` is unchanged: no pre-existing entry needs renumbering, and the new arm holds no packets yet.
  - `C'@parent.z` extends `C@parent.z` to admit packets that classify into the new subtree, mapping them to child index `k` at `parent`.
- _At every node inside `⌈newpol⌉`:_ the local control is `⌈newpol⌉`'s, verbatim.
- _At every other node (outside `⌈newpol⌉` and not `parent` or one of `parent`'s proper ancestors):_ the local control is preserved verbatim.

##### Preservation of |-

We must show that `|- C` implies `|- C'`: any pifo entries or packets that `[[Add(parent, newpol, meta?)]]` introduces must leave the per-node pifo and packet counts in balance.

At `parent`, `C'@parent.pifo = C@parent.pifo` contains no entry equal to `k` (no pre-existing entry can name a slot that did not exist in `C`), and the new arm at slot `k` holds no packets, so its well-formedness count reads `0 = 0`.
Every other slot at `parent` keeps its index, its packets, and its entries in `C'@parent.pifo` verbatim, so its matched count is inherited.
Every other node's pifo is untouched (the ancestor `z` extensions touch no pifo at this instant; they only affect the classification of packets that arrive later).
Nothing needs repair.

##### Preservation of state

We must show that at every node structurally shared between `C` and `C'` and outside the edit site, the local `state` is preserved verbatim; and that at the edit site, and at every node of the freshly spawned subtree, the `state` is exactly what `init_node_D` / `init_slot_D` (§3.2) prescribes.

Outside the edit site the local control (and thus its `state`) is preserved verbatim, including at each proper ancestor of `parent`, where only `z` changes.
Inside `⌈newpol⌉`, every node's `node_state` and `slot_states` are what `init_node_D` / `init_slot_D` (§3.2) prescribe, by construction of `⌈newpol⌉`.
At `parent`, `node_state` is unchanged and `slot_states` is appended with exactly `init_slot_D(C@parent.node_state, meta?)`, as required at the edit site.

##### Preservation of observation

We must show that the commit is invisible to the user's `pop` stream: every in-flight packet in `C` sits in some `pifo` that survives verbatim into `C'`, and no live `pifo` entry is rewritten, so a `pop` immediately after the diff returns exactly what a `pop` immediately before would have returned.

No in-flight packet straddles the diff.
At the diff instant, every packet sits in some pre-existing node's `pifo`, and each such packet survives into the same `pifo` at the same slot index in `C'`.
The new slot `k` holds nothing, and no pifo entry is rewritten.
So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
Observation is preserved.

##### Characterization

We must state `den(Add(parent, newpol, meta?))` in closed form and prove `⌊C'⌋ =R den(Add(parent, newpol, meta?))(⌊C⌋)`.

We define `den` by recursion on `parent`:

```
den(Add([],        newpol, meta?)) (D ts) = D ( ts ++ [newpol] )
den(Add(i :: rest, newpol, meta?)) (D ts) = D ( ts[ den(Add(rest, newpol, meta?)) (ts[i]) / i ] )
```

The base case applies once `parent = []`: the recursion has reached the node where the new arm goes, and `newpol` is appended as the last child.
The recursive case walks one step deeper into child `i` and writes the result back in place.
The `meta?` argument is threaded through the recursion but does not appear in the closed form: it is per-arm bookkeeping consumed by `init_slot_D` at the operational level, so it is pol-invisible.

_Proof of characterization._
We argue that the structural skeleton of `C'` matches `den(Add(parent, newpol, meta?))(⌊C⌋)`.
The operational rule above leaves every pre-existing arm structurally intact (the `z` extensions along the ancestor chain are pol-invisible) and adds a new arm at `parent`.
That new arm is exactly `⌈newpol⌉`.
Descending `parent` in `⌊C⌋` traces the recursion step for step: at each proper ancestor, step into the child named by `parent`; at `parent`, append `newpol` to the child list.
For `Add`, `⌊C'⌋` and `den(Add(parent, newpol, meta?))(⌊C⌋)` are in fact _equal_ as pol-trees, not just `=R`-equivalent: at each proper ancestor of `parent` the child lists agree pointwise, and at `parent` both child lists are `ts ++ [newpol]`.
We still phrase the characterization mod `=R` to keep a uniform shape across productions:
`⌊C'⌋ =R den(Add(parent, newpol, meta?))(⌊C⌋)`.

#### 3.4.2. `ChangeWeight(target, weight)`

Throughout, `target : path` is the path to the arm whose weight changes; it is non-empty (see precondition below) and we destruct it as `π ++ [k]`.

Say the presently running control is `C`.
Intuitively, `ChangeWeight` overwrites `C@π.slot_states[k].weight` with the new weight provided.

##### Definition

`[[ChangeWeight(target, weight)]](C)` is defined when `target` is non-empty and the parent at `π` runs WFQ.

- _At `π`:_ `node_state` is unchanged; `slot_states[k].weight` is overwritten with the new weight. Every other field of `slot_states[k]` is preserved verbatim, including the virtual-finish tag that records how far slot `k` has run in the current round. Other `slot_states`, `pifo`, and `z` are unchanged.
- _Everywhere else:_ preserved verbatim.

##### Preservation

Preservation of `|-`: no `pifo` entry is rewritten, no subtree is touched, no packet is relocated, so well-formedness is inherited everywhere.
Preservation of state: the targeted weight field is the intended edit; every other field at `π` and everywhere else is verbatim.
Preservation of observation: ranks for in-flight `pifo` entries were determined when they were pushed. They remain fixed; the new weight affects only the rank computation for future pushes.
So a `pop` immediately after this `[[ChangeWeight(target, weight)]]` returns exactly what a `pop` immediately before would have.

The virtual-finish tag at slot `k` is deliberately preserved rather than reset: resetting would either reward or penalize the targeted arm by yanking it out of the current round, whereas `init_slot_WFQ` (§3.2) was designed precisely to let a fresh arm "join the current round" without disturbing siblings, and the same principle applies to a weight change on an already-running arm.

##### Characterization

```
den(ChangeWeight(target, w)) p = p
```

is defined whenever `target` resolves in `p` and the parent at `π` runs WFQ.
Every node's `node_state`, `slot_states.discipline`, `pifo`, and child list are preserved verbatim (the rewritten field is in `slot_state`, which the compilation rule strips), so `⌊C'⌋ = ⌊C⌋`.
Hence `⌊C'⌋ =R den(ChangeWeight(target, weight))(⌊C⌋)`.

#### 3.4.3. `Quiesce(target)`

Throughout, `target : path` is the path to the subtree to silence.

Say the presently running control is `C`.
Intuitively, `Quiesce` restricts `z`s at various parts of the control to ensure that any packet bound for a leaf under `C@target` is refused admission.

##### Definition

`[[Quiesce(target)]](C)` is defined whenever `target` resolves to a node in `C`.
The empty path is permitted: a root-`Quiesce` silences every leaf in the tree, which is meaningful as the first step of a whole-tree retirement.
There is no precondition on the subtree's contents: `Quiesce` is the production that stops further admissions, regardless of what is currently held below `target`.

Let `T` denote the set of leaves of `C@target` (the leaves we are silencing), and let `A` denote the union of their ancestor chains: every internal node within `C@target`'s subtree, the node `C@target` itself, and every ancestor of `C@target` up to and including the root.
The topology, the discipline at every node, and every arm's slot index are unchanged.

- _Outside `T ∪ A`:_ the local control is preserved verbatim.
- _At each leaf `L` in `T`:_ `node_state`, `slot_states`, and `pifo` are preserved verbatim (including any packets `L` currently holds).
  `C'@L.z` restricts `C@L.z`: every packet that `L` used to admit is removed from `C'@L.z`'s domain.
- _At each internal node `n` in `A`:_ `node_state`, `slot_states`, and `pifo` are preserved verbatim (including any pre-existing entries pointing toward the quiesced subtree, which continue to be honored on `pop`).
  `C'@n.z` restricts `C@n.z`: packets whose classifier predicate matches any leaf label in `T` are no longer in `C'@n.z`'s domain.
  For surviving inputs the output is `C@n.z`'s output verbatim.

##### Preservation

Preservation of `|-` and state: every node's `node_state`, `slot_states`, `pifo`, and child subtree are preserved verbatim, so well-formedness counts are inherited everywhere and `state` is preserved at every node.
The `z` restrictions across `T ∪ A` touch no `pifo` entry and no stored packet. They only refuse future `push`es.
Preservation of observation follows from §3.4.1's argument: every in-flight packet sits in some pre-existing `pifo` that survives verbatim, so a `pop` immediately after this `[[Quiesce(target)]]` returns exactly what a `pop` immediately before would have.

##### Characterization

`Quiesce` is `pol`-invisible: its entire effect lies in `z`'s domain, which `pol` does not record.

```
den(Quiesce(target)) p = p
```

is defined whenever `target` resolves in `p`.
Every node's `node_state`, `slot_states`, `pifo`, and child list are preserved verbatim. Only the `z`s at nodes in `T ∪ A` are restricted, and `z` is not visible at `pol`-level.
So `⌊C'⌋ =R ⌊C⌋ = den(Quiesce(target))(⌊C⌋)`.

##### Note: Why touch every node on every quiesced push path.

Under §3.2's parallel `push`, every node on the path from a packet's destination leaf to the system root mints an index via its local `z` independently of the others.
If we restricted `z` only at a strict subset of those nodes (only the root of the tree, or only inside `C@target`), the rest would happily mint indices and enqueue them, leaving the tree in a malformed state.
`Quiesce` therefore restricts uniformly, at the leaf (so it refuses to enqueue) and at every ancestor of every quiesced leaf (so no one mints a stray index).
The cost is touching `|T|` leaves and the union of their ancestor chains; the gain is a rejection that preserves `|- C'`.

#### 3.4.4. `Remove(target)`

Throughout, `target : path` is the path to the arm being removed; it is non-empty (see precondition below) and we destruct it as `π ++ [k]`.

Say the presently running control is `C`.
Intuitively, `Remove` unhooks the subtree at slot `k` of `C@π` and renumbers higher siblings down by one.
It is defined only when the subtree at `target` is empty; we explain this restriction below.

##### Definition

`[[Remove(target)]](C)` is defined when `target` is non-empty and resolves to a node in `C`, the subtree at `target` is empty, and `C@target` does not carry the `Strict*` flag (the planner reaches a `Strict*` only through `Undesignate`, see §3.4.6).

The topology loses the arm at slot `k` of `π`; arms at slots `0, ..., k-1` keep their indices; arms at slots `k+1, ...` shift down by one.

- _Outside `π` and the removed subtree:_ the local control is preserved verbatim.
  This includes every proper ancestor of `π` (whose `z` is unchanged) and every sibling arm at `π` other than slot `k`, together with the subtrees under them.
- _At `π`:_
  - `node_state` is unchanged.
  - `slot_states = C@π.slot_states[-/k]`: the entry at slot `k` is dropped. Entries at slots `> k` shift down by one to track the renumbered arms.
  - `pifo = C@π.pifo[-/k]`: the precondition (subtree at `target` is empty) plus `|- C` forces `C@π.pifo` to contain _no entry equal to `k`_, so this renumbering deletes no values; it only decrements entries `> k` by one.
  - `z` is restricted on inputs and renumbered on outputs.
    Packets that `C@π.z` would have routed to slot `k` are no longer in `C'@π.z`'s domain. For surviving inputs, an output of `(i, r)` with `i > k` becomes `(i - 1, r)`.
    Slots `< k` are untouched on either axis.

A standalone `Remove` thus restricts classification at `π` so that packets bound for the removed subtree are dropped at `π` rather than being routed to a non-existent slot.
In the typical planner usage, a preceding `Quiesce` has already restricted every `z` on every push path to the soon-to-be-removed leaves (§3.4.3), so no new packet bound for the removed subtree is even being admitted; the input restriction at `π` is then redundant but harmless, while the output renumbering at `π` is still needed.

##### Preservation

Preservation of `|-`: at `π`, the precondition gives `C@π.pifo` no entry equal to `k`, so the renumbering `[-/k]` deletes no values; surviving slots `i' < k` of `C'@π` inherit their matched counts from slot `i'` of `C@π`, and surviving slots `i' >= k` inherit theirs from slot `i' + 1` (same subtree, renumbered entries).
Every proper ancestor and every surviving sibling is verbatim, so its well-formedness count is inherited.
Preservation of state: every proper ancestor and surviving sibling preserves its `state` verbatim. No `init`-rule fires, and `slot_states` drops slot `k`'s entry while every other entry is preserved verbatim, with its position shifted to match the new arm order.
Preservation of observation follows from §3.4.1's argument applied to the surviving structure: every in-flight packet sits in a preserved `pifo` at the same slot (if `< k`) or one slot lower (if `> k`), and every surviving `pifo` entry points to the same child subtree after renumbering, so a `pop` immediately after this `[[Remove(target)]]` returns exactly what a `pop` immediately before would have.
The restriction at `C'@π.z` comes into play only for `push`es that arrive after this `[[Remove(target)]]` fires.

##### Characterization

```
den(Remove([k]))         (D ts) = D ( ts[-/k] )
den(Remove(i :: rest))   (D ts) = D ( ts[ den(Remove(rest)) (ts[i]) / i ] )      when rest is non-empty
```

The base case fires once `target` has been walked down to the parent of the removed arm: the `k`-th child is dropped from the arm list.
The recursive case walks one step deeper into child `i` and writes the result back in place.
`den(Remove(target))` is defined when `target` is non-empty and resolves in the input pol; the emptiness precondition that `[[Remove(target)]]` imposes is operational, not visible at `pol` level.

Proof shape is the same as `Add`'s: outside the removed subtree every node is structurally intact (ancestors verbatim, surviving siblings verbatim), and at `π` the child list shrinks by dropping slot `k`.
So `⌊C'⌋ =R den(Remove(target))(⌊C⌋)`.

##### Note: Why an empty subtree?

Deleting an _occupied_ child has no canonical local realization.
Consider `P(Q(A, B), C)`.

_Diagram legend._
Each internal node is labeled with its index-PIFO and each leaf with the packets it holds.
Each PIFO is drawn with its most favorably ranked entry (the next one to be popped) on the far right; so a leaf labeled `[a2,a1]` releases `a1` before `a2`, and an internal-node PIFO `[1,2,1,2]` will yield slot indices `2, 1, 2, 1` in pop order.
A struck entry like `~1~` is one we have dropped from the PIFO.

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
The _choice of which_ `1`s to drop _silently reschedules `A` against `C`_, and nothing in `p1` or the deletion request records which the operator wanted.

We do not wish to make a scheduling decision for the operator, so our only other option is to reconstruct the tree as if `B` had never been admitted: drain the tree completely, filter the `B`-packets, and re-push, recomputing every rank and cursor along the way.
This is a stop-the-world rebuild.

Our grammar sidesteps the question entirely.
By draining `B` to empty before removing it (§4), every pop that served a packet from `B` has already removed the matching index-entries from `B`'s ancestors in the ordinary course of `pop`: a packet leaving `B` consumes a `2` at `Q` and a `1` at `P`, bringing each ancestor's count into accord with its post-drain contents.
At the instant `Remove` fires, `Q`'s `2`-count is zero (`B` is empty) and `P`'s `1`-count is exactly the number of packets still under `Q` (here `2`, all attributable to `A`).
There is nothing left to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

#### 3.4.5. `Designate(target, survivor)`

Throughout, `target : path` is the path to the subtree being wrapped, and `survivor : pol` is the designated-survivor policy that becomes the wrap's low-priority arm.

Say the presently running control is `C`.
Intuitively, `Designate` replaces `C@target` in place with `Strict*(C@target, ⌈survivor⌉)`.

`Strict*` is the same discipline as `Strict`, distinguished only by a one-bit `designated` flag that each node carries: `Designate` sets the bit, `Undesignate` (§3.4.6) clears it.
Semantically the two are identical; every push, pop, and well-formedness check treats `Strict*(A, B)` exactly as `Strict(A, B)`, so the obligations below are stated against the ordinary `Strict` semantics of §3.2.
The star exists only so that `Undesignate`'s precondition ("target lands on a `Strict*`") and a key hardware-level argument in §6 (that `Strict*` adds no PE depth) can be stated structurally.
The §3.2 DSL in which the operator writes policies can parse only `Strict`, never `Strict*`, so a `Strict*` is unreachable in any user-written `pol` and arises only in the middle of a planner sequence, between a `Designate` and its eventual `Undesignate`.
The wrap is therefore `pol`-visible only as a `Strict`, with the `designated` bit operational and not part of any `pol`.

##### Definition

`[[Designate(target, survivor)]](C)` is defined when `target` resolves to a node in `C` and `survivor`'s leaf labels are disjoint from those of `C` outside the subtree `C@target`.
Overlap with `C@target`'s own leaves is explicitly permitted: the moment the diff fires becomes the boundary between old traffic (which continues to drain from `C@target`) and new traffic (which the new arm 1 admits), so the leaves stay operationally disjoint.
Let `N` denote the freshly introduced `Strict*` node, and let `P0` denote the number of packets currently held under `C@target`.

- _Outside the subtree at `target`, outside `N`, and off the ancestor chain to `target`:_ preserved verbatim.
  If `target = π ++ [k]` is non-empty: at `π` the `node_state`, `slot_states`, and `pifo` are unchanged; the slot `k` that used to point to `C@target` now points to `N`, inheriting `C@π.slot_states[k]` (the per-arm meta is held by `π`, not by `N`, so the wrap is transparent to `π`); `z` is extended as described in the next bullet.
- _At each proper ancestor of `target` (including the root, and including `π`):_ `node_state`, `slot_states`, and `pifo` preserved verbatim.
  The local `z` is extended to admit packets that classify into the new arm-1 subtree under `N`, mapping them to whichever child slot at this ancestor lies on the path down to `N`.
  Pre-existing mappings are untouched.
  This is the exact analog of `Add`'s ancestor `z`-extensions (§3.4.1).
- _At `N`:_ `node_state = init_node_Strict()`; `slot_states = [init_slot_Strict(node_state, hi), init_slot_Strict(node_state, lo)]` for any priority ranks `hi < lo`; arm 0 is the old `C@target` (verbatim, including its entire subtree and contents); arm 1 is `⌈survivor⌉`; `pifo` is seeded with `P0` entries with slot value `0` and rank `hi` (one per packet still held under arm 0; later pushes at slot 0 would produce entries at the same rank).
  `z` routes packets by leaf label: a packet whose label exists in `⌈survivor⌉` goes to slot `1`, otherwise (label exists only in the old subtree) it goes to slot `0`.
  This rule is total on `N`'s admitted labels and resolves the overlap case toward the survivor: a label shared between the old subtree and `survivor` belongs to the survivor from the diff onward, while the residual packets under arm 0 drain on their existing entries.
- _Inside `⌈survivor⌉`:_ the local control is `⌈survivor⌉`'s, verbatim.

If `target` is empty, the entire control becomes `N`, with the old root sitting as arm 0.

##### Preservation

Preservation of `|-` holds at `N`: by construction the `0`-count in `N.pifo` equals the packet count under arm 0, and arm 1 holds no packets and no `1`-entries in `N.pifo`.
Every node inside arm 0 is preserved verbatim, so its well-formedness count is inherited.
At `π` (if `target` non-empty), `pifo` is unchanged, and the slot-`k` count `P0 + 0` matches the count that `C@target` had under `π`.
The ancestor `z`-extensions touch no `pifo` and route no in-flight packet, so they affect only `push`es that arrive after this `[[Designate(target, survivor)]]`.
Preservation of state is the standard split: arm 0 is verbatim; arm 1 is `⌈survivor⌉` by construction; `N`'s `node_state` and `slot_states` come from the listed `init` calls; everything else is verbatim.
Preservation of observation: every in-flight packet under arm 0 sits at exactly the same position it did in `C@target`, since arm 0 is `C@target` verbatim.
If `P0 > 0`, the smallest entry in `N.pifo` is `0`, so any pop that descends to `N` continues into arm 0 and returns exactly the packet a pre-diff pop would have.
If `P0 = 0`, then by `|- C` no pop ever descends to `N` (the subtree at `target` was already empty), so the question is vacuous.
Arm 1 first sees traffic only after a future `push`.

##### Characterization

```
den(Designate([],     survivor)) A      = Strict(A, survivor)
den(Designate(i :: r, survivor)) (D ts) = D ( ts[ den(Designate(r, survivor)) (ts[i]) / i ] )
```

`den` returns `Strict`, not `Strict*`: the designated bit is `pol`-invisible.
The proof is the same shape as `Add`'s: the operational rule leaves every node outside the wrap structurally intact, the new arm 1 is `⌈survivor⌉`, and the new `Strict*` reads as `Strict` at `pol`-level.
So `⌊C'⌋ =R den(Designate(target, survivor))(⌊C⌋)`.

#### 3.4.6. `Undesignate(target)`

Throughout, `target : path` is the path to the `Strict*` node being unwrapped.

Say the presently running control is `C`.
Intuitively, `Undesignate` unwraps the `Strict*(A, B)` at `C@target` whose `A`-arm is empty, leaving `B` in its slot.
It is the structural inverse of `Designate`.

##### Definition

`[[Undesignate(target)]](C)` is defined when `target` resolves to `C@target`, `C@target` carries the `designated` bit (so it is a `Strict*` introduced by an earlier `Designate`), and arm 0 of `C@target` is empty.
Let `N = C@target`.

- _Inside `B` (arm 1 of `N`):_ preserved verbatim, every node.
- _At `π`_ (if `target = π ++ [k]` is non-empty): `node_state`, `pifo`, and `z` unchanged; `slot_states[k]` preserved verbatim (the wrapper inherited it at `Designate` time, and the unwrap returns it unchanged); slot `k` now points to `B` rather than to `N`.
  `N` itself and its arm-0 stub are discarded.
- If `target` is empty, the new root is `B`, inheriting `B`'s own `node_state`, `slot_states`, `pifo`, `z`, and child list verbatim; `N` and its arm-0 stub are discarded.

##### Preservation

Preservation of `|-`: at `π`, `pifo` is unchanged; slot `k`'s count was `0 + |B|` under `N` and is now `|B|` under `B` directly, so the count matches.
Inside `B`, every node is verbatim.
The discarded `N.pifo` was carrying its own count of arm-1 entries one level too deep: by `|- C` and the empty-arm-0 precondition, that count equals `|B|`, which is exactly what `π.pifo`'s `k`-entries already carry one level up.
After the unwrap, those `k`-entries route directly to `B`, whose pifo count is still `|B|`, so the well-formedness obligation at `π` reads `|B| = |B|` and inside `B` reads as before.
The previously-redundant routing step ("`Strict*` says go to arm 1," which arm 0 being empty had already forced) is the only step elided.
Preservation of state: standard verbatim everywhere outside `N`; `N`'s local state vanishes.
Preservation of observation: arm 0 holds no packet by precondition, so `Strict*`'s "favor arm 0" behavior was already routing every pop to arm 1, and the unwrap preserves this routing exactly.

##### Characterization

```
den(Undesignate([]))     (Strict(A, B)) = B
den(Undesignate(i :: r)) (D ts)         = D ( ts[ den(Undesignate(r)) (ts[i]) / i ] )
```

The base case consumes the `Strict` wrapper (which on the `pol` side is what the `Strict*` reads as).
Proof shape is `Remove`'s: outside `N` nothing structural moves, and at `N` the `Strict` wrapper is dropped.
So `⌊C'⌋ =R den(Undesignate(target))(⌊C⌋)`.

#### 3.4.7. `ChangeRoot(newroot)`

Throughout, `newroot : path` is the path to the subtree being promoted to the new root.

Say the presently running control is `C`.
Intuitively, `ChangeRoot` promotes `C@newroot` to the new root, discarding every ancestor above it.
It is the only production that erases structural ancestors, and the only one whose `pol`-level effect is to shrink the tree from above rather than edit it locally.
The mechanics are short; the notes below are substantive.

##### Definition

`[[ChangeRoot(newroot)]](C)` is defined when `newroot` is non-empty, resolves to a node in `C`, and every internal node strictly above `C@newroot` is _unary_, with its sole arm continuing toward `newroot`.
Under these preconditions the discarded ancestor chain carries scheduling metadata but no off-path traffic.

- _At `C@newroot` and inside its subtree:_ preserved verbatim.
  The result `C'` is exactly `C@newroot` standing alone as a tree.
- _At every proper ancestor of `newroot`:_ discarded entirely, together with the local `node_state`, `slot_states`, `pifo`, and `z`.

##### Preservation

Preservation of `|-`: `|- C` is a conjunction of per-node well-formedness obligations, and the subset of those obligations attached to `C@newroot`'s nodes constitutes `|- C@newroot`, which is exactly `|- C'`.
The discarded ancestors' obligations are simply forgotten.
Preservation of state: standard verbatim within `C@newroot`; ancestor state is gone.
Preservation of observation: every in-flight packet sits in some `pifo` within `C@newroot`'s subtree (the ancestor `pifo`s held routing entries, not packets), all preserved verbatim.
A pop immediately after this `[[ChangeRoot(newroot)]]` returns exactly what a pop immediately before would have: a pre-diff pop descends through the unary chain by popping each ancestor's PIFO in turn, and since each such PIFO has a single slot (slot `0`), the popped entry is forced to be a slot-`0` entry regardless of its rank; the descent therefore lands at `C@newroot`'s root and pops from there.
A post-diff pop acts on `C@newroot`'s root directly, returning the same packet.

##### Characterization

```
den(ChangeRoot(newroot)) p = p@newroot
```

is defined whenever `newroot` is non-empty and resolves in `p`.
The realizes-relation propagates structurally: if `C` realizes `p`, then `C@newroot` realizes `p@newroot`, since compilation is per-node and the discipline at each surviving node is unchanged.
So `⌊C'⌋ =R p@newroot = den(ChangeRoot(newroot))(⌊C⌋)`.

##### Notes

_Why the unary precondition._
The precondition rules out, by construction, any silently-dropped packet-bearing siblings.
A non-unary ancestor would have arms branching off the path to `C@newroot`; those subtrees would vanish with their ancestor, taking their packets with them.
Allowing this would make `ChangeRoot` a structural deletion of arbitrarily many off-path subtrees masquerading as a root change, with no semantic story for those packets.
The richer reconfiguration of pruning to a subtree whose ancestor chain branches off into packet-bearing relatives is realized as the `PruneDownTo` idiom (§4.1), which first drains and `Remove`s those siblings in sequence, reducing the chain above `C@newroot` to the unary shape that `ChangeRoot` then accepts.

_What the chain carries, and what is discarded._
An internal node's `node_state`, rank function, and `pifo` ordering exist to pick among siblings.
At a unary node the sibling-picking role degenerates: with one arm, every pop is forced into it.

What can remain active at a unary node is rank computation that depends on per-packet attributes rather than on sibling identity.
If a chain's disciplines do no such per-packet reordering, each unary node along the chain is operationally a passthrough, and the immediate-pop observation-preservation above extends to every subsequent pop and push.
If they do (LSTF is a canonical example, with rank determined by each packet's deadline), the chain is actively shaping traffic, and `ChangeRoot` removes that shaping.
That is precisely the production's intended role: the atomic step by which the operator says "promote `p1@newroot` to the root and discard the ancestor shaping above it."
The `PruneDownTo` idiom packages this with the upstream draining and `Remove`s that make the chain unary in the first place, so the operator's request ("prune to this subtree") is realized as a sequence whose final step discards exactly the ancestor influence that the operator has chosen to abandon.

_Pol-level effect._
The `pol` changes from the unary chain wrapping `p1@newroot` (e.g., `Strict(p1@newroot)` or `LSTF(p1@newroot)`) to just `p1@newroot`.
This is `pol`-visible: the root discipline observably changes, which is the substantive content of the production.
Whether that pol-visible change reorders in-flight traffic depends on whether the discarded chain was doing per-packet reordering (see previous Note).

#### 3.4.8. `Graft(ctx)`

[AM TODO: Anshuman wants to discuss this IRL.]

### 3.5 Preserving this proof down to hardware

§3.4 proves soundness at the tree-diff level, where each atomic edit (`Add`, `Quiesce`, `Remove`, ...) carries a well-formed tree to a well-formed tree.
To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR.
The IR alphabet and the substrate machinery are the subject of §6; here we only need that the IR has fine-grained verbs (we will name `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, and the like) and that a single such verb, unlike a whole tree-diff edit, _can_ leave the tree malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, for example.

We do not prove soundness at the IR level, but instead informally make the case for why the §3.4 proof survives the lowering.
There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit.
  We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed.
  That commit is precisely how the substrate _realizes_ the atomicity property of §3: atomicity asked for an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant.
  Every `push`/`pop` therefore still lands on a well-formed control (`p1`, a `link`, or `p2`), exactly as §3.4 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.4 proved.
The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §6.

[AM note for Zhiyuan: the §3.5 argument leans on the substrate supporting _atomic transactional commits_: a multi-instruction lowering must install as a single instant from the user's perspective, so that the transiently-malformed intermediate trees inside a commit are never observed.
Our own substrate provides this.
The open question for §6 is whether composition with a third-party substrate (e.g., vPIFO) requires the same property and, if so, whether vPIFO offers it.
Flagged here so the §3.5 claim "the proof survives the lowering" is not read as substrate-independent.]

## 4. Realizing Reconfigurations as Sequences

This section composes the atomic diffs of §3 into _guarded sequences_ `(φ ; δ)*`, where a guard `φ` is a predicate on the state of the live control and a diff `δ` is a production of the grammar from §3.3.
Each `δ` fires as soon as its guard becomes true.
Sequences realize changes no single diff can express.

A guarded sequence threads the live control through a chain of intermediate controls, one per pair of consecutive diffs.
We call each such intermediate control a `link`, writing `link_i` for the control left behind by the `i`-th diff.

The headline result of the section, proved below, is that each `link` is itself an ordinary §3.1 control, so the "transitionary period" needs no new semantics: this is Obligation 1 of §1.
Moreover, since every production of §3 is _pol-explainable_ (each `δ` has a `den(δ)` that tracks its pol-level effect), we can echo to the operator, at every step of the sequence, the `pol` that the live control currently realizes.
The chain of `den`s starting from `⌊C⌋` gives `⌊link_i⌋` for each `i`, so the operator is never in the dark about what is running.

### 4.1 Guarded Sequences

##### Grammar

```
φ      ::= true                   // trivially satisfied
         | empty(path)            // the subtree at `path` holds no packets

gseq   ::= ε                      // empty sequence
         | (φ ; δ) ; gseq         // a guarded atomic diff, then more
         | I ; gseq               // an idiom invocation, then more

I      ::= Retire(path)
         | SlowRetire(path)
         | Replace(path, pol)
         | PruneDownTo(path)
```

We write `gseq` short for "guarded sequence."
A guard is always paired with exactly one atomic diff `δ` drawn from §3.
An idiom `I` appears in a `gseq` _without_ a guard because an idiom expands into a `gseq` of its own whose internal guards carry the synchronization (§4.2).
For instance, §4.2's `PruneDownTo` expansion reads

```
Retire(π_a) ; ... ; Retire(π_z) ; (true ; ChangeRoot(path))
```

mixing bare idiom invocations with one explicitly-guarded atomic diff.
After idiom expansion (§4.2), every step is a `(φ ; δ)` pair, and that is the form on which the rest of §4 reasons.
When we write `(φ ; δ)*` informally we mean this fully-expanded form: a finite sequence of `(guard, atomic diff)` pairs.

Having just those two guard forms has sufficed for every sequence we have needed; we take the minimality as a small design win.
The framework does not depend on it, and a richer predicate language can be slotted in without disturbing the rest of §4.

##### Sequence semantics

The grammar above gives a `gseq` its shape; this subsection gives it operational meaning.
§3 wrote `C` for the pre-edit control and `C'` for the post-edit one; here we generalize from a single-step replacement to a chain of steps.
A _transition planner_ realizes a reconfiguration from `C` to `C'` as a guarded sequence

```
(φ_0 ; δ_0) ; (φ_1 ; δ_1) ; ... ; (φ_n ; δ_n).
```

Write `link_0 = C` and `link_{i+1} = [[δ_i]](link_i)` for `0 <= i <= n`, with `link_{n+1} = C'`.
Pairs fire in order: `link_i` runs and serves ordinary pushes and pops until `φ_i` becomes true on its state, at which instant `δ_i` fires and produces `link_{i+1}`; only then is the next pair `(φ_{i+1}; δ_{i+1})` in play.
Crucially, `φ_{i+1}` is evaluated on `link_{i+1}`, which exists only after `δ_i` has fired, so the sequence is genuinely sequential and not a set of independent guards racing on the same control.

A guard may be `true`, in which case `δ_i` fires the moment `link_i` is installed, with no waiting; but a `true` later in the sequence is still gated by every preceding pair.
For instance, the closing `(true ; ChangeRoot(path))` of §4.2's `PruneDownTo` is nominally guarded by `true` but in the global timeline cannot fire until every preceding `Retire` has emptied its target and run its `Remove`: the `true` says "install at the moment the predecessor's link is installed," not "install at sequence start."
A length-one sequence with `φ_0 = true` is a single atomic diff applied immediately, with `C` abutting `C'` and no intervening link.
The empty sequence is the case `⌊C⌋ =R ⌊C'⌋`: the live control is left untouched.

##### Safety

Every sequence the planner emits is safe in the sense that the live scheduler is well-formed at every instant.
`|- link_0` by assumption.
Each `δ_i` preserves `|-` by the per-production obligation discharged in §3.4.
Each `link_i` itself preserves `|-` under ordinary `push`/`pop`, since §3.1's `push` carries a well-formed PIFO tree to a well-formed one and `pop` does likewise on non-empty inputs.
So the user-observable trace is a stream of ordinary `push`/`pop` operations served in turn by well-formed controls `C, link_1, ..., link_n, C'`.
Each intervening `link_i` is therefore an ordinary §3.1 control, and the "semantics of `link`" is nothing more than the §3.1 semantics of the controls we install: each diff's job is to be a sound atomic replacement, and the planner's job is to order them behind sensible guards.
Safety is entirely local: it follows from the soundness of the individual diffs, with no global argument about the sequence.
This discharges Obligation 1 of §1.

##### Liveness

A sequence reaches `C'` only if each `φ_i` eventually becomes true on `link_i`, and safety says nothing about that.
A `Quiesce`d subtree may never drain if higher-priority siblings starve it; a guard `empty(path)` on a subtree fed by an adversarial higher-priority neighbor may never fire.
We therefore treat liveness as a _nice-to-have_, the planner's and the operator's joint concern, not a soundness condition.
Every sequence we emit is safe; whether and how fast it completes is a function of how well the planner chose its guards and of the traffic the live scheduler meets.
For the case of follow-up requests arriving mid-flight, see §4.4.

### 4.2 Idioms: Named Sequences

_Idioms_ are an operator-facing vocabulary for frequently-used guarded sequences.
They are the typical building block of imperative-mode sequences (§4.3).

An idiom is a macro over the diff grammar (and, recursively, over other idioms).
It expands into a fixed `(φ; δ)*` sequence: a list of atomic diffs with the guards between them spelling out what the system waits for.
Soundness is compositional: each step of the expansion is sound by §3.4, and the sequence inherits the §4 sequence-level reasoning above.
An idiom expansion that would hit an undefined production on the current control is rejected: the soundness checks fire on the expanded sequence just as they would on a hand-written one.

We name four starter idioms.
New ones can be added later without changing the framework, since an idiom is just a named `(φ; δ)*` shorthand.

- **`Retire(path)`** = `(true; Quiesce(path)) ; (empty(path); Remove(path))`.
  Quiesces the subtree at `path`, waits for it to drain, then `Remove`s it.
  The operator-facing way to say "tear this subtree down gracefully."

- **`SlowRetire(path)`** = `(empty(path); Remove(path))`.
  Waits for the subtree at `path` to drain, then `Remove`s it.
  The operator may use this if the subtree at `path` needs to receive a little more traffic but is generally being phased out.

- **`Replace(path, B)`** =

  ```
  (true; Designate(path, B)) ;
  (true; Quiesce(path ++ [0])) ;
  (empty(path ++ [0]); Undesignate(path))
  ```

  Designates `B` as the survivor of the current `pol@path` (which, after Designate, sits at `path ++ [0]` as the first arm of the inserted `Strict*` node), quiesces it, waits for it to drain, then collapses the `Strict*` onto `B`.
  At the pol level the composite effect is wholesale replacement of `pol@path` by `B`, computed as `den(Undesignate) ∘ den(Designate(_, B))` against the §3.4 productions; the operator-facing idiom adds the `Quiesce` + drain in the middle so that the original subtree empties out before the collapse fires.

- **`PruneDownTo(path)`** = `Retire(π_a) ; ... ; Retire(π_z) ; (true; ChangeRoot(path))`, where `π_a, ..., π_z` are paths to the off-path subtrees along the route from the root to `path`.
  This idiom is the operator-facing way to say "abandon everything except this subtree."

  Each `Retire` removes one off-path subtree; once they have all fired, every ancestor along the path is unary, satisfying `ChangeRoot`'s precondition (§3.4.7).
  (If no off-path subtrees exist, the idiom reduces to `(true; ChangeRoot(path))` alone.)

  For instance, suppose the running tree is `A(B(E(F, G), D), C)`:

  ```
  A
  ├── B
  │   ├── E
  │   │   ├── F
  │   │   └── G
  │   └── D
  └── C
  ```

  The index paths to its nodes are:
  - `A` at `[]`
  - `B` at `[0]`
  - `C` at `[1]`
  - `E(F, G)` at `[0, 0]`
  - `F` at `[0, 0, 0]`
  - `G` at `[0, 0, 1]`
  - `D` at `[0, 1]`

  The operator wants to retain only `D`, so they issue `PruneDownTo([0, 1])`.

  The off-path subtrees are `E(F, G)` (at `[0, 0]`) and `C` (at `[1]`).
  So `PruneDownTo([0, 1])` expands to: `Retire([0, 0]) ; Retire([1]) ; (true; ChangeRoot([0, 0]))`.

  Note that the path `[0, 0]` appears twice in this expansion with different referents: in `Retire([0, 0])` it points to `E(F, G)` (the operand at the moment that `Retire` fires), and in the final `ChangeRoot([0, 0])` it points to `D`, which moved from `[0, 1]` to `[0, 0]` once `E(F, G)` was retired.
  The path-resolution system computes these targets for the operator; the operator only writes `PruneDownTo([0, 1])`, naming `D` by its location at the moment of request.

### 4.3 Authoring modes

Two authoring modes produce sequences, and the operator chooses freely between them.

- In _declarative mode_, the operator writes a `pol`, say `p1`, and, to reconfigure, writes a second `pol`, say `p2`.
  A differ (§5) proposes a guarded sequence that achieves this change, and the operator either accepts it (in which case we apply the sequence to the running control) or declines it.
  The differ is intentionally simple: it sees only `pol`-level diffs whose translation to a sequence is straightforward, and falls back to the generic `Designate([], p2) ; Undesignate([])` pair (§5) for anything richer.
- In _imperative mode_, the operator writes both their desired `p2` and a `(φ; δ)*` sequence intended to reach it; the sequence may include idioms from §4.2.
  Before running it we check the sequence at the pol level, as described next.

The two modes are not formally distinct: they produce sequences over the same substrate that discharge the same per-production obligations from §3.4.
Imperative mode buys expressivity, not a different proof obligation; it admits sequences the differ might never produce, but they are still `(φ; δ)*` sequences.

##### The pol-level check on imperative sequences

We fold each diff's `den(δ_i)` (§3.4) along the sequence, producing a chain

`p1' -[den(δ_1)]-> ip_1 -[den(δ_2)]-> ip_2 -[den(δ_3)]-> ... -[den(δ_n)]-> ip_n`

of _intermediate pols_ starting from the running representative `p1' = ⌊C⌋` (the pol we echoed to the operator), and accept the request iff every `den(δ_i)` is defined on the intermediate pol it sees (with `ip_0 := p1'`) and `ip_n =R p2`.
That is, the user-provided sequence actually takes `p1'` to `p2`.
This check is exactly Rule 3 of `⌊·⌋` (§3.2) chained across the sequence: each step's `den(δ_i)` was proved to match its operational counterpart `[[δ_i]]` per-production in §3.4, so a fold that lands at `ip_n =R p2` certifies that the live operational chain ends in a control whose `⌊·⌋` is `p2`.

Each `ip_i` is itself a valid pol with an explainable scheduling semantics, not a transient artifact of the proof.
For instance, after the first step of `Replace(path, B)`'s expansion the intermediate pol holds `Strict(pol@path, B)` at `path`: a temporary strict-priority node favoring the outgoing `pol@path` over the incoming `B`, with a perfectly readable scheduling interpretation in its own right.

The "defined when" clauses on each `den(δ_i)` are the per-production preconditions listed in §3.4.1-3.4.8.
Guards play no role in this check; they govern the operational timing of when each `δ_i` fires on the live control, and are pol-invisible (§3.4).
For instance, a `(empty(path); Remove(path))` step folds at the pol level exactly as if its guard were `true`, since `den` only sees `δ`.
If the chain fails to reach `p2`, or some `den(δ_i)` is undefined on its intermediate pol, we reject the request.

### 4.4 Handling follow-up requests

This paper's transition planner engages with one reconfiguration at a time, and commits to a simple answer when the operator submits a follow-up request `p3` while a `p1 -> p2` sequence is still mid-flight (i.e., while some guard `φ_i` has not yet become true): we queue `p3`, and the planner does not begin work on it until the in-flight sequence completes, which is to say until the live control `C_z` satisfies `⌊C_z⌋ =R p2` (equivalently, until the echoed `p2'` is the running pol).
At that instant the planner pulls `p3` from the queue, treats `p2'` as the new starting point, and produces a fresh `(φ; δ)*` sequence to reach `p3` exactly as in §4's main loop.

We commit to this rather than defaulting to it.
A more aggressive strategy is possible: the planner could begin work on `p3` mid-flight, splicing or canceling the in-flight sequence to converge on `p3` directly, sometimes at lower total cost than running `p1 -> p2 -> p3` in series.
We do not pursue this here, because the splicing analysis introduces its own correctness obligations (atomicity of the splice, what guarantees the operator has during an aborted in-flight sequence, what `link` the operator observes between the splice and `p3`) that are out of scope of this paper.
See https://github.com/cucapra/packet-scheduling/discussions/115 for a sketch of the stronger possibility.

## 5. Identifying Better Transitions

§3 gave us a toolkit of atomic diffs and §4 sequenced them through `link`s into full reconfigurations.
With those tools in hand, this section asks how the transition planner can wield them well.
The metric we adopt is _confinement_: a good sequence is one whose diffs and intervening `link`s disturb as little of the running scheduler as possible, leaving the parts of the tree that did not need to change running undisturbed.

There is always a maximally unconfined fallback.
To reach any `p2` from any `p1`, the planner can issue `Designate([], p2)`, making the whole of `p2` the survivor of the whole of `p1`.
All new traffic flows to `p2` at once, every `pop` is served by `p1` until `p1` drains, and a closing `Remove` discards `p1` and leaves `p2`.
Nothing is dropped and the sequence is safe by §3.4, but no part of the scheduler is left undisturbed.
This is the worst case the planner ever falls back on.
The rest of §5 is about doing better: localizing the change so that the sequence and its `link`s touch only a small subtree, while the rest of the scheduler keeps running undisturbed.

We make no claim that the planner is canonical or minimal.
We claim only that whatever sequence it emits is safe (§3.4) and no worse than this fallback.

[AM note: Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself.
TK.]

## 6. Compiling to Hardware

### 6.1 The commit model

The substrate exposes its services to the planner as an atomic-commit interface.
The ISA has exactly one unit: a `commit`, which is a list of ISA instructions (§6.2) that the substrate installs _atomically_.
The entire list lands between two consecutive `push`/`pop` operations, with no intermediate state visible to the user.
This is precisely the atomicity that §3.4's _Preservation of observation_ obligation demanded.

Each §3.3 atomic diff `δ` lowers to one `commit`; that is the entire ISA-level contract.
The ISA itself is unconditional: each instruction describes what to do, not what to check.
The substrate trusts that its caller has issued a well-formed commit and acts accordingly.
Any situation-dependent reasoning (e.g., "the subtree we are about to collapse is empty") lives at §4, in a guard `φ` that gated the surrounding diff's firing, not in the substrate.

§4 sequences `(φ ; δ)*` are an orchestration concern, not an ISA concern.
The planner walks the sequence, observes live control state, and dispatches the lowering of `δ_i` to the substrate once `φ_i` becomes true; it then waits for `φ_{i+1}` to become true before dispatching the next commit.

### 6.2 The ISA

Four pieces of substrate vocabulary appear in the listing below:

- **lPIFO** ("logical PIFO"): one PIFO-tree node, addressed by an opaque id `v`.
- **PE** (processing element): hosts one or more lPIFOs and owns their per-node state and logic.
- **flow**: a traffic class.
- **index**: opaque per-lPIFO handle naming one of that lPIFO's children; what an lPIFO uses internally to refer to a child arm.

The ISA has twelve opcodes:

| Opcode                       | Parameters                | Effect                                                                                                                                                                                                                                                                                                                                                                       |
| ---------------------------- | ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Spawn(v, pe)`               | fresh lPIFO id, PE id     | Allocate an empty lPIFO `v` on PE `pe`.                                                                                                                                                                                                                                                                                                                                      |
| `Adopt(i, p, c)`             | index, parent, child      | Parent `p` gains `c` as a child, reachable via index `i`.                                                                                                                                                                                                                                                                                                                    |
| `Emancipate(i, p, c)`        | index, parent, child      | Inverse of `Adopt`: detach `c` from `p`; `i` was the index used to reach `c`.                                                                                                                                                                                                                                                                                                |
| `Assoc(v, f)`                | lPIFO, flow               | `v` begins to accept packets of flow `f`.                                                                                                                                                                                                                                                                                                                                    |
| `Deassoc(v, f)`              | lPIFO, flow               | `v` stops accepting packets of flow `f`.                                                                                                                                                                                                                                                                                                                                     |
| `Map(v, f, i)`               | lPIFO, flow, index        | In `v`'s brain, route flow `f` to index `i`.                                                                                                                                                                                                                                                                                                                                 |
| `Unmap(v, f, i)`             | lPIFO, flow, index        | Forget `v`'s flow-`f`-to-index-`i` entry.                                                                                                                                                                                                                                                                                                                                    |
| `Change_pol(v, t, n)`        | lPIFO, policy type, arity | Set `v`'s policy to `t` with `n` arms; `t` ranges over {FIFO, RoundRobin, Strict, WFQ, Union}. Shrinks drop the rightmost slots and discard their per-arm metadata; grows add fresh slots at the right with uninitialized metadata, which must be set by a subsequent `Isa_change_weight`. Slots at indices `< min(old, new)` keep their child pointer and per-arm metadata. |
| `Isa_change_weight(v, i, w)` | lPIFO, index, new weight  | The child reached via `i` now carries weight `w`.                                                                                                                                                                                                                                                                                                                            |
| `Isa_designate(v, surv)`     | two lPIFOs                | Form the super-node `{v -> surv}` in `v`'s slot: pops favor `v`; the substrate records `surv` as `v`'s designated successor for an eventual `Isa_undes`.                                                                                                                                                                                                                     |
| `GC(v)`                      | lPIFO                     | Release `v`'s PE slot.                                                                                                                                                                                                                                                                                                                                                       |
| `Isa_undes(v)`               | lPIFO                     | Collapse the super-node `{v -> surv}` to `surv`: the parent index that pointed at `{v -> surv}` now points directly at `surv`, and `surv` inherits the slot's per-arm metadata.                                                                                                                                                                                              |

Each opcode performs exactly one job, even where the planner uses two of them together. `Isa_undes(v)` only rewires the parent's index, leaving `v` itself allocated; the planner pairs it with `GC(v)` (in the same commit) to reclaim `v`'s slot.
The clean separation lets the substrate avoid checking live state to figure out what the planner meant: consistent with the unconditional ISA style (§6.1), the planner has already discharged any "is `v` actually empty?" / "is `v` actually the top of a super-node?" question via a §4 guard `φ` that gated the surrounding diff.

### 6.3 Lowering atomic diffs

Each §3.3 diff lowers to an `instr list`.
The lowering _schema_ is mechanical: each diff names a fixed sequence of opcodes, and the planner instantiates the schema's positional parameters (flow lists, chain shapes, routing indices) from live state at issue time.
The substrate itself still sees only a finished commit and does no live-state checking, per §6.1.

Before working through the diffs, we set up one structural convention.
A switch with `P` output ports has `P` separate trees, and at the ISA we put a uniform thin wrapper around each.
Every port hosts a reserved lPIFO `port_root`, allocated at boot on a reserved PE, whose sole child is the actual tree root.
`port_root` runs a fixed 1-arm policy. Its `Assoc` set mirrors that of the live tree's actual root; its `Map` sends each Assoc'd flow through its single index `port_step`. Both are maintained automatically by the §6.3 walks (which start at `port_root`); only `ChangeRoot` and `Graft` rewire `port_root`'s child.
The wrapper exists so that "swap the root" can be expressed as one `Emancipate`/`Adopt` pair against `port_root`, in the same vocabulary as any child-level edit; there is no opcode for changing the root directly.

The lowerings follow, one per diff in the order of the §3.3 grammar.
We fix three reading conventions for the list below.
First, we identify a tree position with the lPIFO that lives there.
Second, when the lowering hinges on the relation between a target and its parent, we destruct the diff's path as `π ++ [k]` (reusing §3.4's convention: `π` is the parent's path, `k` is the local index by which that parent reaches the target).
Third, we use shorthand for the chain walks that recur across entries.
Write `flows(path)` for the set of flows admitted by some leaf under `path`.
Write `chain(f)` for the unique sequence of lPIFOs from `port_root` down to the leaf admitting `f`, and `internals(f)` for that sequence minus the leaf.
Write `walk(op, C, f)` for issuing `op(v, f, ...)` at each `v ∈ C` in chain order; positional remainders such as `Map`'s routing index `i_{v,f}` are read off the live tree.
When an entry's opcodes name an unmentioned parameter at `π` (e.g., `Change_pol`'s discipline `t` or arity `n`), it is the live value at `π` at the time the command is issued.
`Spawn`'s `pe` argument is similarly elided in the entries: it is `pe(path)` for the new lPIFO's position, computed from the §6.1 deployment convention.

- `ChangeWeight(π ++ [k], w)`:
  `Isa_change_weight(π, k, w)`.
- `Quiesce(path)`:
  for each `f ∈ flows(path)`, `walk(Deassoc, chain(f), f)`.
- `Add(path, pol, meta?)`:
  - Let `n` be `path`'s current arm count; the new subtree will occupy `path`'s rightmost slot, index `n`, so existing children's indices are undisturbed.
  - Lay out the subtree `pol` in hardware. That is, for each new lPIFO in the subtree compiled from `pol`, `Spawn`+`Adopt`+`Change_pol` is issued. Per-arm `Isa_change_weight`s are issued where the discipline requires them.
  - `Change_pol(path, t, n+1)` grows `path` to arity `n+1`; where the discipline requires it, `Isa_change_weight(path, n, w)` initializes the new slot's metadata from `meta?`.
  - `Adopt(n, path, root_of_pol)` attaches the new subtree at index `n`.
  - Start serving traffic to the new subtree. That is, for each `f ∈ flows(pol)`: `walk(Assoc, chain(f), f)`; `walk(Map, internals(f), f)`.
- `Remove(π ++ [k])`:
  - `Emancipate(k, π, π ++ [k])` detaches the doomed child.
  - Shift higher-indexed siblings down to fill the gap. For each `j ∈ {k+1, ..., n-1}` in increasing order, let `child_j` be the live child at slot `j`: `Emancipate(j, π, child_j); Adopt(j-1, π, child_j)`; where the discipline keeps per-arm metadata, `Isa_change_weight(π, j-1, w_j)` carries slot `j`'s live metadata along. [AM note: this is a little crazy! Zhiyuan, possible to make do without this massive emancipate-adopt cascade?]
  - Rewrite `π`'s flow-to-index Map for the same shift: for each `j ∈ {k+1, ..., n-1}` and each `f ∈ flows(π ++ [j])`, `Unmap(π, f, j); Map(π, f, j-1)`. [AM note: this is a little crazy! Zhiyuan, possible to make do without this massive emancipate-adopt cascade?]
  - `Change_pol(π, t, n-1)`.
  - For each `f ∈ flows(π ++ [k])`, `walk(Unmap, internals(f), f)` clears the doomed flows' routing through the chain above `π ++ [k]` (including `π` itself, whose `f → k` entry vanishes).
  - One `GC` per lPIFO in the now-detached subtree.
- `Designate(path, pol)`:
  - Stand up `pol`'s subtree as in `Add`, except that the subtree's root `surv_root` is left unattached.
  - `Isa_designate(path, surv_root)` attaches the subtree's root via the super-node trick.
  - Serve traffic to the designated survivor.
    For each `f ∈ flows(surv_root) \ flows(path)`: `walk(Assoc, chain(f), f)`; `walk(Map, internals(f), f)`.
    Flows in `flows(surv_root) ∩ flows(path)` need no further wiring: the above-super-node chain is already in place from `path`'s prior wiring, and the inside-`surv_root` chain was wired during the "stand up" step.
  - A partial Quiesce-style silencing restricted to flows absent from the new policy.
    That is, for each `f ∈ flows(path) \ flows(surv_root)`: `walk(Deassoc, chain(f), f)`; `walk(Unmap, internals(f), f)`.
- `Undesignate(path)`:
  let `v` be the retiring arm of the super-node formed at `path` by the prior `Designate`.
  `Isa_undes(path)`; one `GC` per lPIFO in the now-detached subtree rooted at `v`.
- `ChangeRoot(path)`:
  let the live tree be `port_root -> a_0 -> a_1 -> ... -> a_m -> path` (the §3.3 restriction makes `a_0 .. a_m` a unary vine).
  `Emancipate(port_step, port_root, a_0)`; `Adopt(port_step, port_root, path)`; `GC(a_0), GC(a_1), ..., GC(a_m)`.
- `Graft(ctx)`:
  let `prev_root` be `port_root`'s current child (the live tree's actual root) and `new_ctx_root` be the root of `ctx`'s compiled subtree.
  Compile `ctx` exactly as in `Add`, with one modification at the hole: instead of spawning a fresh subtree for the hole, feed the hole's `Adopt` the existing `prev_root`'s id directly.
  Change what `port_root` points at: `Emancipate(port_step, port_root, prev_root)`; `Adopt(port_step, port_root, new_ctx_root)`.

Three items in the list deserve a sentence of unpacking.

`Quiesce`'s wide reach (root-to-`path` chain, plus the subtree under `path`) is forced by §3.2's parallel push: every node on a packet's path mints its own routing index independently, so dropping `f` only at a strict subset would leave the rest happy to mint stray indices that would leave the tree malformed.
The chain walk is what §3.4.3 calls _restricting `z` uniformly_.
Quiesce stops short of `Unmap`: silenced flows' Map entries must outlive the silencing so packets already pushed can route through the chain on the way to pop. `Remove` pairs `Emancipate` with `Unmap` precisely because it fires after the drain, when there is no longer any in-flight traffic to preserve.

`Designate`'s `Isa_designate(path, surv_root)` performs no edit on `path`'s parent: the super-node `{path -> surv_root}` reuses `path`'s slot, so the parent's index, weight, and per-arm metadata are untouched.
This is what lets the pre-existing per-arm metadata at `path` continue to apply after the give-up; the substrate consults `surv_root` only on a later `Isa_undes`.

`ChangeRoot` and `Graft` are the only diffs that touch `port_root`.
In both cases the root swap is exactly one `Emancipate`/`Adopt` pair against `port_root`; the rest of the commit is the diff-specific build-up or tear-down of the surrounding structure.

### 6.4 The super-node gadget (TBD)

### 6.5 Substrate portability (TBD)

---

The remainder of this section is scaffolding from earlier passes; it will be reorganized into §§6.2-6.5 above.

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
