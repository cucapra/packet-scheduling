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
At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself.
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
The grammar carries no structural mark of any of this: the operator writes the metadata in the surface syntax (e.g., `WFQ(w_1: pol_1, ..., w_n: pol_n)`, or positional sugar like `Strict(A, B)` which desugars to `Strict(hi: A, lo: B)`), but the metadata lives in the arm's `slot_state` once compiled (see `init_slot_D` below) and `push`/`pop` can read it but not modify it.
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
To this end: `init_slot_RR` returns the empty tuple (no per-arm bookkeeping to seed); `init_slot_Strict(_, p) = p` (the arm's `slot_state` is just its priority rank, drawn from a dense total order such as the rationals so that a fresh priority can always be slotted strictly between two existing ones).
For `WFQ`, the `node_state` at a parent is the virtual time `vt`, and we set `init_slot_WFQ(vt, w) = (w, vt)`: a new arm carries its weight `w` and inherits the parent's current `vt` as its last-finish tag.
This is enough: standard WFQ will assign the first packet on this arm a tag of `max(virtual_clock, vt) + 1/w`, which slots it into the round that the established arms are currently in.
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
  The shape of that segment differs between internal nodes and leaves:
  - at an internal node, `z : state × Pkt ⇀ (idx × rank) × state`: pick a child index `i` and the rank `r` with which to enqueue `i` at this node's index-PIFO;
  - at a leaf, `z : state × Pkt ⇀ rank × state`: pick the rank `r` for the packet's own PIFO entry.

  When `z` is undefined for a packet, the per-node action is empty: nothing is enqueued at this node and `state` is unchanged.
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
The rest of the paper has no need for gluing a control together in this way (`|- C` is stated directly per the previous paragraph, and the diff rules of §3.4 act node-locally), but a reader more at home in FA's framing can recover it in this way.

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
3. _Closure under diffs._ Each grammar diff `δ` (§3.3) has two readings, both defined in §3.4: an operational rewrite on the live control, written `[[δ]] : control ⇀ control`, and a closed-form pol-level effect, written `den(δ) : pol ⇀ pol`. Rule 3 says the two readings agree: `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`. The `=R` slack is needed here because the operational rewrite has arm-order freedom, just as the compiler does.

The three rules together let us propagate `⌊·⌋` from any `⌈p⌉` along any sequence of pushes, pops, and diffs.
This is how we will discharge Obligation 1 of §1: telling the operator what `pol` is running even when no user has explicitly requested the `pol`.

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
`C2 := ⌈p2⌉` is the control we _would have built_ had we taken the SOTA stop-the-world path.
We do not actually construct it, but it is the correct reference point and it is crucial that we now relate `C2'` (which we have just produced after a fashion) to `C2` (which SOTA would have produced).
By rule 1, `⌊C2⌋ =R p2`, hence `⌊C2'⌋ =R ⌊C2⌋`.
But we would like to relate the controls directly, not just their `pol`-level projections.
The relation we write is `C2' ~R C2`, which absorbs two gaps at once:

- `C2'` carries the live `pifo`/`state` accumulated since `C1`, while `C2` is freshly compiled and bare. The `~` component covers this.
- `[[δ]]` and `⌈·⌉` are free to pick different sibling orders at internal nodes, so `C2'` and `C2` may also differ in child arrangements. The R-closure covers this.

The diagram thus commutes at the level of `⌊·⌋` modulo `=R`: both routes from `p1` to a control realizing `p2` land in the same `~R`-class.
The commutation holds verbatim for `pol`-visible diffs and informally for pol-invisible ones like `Quiesce` (whose `z`-domain restriction is not reachable by any push or pop of `C2`).

When writing `p2`, the operator would do well to state their request against `p1'`, the actually-running `pol` that we echoed back to them.
This keeps the diff small and aligns paths with what is running.
The operator may state `p2` against the unprimed `p1` they originally wrote, but at their own risk.
The risks are of two flavors:

- The diff sniffer may infer a larger diff than necessary, or may give up.
- The more serious issue is that the user may use Imperative Mode (§4) to directly specify what edits to make, and if they use the unprimed `p1` to base their paths, they may inadvertently edit the wrong node or provide a malformed path.

##### A worked example

- The operator initially requests `p1 = Strict(Q_hi, P_lo)`.
- The compiler uses its degree of freedom to yield control `C1` such that `⌊C1⌋ = Strict(P_lo, Q_hi)`. Note that the children have been reordered for some compiler-internal reason.
- We echo back `p1' := ⌊C1⌋ = Strict(P_lo, Q_hi)`. We know that `p1 =R p1'`, so the scheduler the operator gets is the one they asked for; only the slot numbering differs.
- As the control serves pushes and pops, it transforms into `C1'`.
- Later, the operator requests `p2 = Strict(P_lo, R_mid, Q_hi)`.
- The runtime can again use its freedom. Instead of literally splicing `R` in between running arms `P` and `Q`, it chooses to append `R` to the end. This converts the running control `C1'` into control `C2'` such that `⌊C2'⌋ = Strict(P_lo, Q_hi, R_mid)`.
- We echo back `p2' := ⌊C2'⌋ = Strict(P_lo, Q_hi, R_mid)` to the user.
- Now the operator changes to Imperative Mode (§4) and writes the path-bearing edit `(True, Quiesce([2]))`. It is not worth getting distracted by the syntax or the semantics; the key thing is that the operator has requested an edit and has identified the target via a path `[2]`. Paths are interpreted against the _actually running_ representative `p2'`, so we `Quiesce` the subtree `R_mid` (`p2'`'s third slot), not `Q_hi` (`p2`'s third slot). If the operator had based the path on `p2` they would have edited the wrong arm; the system has no way to detect or recover from that.

### 3.3 A Grammar for Tree Diffs

§3.2 set up the operator-system loop: the operator writes `p1`, we compile and echo back `p1' = ⌊C1⌋`, and pushes and pops carry the running control to some `C1'` still realizing `p1'`.
When the operator asks for a new `p2`, SOTA would compile `p2` into a fresh `C2` and clobber `C1'` with it.
We want the same user-observable result while being less disruptive to unaffected parts of the running control.

A change to the live control is _atomic_ when its effect falls between two observable operations: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if the change fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by the pre-change control and `op_{N+1}, ...` are served entirely by the post-change control.
No operation straddles the change, and no operation sees an intermediate state.
§1's running example described this informally.

We fix a small grammar of atomic edits.
Each production denotes a single primitive that acts on a live control `C` and produces a new control.
The grammar in this section is the alphabet from which the transition planner (§4) assembles a sequence whose operational composition takes the presently running control `C` (which realizes the operator's prior request `p1`) to a new control realizing the operator's new request `p2` _without clobbering_ `C`.
Most productions are `pol`-visible: their effect shows up in `p2`, and a comparison of `p1` against `p2` is enough to understand them.
Others are transaction-only: their effect lives entirely in `z`, leaving the `pol`-level skeleton untouched.

An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is.
We write `p1@path` for the subtree of `p1` reached by following `path` down from its root.

```
δ ::= Add          (path, pol, meta?)
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
The richer reconfigurations an operator may want (retiring a subtree that has packets buffered in it, replacing a subtree in-place, pruning a tree down to a subtree) are realized instead as _idioms_: named sequences of these atomic edits.
§4 makes these precise.

Notes on the individual edits.

- `Add(path, pol, meta?)` appends `pol` as a new arm under `p1@path`.
  Since arm order at any internal node is `=R`-irrelevant (§3.2), we have the freedom to just _append_ the new arm, leaving other arms undisturbed.
  The third argument `meta?` is the new arm's per-arm metadata: a weight for a `WFQ` parent, a priority rank for a `Strict` parent, absent (`ε`) for an `RR` parent.
  `init_slot_D` reads `meta?` when seeding the new arm's `slot_state` (§3.4.1).
  Add is _non-disturbing_: it writes only the new arm's `slot_state`; every existing arm's `slot_state` is preserved verbatim.
  The new arm carries its own `meta?`, leaving the others' untouched.
- `Quiesce(path)` prevents `p1@path` from receiving new traffic.
  For every leaf `L` under `p1@path`, `L`'s `z` is restricted to reject all traffic it used to accept, and every ancestor of `L`, up to and including the scheduler's root, has its `z` correspondingly restricted to not accept that traffic.
  Topology, disciplines, and labels are unchanged; the edit's whole effect is in the touched `z`s, so it is `pol`-invisible.
- `Designate(path, pol)` converts `p1@path` into `Strict*(p1@path, pol)` in place.
  That is, we introduce a new node with discipline `Strict*`, with the existing subtree `p1@path` as its high-priority sibling and `pol` as its low-priority sibling.
  `pol` is the _designated survivor_ of `p1@path`.
  We make no assumption about how much overlap there is between `p1@path`'s flows and `pol`'s flows.
  In case there is overlap, we simply use timing information (the moment of the request) to distinguish old `p1@path` traffic from new `pol` traffic.
  This keeps the mid-sequence `pol`'s leaf labels disjoint by construction.
  Literally inserting this `Strict*` node in the middle of the running tree would be expensive, as it would require relocating the entire subtree `p1@path` one PE deeper (because siblings and cousins must share a PE, see §2.1).
  §6 features a new in-place hardware gadget that gives us the `Strict*` semantics described here without incurring that relocation cost.
- `Undesignate(path)` collapses `p1@path`, which must be a `Strict*(A, B)` node where `A` is empty, into `B`.
  The edit is in place, in the sense that `B` inherits `Strict*(A,B)`'s slot and per-arm `meta?` under the parent, and the parent now routes through that slot directly to `B`.
- `Remove(path)` structurally removes `p1@path`, dropping its slot and renumbering any higher siblings.
  The subtree `p1@path` must be empty; §3.4.4 discusses why.
  `Remove`'s precondition rules out `Strict*` targets.
- `ChangeWeight(path, weight)` overwrites the weight that `p1@path`'s parent uses for it.
  Concretely it writes the `weight` field of `p1@path`'s `slot_state` (one of the per-arm fields seeded by `init_slot_WFQ`); `push`/`pop` only read this field, so the only way it changes is via this diff.
  It is well-defined only when the parent at `path`'s prefix runs WFQ and `path` is non-empty.
- `Graft(ctx)` produces `ctx[p1]`: the policy context `ctx` is spawned around `p1`, with `p1` plugged into the context's sole hole.
  `Graft` carries no `path`: if the user wants localized graft-style edits, deeper in the tree, they must be realized as an idiom (§4), not by a path-bearing `Graft`.
- `ChangeRoot(path)` promotes `p1@path` to the new root, discarding every ancestor above it.
  It is well-defined only when `path` is non-empty and each internal node strictly above `p1@path` has a single arm (the one continuing toward `p1@path`), so the discarded ancestor chain carries only scheduling metadata, no traffic.
  The chain may have been shaping `p1@path`'s pop order, and `ChangeRoot` discards that influence.
  The richer reconfiguration of pruning a tree down to a subtree that originally shared ancestors with packet-bearing relatives is realized as the `PruneDownTo` idiom (§4), which first drains and `Remove`s those off-path subtrees, reducing the chain above `p1@path` to the unary shape `ChangeRoot` requires.

A `Strict*` node is one introduced by `Designate`; a plain `Strict` is compiled from user input.
Operationally we model the distinction as a one-bit `designated` flag that each node carries.
`Designate` sets it, `Undesignate` clears it.
The star is just shorthand for that bit being set, not a separate discipline.
Semantically the two are identical; every push, pop, and well-formedness check treats `Strict*(A, B)` exactly as `Strict(A, B)`.
The star exists only so that `Undesignate`'s precondition ("path lands on a `Strict*`") and the hardware story in §6 ("`Strict*` adds no PE depth") can be stated structurally.
The §3.2 DSL that the operator writes can parse only `Strict`, never `Strict*`, so a `Strict*` is unreachable in any user-written `pol` and arises only in the middle of a planner sequence, between a `Designate` and its eventual `Undesignate`.

When `p1 = p2` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

### 3.4 All Productions of `δ` are Sound

Each `δ` (§3.3) denotes the live rewrite

```
[[δ]] : control ⇀ control
```

acting on the control `C`.
We take `[[δ]]` as the source of truth for what `δ` means.
The per-production rules below state where `[[δ]]` is defined; outside that, `δ` is _incompatible_ with the input control and `[[δ]](C)` is undefined.
§4's transition planner only emits a `δ` whose `[[δ]]` is defined on the live `C`.
The preconditions vary by production, e.g.:

- For `Add` we require `path` to land at an internal node of `C`, the operator-supplied `meta?` to match what the parent discipline requires (a weight for `WFQ`, a priority rank for `Strict`, absent for `RR`), the new leaf labels to be fresh, and the new classifier predicates to be disjoint from the domain of `C`'s live `z`.
- For `Remove` we require the target subtree to be empty.
- For `Undesignate` we require the target node to be a `Strict*(A, B)` with `A`'s subtree empty.
- For `ChangeWeight` we require the parent at `path`'s prefix to run WFQ.

Independently of `[[δ]]`, we describe each diff's intended pol-level effect

```
den(δ) : pol ⇀ pol
```

by recursion on `pol`.
`den(δ)` gives a diff's `pol`-level effect in isolation.
For each production we state `den(δ)` in closed form and prove the following informal characterization from the operational rule.

> _Characterization._
> Assume `⌊C⌋` is defined and `[[δ]](C)` is defined.
> Then `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`.

In words: applying the control-level denotation of a diff to a live control yields a new control whose running `pol` matches `den(δ)`'s `pol`-level prediction.
This is rule 3 of `⌊·⌋` (§3.2), made concrete for each production by the closed-form `den(δ)`.
Working at this level, §4's planner searches for a sequence of edits that, when viewed via `den` and composed together, carries `p1` to `p2`.

`den(δ)` is partial for the same syntactic reasons that `[[δ]]` is (paths must resolve, leaf labels must be fresh, weights must match), but, because `pol` carries no live contents or transaction, it never fails for operational reasons like `Remove`'s emptiness or `Add`'s classifier disjointness.
Effects that live entirely in `z` are pol-invisible: `den(Quiesce) = id_pol`, so by rule 3, `⌊[[Quiesce]](C)⌋ = ⌊C⌋`; the running pol after a `Quiesce` is unchanged, even though `[[Quiesce]]` has done real work on `z`.
The `designated` bit on a `Strict*` node is likewise pol-invisible (it is operational, not part of any `pol`), so `den(Designate(path, B))` wraps `pol@path = A` into `Strict(A, B)`, and `den(Undesignate(path))` consumes a `Strict(A, B)` at `path` and yields `B`.
Composed in that order, they act as a replacement at `path`.

_Atomicity_ can be restated crisply: when `δ` is compatible with `C`, `[[δ]]` replaces the live control `C` with `[[δ]](C)` between two consecutive `push`/`pop` operations.
Modeling `[[δ]]` as a single (partial) function `control ⇀ control` bakes indivisibility into the abstraction; the obligations below ensure that the result of this single step is correct.

For each production we state `den(δ)`, prove the characterization theorem above, and discharge two further obligations, all assuming `[[δ]](C)` is defined.
Throughout, we write `C` for the pre-edit control and `C'` for the post-edit control.

- _Pol-level characterization_ pins down the structural skeleton up to `=R`: the closed-form recursion `den(δ)`, plus the proof that `⌊C'⌋ =R den(δ)(⌊C⌋)` whenever both sides are defined.
  `den` returns a definite pol (a definite representative of an `=R`-class); the implementation is free to lay out `C'` so that `⌊C'⌋` is any other representative of the same class.
  For our running example, `δ = Add([], spotify, mid)`, the closed form gives `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and the theorem says `⌊C'⌋ =R Strict(gmail, spotify, zoom) = p2` for any `C` with `⌊C⌋ =R Strict(gmail, zoom)`.
  Note that we are only `=R`, not `=`, to the user's request, but that is okay.
  Transaction-only effects (e.g., `Quiesce`'s shrinking of `z`'s domain) lie outside the scope of `den` and are fixed by the per-production operational rule below, not by the characterization.
  The characterization says nothing about contents.
- _Soundness_ constrains the live contents: `|- C` must imply `|- C'`.
  Well-formedness (§3.2) is about per-node pifos and the packets stored under them, so this obligation says that any packets and index entries `[[δ]]` drops or adds must leave the counts in balance.
- _State preservation_ constrains each local control's state.
  At every node structurally shared between `p1` and `p2` and outside the production's local edit site, the local `state` is preserved verbatim.
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

#### 3.4.1. `Add(path, pol, meta?)`

##### Operational transition

Let `π = path` be the parent under which the new arm goes (here `π = []`, the root `Strict`); let `D` be the discipline at `π` and `k = |C@π.slot_states|` be the new arm's slot index (since our intention is just to append).
The transition `C' = [[Add(path, pol, meta?)]](C)` is stated per node.

The topology gains a new arm at `π`, indexed `k`; every pre-existing arm at `π` keeps its index.
The local controls update as follows.

- _At every node outside the new subtree, other than `π` and its proper ancestors:_ the local control is preserved verbatim.
- _At each proper ancestor of `π` (including the root):_ `node_state`, `slot_states`, and `pifo` are preserved verbatim.
  The local `z` is extended to admit packets that classify into the new subtree, mapping them to whichever child slot at this ancestor lies on the path down to `π`.
- _At `π`:_
  - `C'@π.node_state = C@π.node_state` (unchanged).
  - `C'@π.slot_states = C@π.slot_states ++ [ init_slot_D(C@π.node_state, meta?) ]`: whatever per-arm bookkeeping that `init_slot_D` prescribes for an arm newly spliced under a running `D`-parent is appended at index `k`.
  - `C'@π.pifo = C@π.pifo` (unchanged): no pre-existing entry needs renumbering, and the new arm holds no packets yet.
  - `C'@π.z` extends `C@π.z` to admit packets that classify into the new subtree, mapping them to child index `k` at `π` (with descent handed off to the new subtree's `z` pieces below).
    Pre-existing mappings are untouched.
- _At every node inside the new subtree:_ the local control is what §3.2's compiler produces for that node.

##### Characterization

The pol-level effect of `Add(path, pol, meta?)` is the structural map on `pol`, given by recursion on `path`:

```
den(Add([],        pol, meta?)) (D ts) = D ( ts ++ [pol] )
den(Add(i :: rest, pol, meta?)) (D ts) = D ( ts[ den(Add(rest, pol, meta?)) (ts[i]) / i ] )
```

The base case applies once `path` has walked down to the new arm's parent: `pol` is appended as the last child.
Recall from §3.3 that `Add`'s `path` resolves in `p1` and names the new arm's parent, so the base case lands at that parent.
The recursive case walks one step deeper into child `i` and writes the result back in place.
The `meta?` argument is threaded through the recursion but does not appear in the closed form: it is per-arm bookkeeping consumed by `init_slot_D` at the operational level (§3.2's note that pol's grammar carries no structural mark of meta), so it is pol-invisible.

_Proof of characterization._
We argue that the structural skeleton of `C'` matches `den(Add(path, pol, meta?))(⌊C⌋)`, which justifies rule 3 of `⌊·⌋` (§3.2) for this production.
The operational rule above leaves every pre-existing arm structurally intact (the `z` extensions along the ancestor chain are pol-invisible) and adds a new arm at `π` whose subtree is exactly what §3.2's compiler emits for the operator-supplied `pol`.
Walking `path` from the root through `⌊C⌋`, this matches the recursion: at each proper ancestor we recurse into the child on the path; at `π` we append `pol` to the child list, exactly as the closed form prescribes.
Equality is on the nose, but we state the characterization mod `=R` for uniformity with the other productions.
So `⌊C'⌋ =R den(Add(path, pol, meta?))(⌊C⌋)`.

Our running example computes `den(Add([], spotify, mid)) Strict(gmail, zoom) = Strict((gmail, zoom) ++ [spotify]) = Strict(gmail, zoom, spotify) =R Strict(gmail, spotify, zoom) = p2`, as intended.
The closed form appends `spotify` at the end, while the operator wrote it in the middle slot.
Both layouts are `=R`-equivalent: priority lives in each arm's `meta?` (§3.2), not in the slot index, so the runtime is free to lay out the children in any slot order it likes.
The realized control is `Strict(gmail_hi, zoom_lo, spotify_mid)`, with `spotify` appended at slot 2 even though its priority sits between `gmail`'s and `zoom`'s.
A deeper edit looks the same modulo descent: `den(Add([1], spotify, ε)) Strict(gmail, RR(zoom, youtube)) = Strict(gmail, RR(zoom, youtube, spotify))` recurses into the `RR` child at slot `1` and appends `spotify` there; `meta?` is `ε` since the parent runs `RR`.

##### Soundness

`|- C` gives `|- C'`.
At `π`, `C'@π.pifo = C@π.pifo` contains no entry equal to `k` (no pre-existing entry can name a slot that did not exist in `C`), and the new arm at slot `k` holds zero packets, so its well-formedness count reads `0 = 0`.
Every other slot at `π` keeps its index, its packets, and its entries in `C'@π.pifo` verbatim, so its matched count is inherited.
Every other node's pifo is untouched (the ancestor `z` extensions touch no pifo at this instant; they only affect the classification of packets that arrive later).
Nothing needs repair.

##### State preservation

Outside the edit site the local control (and thus its `state`) is preserved verbatim, including at each proper ancestor of `π`, where only `z` changes.
Inside the new subtree, the state is freshly compiled per §3.2.
At `π`, `node_state` is unchanged and `slot_states` is appended with exactly `init_slot_D(C@π.node_state, meta?)`, as §3.4 prescribes at the edit site.

##### Notes

_Atomicity._
No in-flight packet straddles the diff.
At the diff instant, every packet sits in some pre-existing node's `pifo`, and each such packet survives into the same `pifo` at the same slot index in `C'`.
The new slot `k` holds nothing, and no pifo entry is rewritten.
So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
The first `push` that `C'@π.z` routes to `k` is the first packet ever to occupy the new subtree.

_Deeper paths._
The running example edits the root, but `path` may be arbitrary; `Add([1, 2], pol, meta?)` appends a new arm under the grandchild at `[1, 2]`.
Nothing in the argument changes.
The descent from the root to `π` passes through ancestors whose only change is the `z` extension above; their `node_state`, `slot_states`, and `pifo` are untouched.
Because the new subtree is empty, no packet is yet routed through any ancestor's `z` extension, so each ancestor's count for the child it forwards through is exactly what it was.
No ancestor pifo is rewritten, and the edit is otherwise confined to `π` and the fresh subtree below it.

The remaining productions all reuse the same obligations and arguments as `Add`.
We present them in compact form: closed-form `den`, the operationally interesting bits of the per-node rule, and the points where the soundness or atomicity argument differs in substance from what has already been shown.

#### 3.4.2. `ChangeWeight(path, weight)`

`ChangeWeight` overwrites the WFQ weight that `p1@path`'s parent uses for it (§3.3).
Weights live in `slot_state`, not in `pol`: `pol`'s WFQ records only an arity, and per-arm weights are operator-supplied at compile or `Add` time and held thereafter only in the running control's `slot_state.weight` field.
`ChangeWeight` is therefore `pol`-invisible, in the same sense as `Quiesce` (§3.4.3): its entire effect lies in operational state that `pol` does not record.
`path` must be non-empty and the parent at `π = path[:-1]` must run WFQ; let `k` be the last index of `path`.

##### Operational transition

- _At `π`:_ `node_state` (the parent's WFQ virtual time) unchanged; `slot_states[k].weight` overwritten with the new weight; every other field of `slot_states[k]` preserved verbatim, including the virtual-finish tag that records how far slot `k` has run in the current round; other `slot_states`, `pifo`, and `z` unchanged.
- _Everywhere else:_ preserved verbatim.

##### Characterization

```
den(ChangeWeight(path, w)) p = p
```

defined whenever `path` resolves in `p` and the parent at `path`'s prefix is `WFQ`.
Proof: every node's `node_state`, `slot_states.discipline`, `pifo`, and child list are preserved verbatim (the rewritten field is in `slot_state`, which the compilation rule strips), so `⌊C'⌋ = ⌊C⌋` on the nose.
Hence `⌊C'⌋ =R den(ChangeWeight(path, weight))(⌊C⌋)`.

##### Soundness, state preservation, atomicity

No `pifo` entry is rewritten, no subtree is touched, no packet is relocated, so well-formedness is inherited everywhere.
State preservation: the targeted weight field is the intended edit; every other field at `π` and everywhere else is verbatim.
Atomicity: ranks for in-flight `pifo` entries were burned in by `push` at the old weight and remain fixed; the new weight affects only the rank computation for future pushes.
So a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.

The virtual-finish tag at slot `k` is deliberately preserved rather than reset: resetting would either reward or penalize the arm by yanking it out of the current round, whereas `init_slot_WFQ` (§3.2) was designed precisely to let a fresh arm "join the current round" without disturbing siblings, and the same principle applies to a weight change on an already-running arm.

#### 3.4.3. `Quiesce(path)`

`Quiesce` is the grammar's sole tool for halting traffic into a subtree.
It does not modify the subtree's structure or its contents.
The patch lives in `z`: every leaf under `p1@path` and every ancestor up to the root has its `z` restricted so that any packet bound for any leaf under `p1@path` is refused admission.
Topology, disciplines, slot states, and pifo entries are all unchanged, so `Quiesce` is `pol`-invisible.

##### Operational transition

The transition `C' = [[Quiesce(path)]](C)` is defined when `path` is non-empty and resolves to a node in `C`.
There is no precondition on the subtree's contents: `Quiesce` is the production that stops further admissions, regardless of what is currently held below `path`.

Let `T` denote the set of leaves of `p1@path` (the leaves we are silencing), and let `A` denote the union of their ancestor chains: every internal node within `p1@path`'s subtree, the node `C@path` itself, and every ancestor of `C@path` up to and including the root.
The topology, the discipline at every node, and every arm's slot index are unchanged.

- _Outside `T ∪ A`:_ the local control is preserved verbatim.
- _At each leaf `L` in `T`:_ `node_state`, `slot_states`, and `pifo` preserved verbatim (including any packets `L` currently holds).
  `C'@L.z` restricts `C@L.z`: every packet that `L` used to admit is removed from `C'@L.z`'s domain.
- _At each internal node `n` in `A`:_ `node_state`, `slot_states`, and `pifo` preserved verbatim (including any pre-existing entries pointing toward the quiesced subtree, which continue to be honored on `pop`).
  `C'@n.z` restricts `C@n.z`: packets whose classifier predicate matches any leaf label in `T` are no longer in `C'@n.z`'s domain.
  For surviving inputs the output is `C@n.z`'s output verbatim.

##### Characterization

`Quiesce` is `pol`-invisible: its entire effect lies in `z`'s domain, which `pol` does not record.

```
den(Quiesce(path)) p = p
```

defined whenever `path` is non-empty and resolves in `p`.
Every node's `node_state`, `slot_states`, `pifo`, and child list are preserved verbatim; only the `z`s at nodes in `T ∪ A` are restricted, and `z` is not visible at `pol`-level.
Equality is on the nose, so `⌊C'⌋ =R ⌊C⌋ = den(Quiesce(path))(⌊C⌋)`.
This is the showcase case of a diff whose semantic content lives entirely in `z`: the characterization theorem honestly reports it as a no-op on `pol`, because nothing structural moved.

##### Soundness, state preservation, atomicity

Every node's `node_state`, `slot_states`, `pifo`, and child subtree are preserved verbatim, so well-formedness counts are inherited everywhere and `state` is preserved at every node.
The `z` restrictions across `T ∪ A` touch no `pifo` entry and no stored packet; they only refuse future `push`es.
Atomicity follows from §3.4.1's argument: every in-flight packet sits in some pre-existing `pifo` that survives verbatim, so a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.

##### Notes

_Why touch every node on every quiesced push path._
Under §3.2's parallel `push`, every node on the path from a packet's destination leaf to the system root mints an index via its local `z` independently of the others.
If we restricted `z` only at a strict subset of those nodes (only the root of the tree, or only inside `C@path`), the rest would happily mint indices and enqueue them, leaving the tree in an ill-formed state.
`Quiesce` therefore restricts uniformly, at the leaf (so it refuses to enqueue) and at every ancestor of every quiesced leaf (so no one mints a stray index).
The cost is touching `|T|` leaves and the union of their ancestor chains; the gain is a rejection that preserves `|- C'`.

_Cooperates with `Remove`._
`Quiesce` does not by itself delete or drain the subtree; it only stops new traffic.
The natural sequence `Quiesce(path); let the subtree at p1@path drain to empty; Remove(path)` is the `Retire` idiom of §4.1: after the `Quiesce`, no new packet enters `p1@path`; the drain consumes the packets already there through ordinary `pop` operations; and the `Remove` is then defined, since `p1@path` is empty.
`Quiesce`'s `z` restrictions on every push path to a quiesced leaf make the input restriction at `π` that `Remove` performs (§3.4.4) redundant in this sequence, but `Remove`'s output renumbering at `π` is still needed.

#### 3.4.4. `Remove(path)`

`Remove` is the grammar's one structural deletion.
It unhooks the subtree `p1@path` from its parent, drops the vacated slot, and renumbers any higher siblings.
It is defined only when `p1@path` is _empty_; the Notes below explain why.
Retiring or replacing a subtree that still holds packets is therefore not `Remove`'s job alone: it is realized as a _sequence_ that first drains the subtree and only then removes it (see §4).

##### Operational transition

Let `path = π ++ [k]`, where `π` is the path to the parent of the removed subtree and `k` is the slot index at that parent.
Let `D` be the discipline at `π`.
The transition `C' = [[Remove(path)]](C)` is defined when (i) `path` is non-empty and resolves to a node in `C`, (ii) the subtree at `path` is empty, and (iii) `C@path` does not carry the `Strict*` flag (the planner reaches a `Strict*` only through `Undesignate`, §3.4.6).

The topology loses the arm at slot `k` of `π`; arms at slots `0, ..., k-1` keep their indices; arms at slots `k+1, ...` shift down by one.

- _Outside `π` and the removed subtree:_ the local control is preserved verbatim.
  This includes every proper ancestor of `π` (whose `z` is unchanged) and every sibling arm at `π` other than slot `k`, together with the subtrees under them.
- _At `π`:_
  - `node_state` unchanged.
  - `slot_states = C@π.slot_states[-/k]`: the entry at slot `k` is dropped; entries at slots `> k` shift down by one to track the renumbered arms.
  - `pifo = C@π.pifo[-/k]`: the precondition (subtree at `path` empty) plus `|- C` forces `C@π.pifo` to contain no entry equal to `k`, so this renumbering deletes no values; it only decrements entries `> k` by one.
  - `z` is restricted on inputs and renumbered on outputs.
    Packets that `C@π.z` would have routed to slot `k` are no longer in `C'@π.z`'s domain; for surviving inputs, an output of `(i, r)` with `i > k` becomes `(i - 1, r)`.
    Slots `< k` are untouched on either axis.

A standalone `Remove` thus restricts classification at `π` so that packets bound for the removed subtree are dropped at `π` rather than being routed to a non-existent slot.
In the typical planner usage (`Retire` of §4.1), a preceding `Quiesce` has already restricted every `z` on every push path to the soon-to-be-removed leaves (§3.4.3), so no new packet bound for the removed subtree is even being admitted; the input restriction at `π` is then redundant but harmless, while the output renumbering at `π` is still needed.

##### Characterization

```
den(Remove([k]))         (D ts) = D ( ts[-/k] )
den(Remove(i :: rest))   (D ts) = D ( ts[ den(Remove(rest)) (ts[i]) / i ] )      when rest is non-empty
```

The base case fires once `path` has been walked down to the parent of the removed arm: the `k`-th child is dropped from the arm list.
The recursive case walks one step deeper into child `i` and writes the result back in place.
`den(Remove(path))` is defined when `path` is non-empty and resolves in the input pol; the emptiness precondition that `[[Remove(path)]]` imposes is operational, not visible at `pol` level.

Proof shape is `Add`'s: outside the removed subtree every node is structurally intact (ancestors verbatim, surviving siblings verbatim), and at `π` the child list shrinks by dropping slot `k`.
Equality is on the nose, so `⌊C'⌋ =R den(Remove(path))(⌊C⌋)`.

##### Soundness, state preservation, atomicity

At `π`, the precondition gives `C@π.pifo` no entry equal to `k`, so the renumbering `[-/k]` deletes no values; surviving slots `i' < k` of `C'@π` inherit their matched counts from slot `i'` of `C@π`, and surviving slots `i' >= k` inherit theirs from slot `i' + 1` (same subtree, renumbered entries).
Every proper ancestor and every surviving sibling is verbatim, so its well-formedness count is inherited and its `state` is preserved.
No `init`-rule fires; `slot_states` drops slot `k`'s entry but every other entry is preserved verbatim, with its position shifted to match the new arm order.
Atomicity follows from §3.4.1's argument applied to the surviving structure: every in-flight packet sits in a preserved `pifo` at the same slot (if `< k`) or one slot lower (if `> k`), and every surviving `pifo` entry points to the same child subtree after renumbering, so a `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
The restriction at `C'@π.z` comes into play only for `push`es that arrive after the diff fires.

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
The _choice of which_ `1`s to drop _silently reschedules `A` against `C`_, and nothing in `p1` or the deletion request records which the operator wanted.

We do not wish to make a scheduling decision for the operator, so our only other option is to reconstruct the tree as if `B` had never been admitted: drain the tree completely, filter the `B`-packets, and re-push, recomputing every rank and cursor along the way.
This is a stop-the-world rebuild.

Our grammar sidesteps the question entirely.
By draining `B` to empty before removing it (§4), every pop that served a packet from `B` has already removed the matching index-entries from `B`'s ancestors in the ordinary course of `pop`: a packet leaving `B` consumes a `2` at `Q` and a `1` at `P`, bringing each ancestor's count into accord with its post-drain contents.
At the instant `Remove` fires, `Q`'s `2`-count is zero (`B` is empty) and `P`'s `1`-count is exactly the number of packets still under `Q` (here `2`, all attributable to `A`).
There is nothing left to reconcile and no hidden policy choice to make: the structural deletion is unambiguous.

#### 3.4.5. `Designate(path, pol)`

`Designate` wraps the existing subtree `p1@path` in a fresh `Strict*` node whose high-priority arm is that subtree and whose low-priority arm is the operator-supplied `pol`.
The wrap is `pol`-visible as a `Strict`: the `designated` bit is operational only (§3.3).
Per §3.3, every push, pop, and well-formedness check treats `Strict*(A, B)` exactly as `Strict(A, B)`, so the obligations below are stated against the ordinary `Strict` semantics of §3.2.

##### Operational transition

The transition `C' = [[Designate(path, pol)]](C)` is defined when `path` resolves to a node in `C` and the leaf labels of `pol` are disjoint from those of `C` (the time-stamp trick of §3.3 keeps this so even when the operator's `pol` syntactically overlaps `p1@path`).
Let `P0` be the number of packets currently held under `C@path`, and let `N` denote the freshly introduced `Strict*` node.

- _Outside the subtree at `path`, outside `N`, and off the ancestor chain to `path`:_ preserved verbatim.
  If `path` is non-empty, let `π ++ [k] = path`: at `π` the `node_state`, `slot_states`, and `pifo` are unchanged; the slot `k` that used to point to `C@path` now points to `N`, inheriting `C@π.slot_states[k]` (the per-arm meta is held by `π`, not by `N`, so the wrap is transparent to `π`); `z` is extended as described in the next bullet.
- _At each proper ancestor of `path` (including the root, and including `π`):_ `node_state`, `slot_states`, and `pifo` preserved verbatim.
  The local `z` is extended to admit packets that classify into the new arm-1 subtree under `N`, mapping them to whichever child slot at this ancestor lies on the path down to `N`.
  Pre-existing mappings are untouched.
  This is the exact analog of `Add`'s ancestor `z`-extensions (§3.4.1).
- _At `N`:_ `node_state = init_node_Strict()`; `slot_states = [init_slot_Strict(node_state, hi), init_slot_Strict(node_state, lo)]` for any priority ranks `hi < lo`; arm 0 is the old `C@path` (verbatim, including its entire subtree and contents); arm 1 is the §3.2-compiled control for `pol`; `pifo` is seeded with `P0` copies of `0` (one per packet still held under arm 0); `z` routes leaf-label predicates of the old subtree to slot `0` and leaf-label predicates of `pol` to slot `1`.
- _Inside arm 1:_ the local control is what §3.2's compiler produces for `pol`.

If `path` is empty, the entire control becomes `N`, with the old root sitting as arm 0.

##### Characterization

```
den(Designate([],     p_low)) A      = Strict(A, p_low)
den(Designate(i :: r, p_low)) (D ts) = D ( ts[ den(Designate(r, p_low)) (ts[i]) / i ] )
```

`den` returns `Strict`, not `Strict*`: the designated bit is `pol`-invisible.
The proof is the same shape as `Add`'s: the operational rule leaves every node outside the wrap structurally intact, the new arm 1 is freshly compiled per §3.2, and the new `Strict*` reads as `Strict` at `pol`-level.
Equality is on the nose, so `⌊C'⌋ =R den(Designate(path, pol))(⌊C⌋)`.

##### Soundness, state preservation, atomicity

Soundness holds at `N`: by construction the `0`-count in `N.pifo` equals the packet count under arm 0, and arm 1 holds no packets and no `1`-entries in `N.pifo`.
Every node inside arm 0 is preserved verbatim, so its well-formedness count is inherited.
At `π` (if `path` non-empty), `pifo` is unchanged, and the slot-`k` count `P0 + 0` matches the count that `C@path` had under `π`.
The ancestor `z`-extensions touch no `pifo` and route no in-flight packet, so they affect only `push`es that arrive after the diff.
State preservation is the standard split: arm 0 is verbatim; arm 1 is freshly compiled; `N`'s `node_state` and `slot_states` come from the listed `init` calls; everything else is verbatim.
Atomicity: every in-flight packet under arm 0 sits at exactly the same position it did in `C@path`, since arm 0 is `C@path` verbatim.
If `P0 > 0`, the smallest entry in `N.pifo` is `0`, so any pop that descends to `N` continues into arm 0 and returns exactly the packet a pre-diff pop would have.
If `P0 = 0`, then by `|- C` no pop ever descends to `N` (the subtree at `path` was already empty), so the question is vacuous.
Arm 1 first sees traffic only after a future `push`.

Hardware realizes the `pifo` seeding of `P0` zeros in O(1) via the in-place super-node gadget of §6, rather than literally writing `P0` entries.

#### 3.4.6. `Undesignate(path)`

`Undesignate` unwraps a `Strict*(A, B)` at `path` whose `A`-arm is empty, leaving `B` in its slot.
It is the structural inverse of `Designate` and the second half of the `Replace` idiom (§4.1).

##### Operational transition

Defined when `path` resolves to `C@path`, `C@path` carries the `designated` bit (so it is a `Strict*` introduced by an earlier `Designate`), and arm 0 of `C@path` is empty.
Let `N = C@path`.

- _Inside `B` (arm 1 of `N`):_ preserved verbatim, every node.
- _At `π = parent(path)`_ (if `path` is non-empty, with `k` the last index): `node_state`, `pifo`, and `z` unchanged; `slot_states[k]` preserved verbatim (the wrapper inherited it at `Designate` time, and the unwrap returns it unchanged); slot `k` now points to `B` rather than to `N`.
  `N` itself and its arm-0 stub are discarded.
- If `path` is empty, the new root is `B`.

##### Characterization

```
den(Undesignate([]))     (Strict(A, B)) = B
den(Undesignate(i :: r)) (D ts)         = D ( ts[ den(Undesignate(r)) (ts[i]) / i ] )
```

The base case consumes the `Strict` wrapper (which on the `pol` side is what the `Strict*` reads as).
Proof shape is `Remove`'s: outside `N` nothing structural moves, and at `N` the `Strict` wrapper is dropped.
So `⌊C'⌋ =R den(Undesignate(path))(⌊C⌋)`.

##### Soundness, state preservation, atomicity

At `π`, `pifo` is unchanged; slot `k`'s count was `0 + |B|` under `N` and is now `|B|` under `B` directly, so the count matches.
Inside `B`, every node is verbatim.
`N`'s `node_state`, `slot_states`, and `pifo` are discarded together with `N`: by precondition `N.pifo` contains no `0`-entries, and its `1`-entries are also discarded harmlessly, since `π.pifo`'s unchanged `k`-entries now route pops directly to `B` rather than through `N` (the only step we skip is the redundant "Strict* says go to arm 1" routing, which arm 0 being empty had already forced).
State preservation: standard verbatim everywhere outside `N`; `N`'s local state vanishes.
Atomicity: arm 0 holds no packet by precondition, so `Strict*`'s "favor arm 0" behavior was already routing every pop to arm 1; the unwrap preserves this routing on the nose.

#### 3.4.7. `ChangeRoot(path)`

`ChangeRoot` promotes `p1@path` to the new root, discarding every ancestor above it (§3.3).
It is the only production that erases structural ancestors, and the only one whose `pol`-level effect is to shrink the tree from above rather than edit it locally.
The mechanics are short; the Notes below carry most of the substance.

##### Operational transition

Defined when `path` is non-empty, resolves to a node in `C`, and every internal node strictly above `C@path` is _unary_, with its sole arm continuing toward `path`.
Under these preconditions the discarded ancestor chain carries scheduling metadata but no off-path traffic.

- _At `C@path` and inside its subtree:_ preserved verbatim.
  The result `C'` is exactly `C@path` standing alone as a tree.
- _At every proper ancestor of `path`:_ discarded entirely, together with the local `node_state`, `slot_states`, `pifo`, and `z`.

##### Characterization

```
den(ChangeRoot(path)) p = p@path
```

defined whenever `path` is non-empty and resolves in `p`.
The realizes-relation propagates structurally: if `C` realizes `p`, then `C@path` realizes `p@path`, since compilation is per-node and the discipline at each surviving node is unchanged.
Equality is on the nose, so `⌊C'⌋ =R p@path = den(ChangeRoot(path))(⌊C⌋)`.

##### Soundness, state preservation, atomicity

Soundness: `|- C` is a conjunction of per-node well-formedness obligations, and the subset of those obligations attached to `C@path`'s nodes constitutes `|- C@path`, which is exactly `|- C'`.
The discarded ancestors' obligations are simply forgotten.
State preservation: standard verbatim within `C@path`; ancestor state is gone.
Atomicity in our formal sense (§3.3) holds: every in-flight packet sits in some `pifo` within `C@path`'s subtree (the ancestor `pifo`s held routing entries, not packets), all preserved verbatim.
A pop immediately after the diff returns exactly what a pop immediately before would have: a pre-diff pop descends through the unary chain by reading some slot-`0` entry at each ancestor (regardless of which entry, since with a single arm every choice resolves to "recurse via arm 0") and ultimately pops from `C@path`'s root; a post-diff pop acts on `C@path`'s root directly.

##### Notes

_Why the unary precondition._
The precondition rules out, by construction, any silently-dropped packet-bearing siblings.
A non-unary ancestor would have arms branching off the path to `C@path`; those subtrees would vanish with their ancestor, taking their packets with them.
Allowing this would make `ChangeRoot` a structural deletion of arbitrarily many off-path subtrees masquerading as a root change, with no semantic story for those packets.
The richer reconfiguration of pruning to a subtree whose ancestor chain branches off into packet-bearing relatives is realized as the `PruneDownTo` idiom (§4.1), which first drains and `Remove`s those siblings in sequence, reducing the chain above `C@path` to the unary shape that `ChangeRoot` then accepts.

_What the chain carries, and what is discarded._
An internal node's `node_state`, rank function, and `pifo` ordering exist to pick among siblings.
For the concrete disciplines this paper targets, those mechanisms therefore degenerate at a unary node: Strict's all-zero ranks reduce to FIFO, RR's cursor is irrelevant with one arm, and WFQ's virtual-finish times under a single weight are monotone in arrival order.
For these disciplines the unary chain is operationally a passthrough, and the immediate-pop atomicity above extends to every subsequent pop and push.

This is not, however, a universal property of unary internal nodes.
A discipline whose rank function depends on per-packet attributes rather than on arm selection (LSTF being the canonical example, with rank determined by each packet's deadline) can keep its `pifo` non-FIFO at a unary node, and that ordering may shape what an external observer sees downstream depending on the surrounding PIFO-tree semantics.
In those cases the discarded chain is _not_ a no-op: `ChangeRoot` removes whatever schedule shaping the chain was performing on the traffic now living under `C@path`.
That is precisely the production's intended role: it is the atomic step by which the operator says "promote `p1@path` to the root and discard the ancestor shaping above it."
The `PruneDownTo` idiom packages this with the upstream draining and `Remove`s that make the chain unary in the first place, so the operator's request ("prune to this subtree") is realized as a sequence whose final step discards exactly the ancestor influence that the operator has chosen to abandon.

_Pol-level effect._
The `pol` changes from the unary chain wrapping `p1@path` (e.g., `Strict(p1@path)` or `LSTF(p1@path)`) to just `p1@path`.
This is `pol`-visible: the root discipline observably changes, which is the substantive content of the production.
For the concrete disciplines (Strict, RR, WFQ) the `pol`-visible change happens to coincide with no in-flight reordering, per the previous Note; for more general disciplines the operator has chosen to give up whatever the chain was shaping.

#### 3.4.8. `Graft(ctx)`

[AM TODO: Anshuman wants to discuss this IRL.]

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
  Every `push`/`pop` therefore still lands on a well-formed control (`p1`, a `link`, or `p2`), exactly as §3.4 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

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

Two authoring modes produce sequences, and the operator chooses freely between them.
In _declarative mode_, the operator writes a `pol` and, to reconfigure, writes a second `pol`; a differ proposes a sequence, and the operator either accepts it (in which case we apply the sequence to the running control) or declines it.
The differ is intentionally simple: it sees only pol-level diffs whose translation to a sequence is straightforward, and falls back to the generic `Designate([], p2) ; Undesignate([])` pair (§5) for anything richer.
Multi-step reconfigurations with operator choice (e.g., `Retire` vs. `SlowRetire` below), graft-style local edits the differ cannot infer, and other confined strategies are outside its scope.
In _imperative mode_, the operator writes both their desired `p2` and a `(φ; δ)*` sequence intended to reach it, drawing on a small vocabulary of _idioms_ (§4.1) and on raw atomic diffs.
Before running the sequence we check it for the operator at the pol level: we fold each diff's `den(δ_i)` (§3.4) along the sequence, producing a chain `⌊p1⌋ -[den(δ_1)]-> p_a -[den(δ_2)]-> p_b -[den(δ_3)]-> ... -[den(δ_n)]-> p_z`, and accept the request iff every `den(δ_i)` is defined on the intermediate pol it sees and `p_z =R p2`.
Guards play no role in this check; they govern the operational timing of when each `δ_i` fires on the live control, and are pol-invisible (§3.4).
If the chain fails to reach `p2`, or some `den(δ_i)` is undefined on its intermediate pol, we reject the request.

The two modes are not formally distinct: the sequences they produce live in the same substrate and discharge the same soundness obligations from §3.4.
Imperative mode buys expressivity, not a different proof obligation.
It admits sequences the differ might never emit, but fundamentally still emits `(φ; δ)*` sequences.

The headline result of the section is that the transitionary scheduler `link` between two consecutive diffs is itself an ordinary §3.1 control, so the "transitionary period" needs no new semantics: this is Obligation 1 of §1, discharged.

[AM TK: liveness (whether and when a sequence's guards become true) is a nice-to-have feature that the user needs to achieve.
The simplest behavior: if the operator requests a new change while the previous one is still in flight, the new change is queued until the old one finishes.]

### 4.1 Idioms: Named Sequences

Our imperative mode above needs a vocabulary.
The atomic diffs of §3 cover individual edits; many useful reconfigurations are multi-diff.
_Idioms_ are imperative mode's vocabulary: named multi-diff patterns the operator can write directly, just as they can write a single atomic diff.
The operator can also define their own idioms.

An idiom is a macro over the diff grammar (and, recursively, over other idioms).
It expands into a fixed `(φ; δ)*` sequence: a list of atomic diffs with the guards between them spelling out what the system waits for.
Soundness is compositional: each step of the expansion is sound by §3.4, and the sequence inherits the §4 sequence-level reasoning above.
An idiom expansion that would hit an undefined production on the current control is rejected: the soundness checks fire on the expanded sequence just as they would on a hand-written one.

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

- **`PruneDownTo(path)`** = `Retire(p_a) ; ... ; Retire(p_z) ; (true; ChangeRoot(path))`, where `p_a, ..., p_z` are the off-path subtrees along the route from the root to `path`.
  This idiom is the operator-facing way to say "abandon everything except this subtree."

  Each `Retire` removes one off-path subtree; once all `m` have fired, every ancestor along the path is unary, satisfying `ChangeRoot`'s precondition (§3.3).
  (If no off-path subtrees exist, `m = 0` and the idiom reduces to `(true; ChangeRoot(path))` alone.)

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

### Short note on `nextnext`.

This paper's transition planner engages with one `p1 -> p2` pair at a time.
If the operator submits a follow-up `nextnext` while a `p1 -> p2` sequence is still mid-flight (i.e., while some guard `φ` has not yet become true), our answer is the simplest possible one: `nextnext` is queued and the transition planner does not begin work on it until the in-flight sequence completes.
See `paper/discussion-separable-nextnext.md` for a stronger possibility.

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
