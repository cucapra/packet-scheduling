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
§3.2 defines a small policy DSL and a compiler from it into a runnable _control_, giving us the syntactic handle on controls that the rest of the section needs.
§3.3 fixes a grammar of structural edits (_diffs_) over that DSL, where every diff is atomic by construction.
§3.4 restates atomicity once the operational rewrite `[[δ]]` is in hand, then, for each production, states the pol-level effect `den(δ)`, proves the informal characterization theorem `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`, and discharges two further obligations: preservation of well-formedness, and preservation of shared state.
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
Arm order in the surface notation is a presentation choice, not a scheduling-meaningful one: `Strict(hi: A, lo: B)` and `Strict(lo: B, hi: A)` describe the same scheduler.
We formalize this below as the _reorder-congruence_ `=R` on `pol`, the smallest congruence under which permuting siblings at any internal node is a no-op.
From §3 onward we use `=R` as a degree of freedom: the compiler is free to pick any representative of an `=R`-class when laying out or editing a control.
Further, the pipeline echoes back to the operator the specific representative that the compiler chose, so the operator can address slots positionally against what is actually running.
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

A PIFO tree _control_ `C` is a tree of triples `(state, pifo, z)`, one per node of the topology.
The tree shape exactly matches that of `pol`, as each node of `C` lines up with a node of the source `pol`.
Each diff of §3.4 acts on a small local neighborhood of these node-local triples; the whole control is just the tree of triples.

At each node of `pol`'s topology, we compile to a control as follows:

- `state` is a pair `(node_state, slot_state list)`.
  The `node_state` carries `D`'s per-node bookkeeping (an `RR` cursor, a `WFQ` global virtual time), seeded by `init_node_D()`.
  The `slot_state` list carries per-arm bookkeeping, one entry per child arm in slot order (a `WFQ` arm's weight and virtual finish; a `Strict` arm's priority rank), each entry seeded by `init_slot_D`.
  Disciplines without per-arm bookkeeping (`RR`) have an empty `slot_state` list.
- `pifo` is an empty PIFO: an index-PIFO at an internal node, a packet-PIFO at a leaf.
- `z` is `D`'s ranking program at the node. It maps the local `state` and an incoming packet to one path segment plus an updated `state`. The shape of that segment differs between internal nodes and leaves:
  - at an internal node, `z : state × Pkt ⇀ (idx × rank) × state`: pick a child index `i` and the rank `r` with which to enqueue `i` at this node's index-PIFO;
  - at a leaf, `z : state × Pkt ⇀ rank × state`: pick the rank `r` for the packet's own PIFO entry.

  When `z` is undefined for a packet, the per-node action is empty: nothing is enqueued at this node and `state` is unchanged. The global consequence (the walk halts, the packet is dropped from the system entirely) is a property of how per-node `z`s are composed, made precise in the FA-compatibility paragraph below.

We address nodes by `path` (§3.3): the local triple at the node reached by following `path` from `C`'s root is written `C@path`, with fields `C@path.state`, `C@path.pifo`, `C@path.z`.
We also write `C@path.node_state` and `C@path.slot_states` for the two components of `C@path.state` (with the plural `slot_states` reflecting that it is a list, one entry per child arm), and `C@path.designated` for the flag on an internal node.

##### Well-formedness: `|- C`

In §3.1 we defined well-formedness on a PIFO tree, written `|- q`. Now we redefine it, lifting it to act on a control `C`. A control `C` is _well-formed_ (written `|- C`) when, at every internal node of `C`, the `pifo` has, for each legal child index `i`, exactly as many occurrences of `i` as there are packets stored in the leaf pifos of the subtree under the `i`-th child. This is stated directly on `C`: no global PIFO tree need be assembled to check it.

##### Compatibility with Formal Abstractions

FA's controls are a single triple `(s, q, z)` with a state map `s`, a PIFO tree `q`, and a single transaction `z : St × Pkt -> Path(t) × St` (total).
Our `C` flattens into such a triple by gluing the pieces together: the FA-style tree `q` is the tree of our `pifo` pieces; the FA-style state `s` collects the `state` pieces indexed by path; the FA-style scheduling transaction `z` walks the topology applying each per-node `z` in turn and appending the emitted path segments into paths.
The partiality that our per-node `z`s allow shows up as partiality on the FA-style global `z` (a drop anywhere along the descent leaves the global function undefined for that packet).
The rest of the paper has no need for gluing a control together in this way (`|- C` is stated directly per the previous paragraph, and the diff rules of §3.4 act node-locally), but a reader more at home in FA's framing can recover it in this way.

##### Equivalence modulo pushes and pops: `~`

`push` and `pop` change the live `pifo`s and `state`s of a control but leave its structural skeleton untouched.
We write `C ~ C'` for the equivalence relation on well-formed controls that identifies any two controls related by a finite sequence of `push` / `pop` operations.

##### Reorder-congruence on `pol`: `=R`

We write `p =R p'` for the smallest congruence on `pol` such that, at any internal `D`-node, permuting the child arms gives a congruent pol: `D(p_1, ..., p_n) =R D(p_{σ(1)}, ..., p_{σ(n)})` for any permutation `σ`.
The per-arm metadata that discipline `D` requires (a `WFQ` weight, a `Strict` priority rank) travels with its arm under the permutation; the metadata is what carries the scheduling-meaningful content, so a permutation does not change the scheduler.
From now on, we state equality on `pol`s using `=R`.

##### Equivalence on controls modulo presentation: `~R`

Two controls whose child lists at some internal node are permutations of one another (with the parent's `pifo` and `z` renumbered accordingly) present different positional layouts but realize the same scheduler.
We write `C ~R C'` for the equivalence obtained by closing `~` under such sibling permutations.
Every `~`-equivalent pair is `~R`-equivalent; the converse fails.
The R suffix marks "closure under sibling reorder" uniformly across relations: `=R` is `=` with R-slack and `~R` is `~` with R-slack.
We will typeset the `R` as a subscript in print.

##### The bridge: `⌊·⌋`

We write `⌊C⌋` to mean "the `pol` that `C` realizes".
`⌊·⌋` is pinned down by three rules that propagate from compilation outward:

1. _Base case (compilation)._ `⌊compile(p)⌋ =R p`. The compiler is free to pick any sibling order when laying out `C`; `⌊C⌋` reads off the order the control _actually_ presents, and that representative is the `pol` `p'` such that `⌊compile(p)⌋ = p'` (note, literal equality). The pipeline echoes `p'` back to the operator.
2. _Closure under pushes and pops._ If `C ~ C'`, then `⌊C⌋ = ⌊C'⌋`. Pushes and pops touch only live `state` and `pifo` contents; they leave the topology and `z` of every node verbatim, so the pol-level skeleton that `⌊·⌋` names is untouched.
3. _Closure under diffs._ `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`, where `den(δ)` is the per-production recursion on `pol` defined in §3.4. We need the `=R`-slack in rules 1 and 3 because the compiler has arm-order freedom both times.

The three rules together let us propagate `⌊·⌋` from any `compile(p)` along any sequence of pushes, pops, and diffs.
This is how we will discharge Obligation 1 of §1: telling the operator what `pol` is running even when no user has explicitly requested the `pol`.

The interplay of the three rules is captured by the following diagram.

```
       p1  -------------den(δ)------------> p2
       |                                     |
    compile                               compile
       |                                     |
       v                                     v
       C1 ~ C1'  -------[[δ]]------> C2' ~R C2
```

The unprimed controls on each side (`C1`, `C2`) are freshly compiled with no traffic; the primed ones (`C1'`, `C2'`) are live, with whatever `pifo` contents and accumulated `state` have built up by then.
Let us study this diagram with an eye to the user's experience.
Our final goal will be to correctly relate `C2'` and `C2`.

- The operator writes `p1`; `compile` produces `C1`, with `⌊C1⌋ =R p1` by rule 1.
- Not shown in this diagram is that we read off `⌊C1⌋` to find the `pol` `p1'` such that `⌊C1⌋ = p1'` (note, this equality is _not_ modulo reordering) and we echo `p1'` back to the user.
- Push and pop operations carry `C1` to `C1'`. `C1 ~ C1'` by the definition of `~`, and `⌊C1'⌋ = ⌊C1⌋` by rule 2, so the live `C1'` still realizes both `p1` and `p1'`.
- The operator writes `p2`; the sniffer (§4) produces a `δ` such that `den(δ)(p1') =R p2`. It is key that we work in the frame of the actually-running representative `p1'` rather than the operator's original `p1`, since `den` has semantically meaningful paths.
- Applying `[[δ]]` to `C1'` lands at `C2'`, and by rule 3 `⌊C2'⌋ =R den(δ)(p1')`. Chaining these, we get: `⌊C2'⌋ =R p2`.
- We again read off `⌊C2'⌋` to find the `pol` `p2'` such that `⌊C2'⌋ = p2'` (note, this equality is _not_ modulo reordering) and we echo `p2'` back to the user.
- `C2 = compile(p2)` is the control we would have built had we taken the SOTA stop-the-world path; we do not actually construct it, but it is the correct reference point and it is crucial that we now relate `C2'` to `C2`. By rule 1, `⌊C2⌋ =R p2`, hence `⌊C2'⌋ =R ⌊C2⌋`. The right-hand link we write is `C2' ~R C2`, which absorbs two gaps at once:
  - `C2'` carries the live `pifo`/`state` accumulated since `C1` while `C2` is freshly compiled and bare. The `~` component covers this.
  - `[[δ]]` and `compile` are free to pick different sibling orders at internal nodes, so `C2'` and `C2` may also differ in child arrangements. The R-closure covers this.

The diagram thus commutes at the level of `⌊·⌋` modulo `=R`: both routes from `p1` to a control realizing `p2` land in the same `~R`-class. The commutation holds verbatim for `pol`-visible diffs and informally for pol-invisible ones like `Quiesce` (whose `z`-domain restriction is not reachable by any push or pop of `C2`).

When writing `p2`, the operator would do well to state their request against `p1'`, the actually-running `pol` that we echo back to them.
This keeps the diff small and aligns paths with what is running.
They may state it against the unprimed `p1` they originally wrote, but at their own risk. The risks are of two flavors:

- The diff sniffer may infer a larger diff than necessary, or may give up.
- The more serious issue is that the user may use Imperative Mode (§4) to directly specify what edits to make, and if they use the unprimed `p1` to base their paths, they may inadvertently touch the wrong node, give a malformed path, etc.

##### A worked example

The operator initially requests `p1 = Strict(B_hi, A_lo)`: strict priority with `B` on top of `A`.
The compiler uses its degree of freedom to yield control `C1` with `⌊C1⌋ = Strict(A_lo, B_hi)`. Note that the children have been sorted.
We echo back `p1' = Strict(A_lo, B_hi)`.
`p1 =R p1'`, so the scheduler the operator gets is the one they asked for; only the slot numbering differs.

Later, the operator requests `p2 = Strict(A_lo, C_mid, B_hi)`: a three-arm strict priority.
The runtime can again use its freedom.
Instead of literally splicing `C` in between running arms `A` and `B`, it chooses to append `C` to the end.
After the diff is applied, the running control realizes `p2' = Strict(A_lo, B_hi, C_mid)`, which we echo back.

Now the operator changes to Imperative Mode (§4) and writes the path-bearing edit `(True, Quiesce([2]))`.
It is not worth getting distrated by the syntax here; the key thing is that the operator has requested an edit and has identified the target via a path.
Paths are interpreted against the _actually running_ representative `p2'`, so the edit affects `C_mid` (`p2'`'s slot 2), not `B_hi` (`p2`'s slot 2).
If the operator had based the path on `p2` they would have edited the wrong arm; the system has no way to detect or recover from that.

### 3.3 A Grammar for Tree Diffs

§3.2 set up the operator-system loop: the operator writes `p1`, we compile and echo back `p1' = ⌊C1⌋`, and pushes and pops carry the running control to some `C1'` still realizing `p1'`.
When the operator asks for a new `p2`, SOTA would compile `p2` into a fresh `C2` and clobber `C1'` with it.
We want the same user-observable result while being less disruptive to unaffected parts of the running control.

A change to the live control is _atomic_ when its effect falls between two observable operations: in any sequence of `push`/`pop` operations `op_1, op_2, ...` served by the scheduler, if the change fires between `op_N` and `op_{N+1}`, then `op_1, ..., op_N` are served entirely by the pre-change control and `op_{N+1}, ...` are served entirely by the post-change control.
No operation straddles the change, and no operation sees an intermediate state.
§1's running example described this informally.

We fix a small grammar of atomic edits.
Each production denotes a single primitive that acts on a live control `C` and produces a new control.
The grammar in this section is the alphabet from which the transition planner (§4) assembles a sequence whose operational composition takes the presently running control `C` (which realizes `pol` `prev`) to a new control realizing the user's next-requested `pol` `next` _without clobbering_ `C`.
Most productions are `pol`-visible: their effect shows up in `next`, and a comparison of `prev` against `next` is enough to understand them.
Others are transaction-only: their effect lives entirely in `z`, leaving the `pol`-level skeleton untouched.

An edit names _where_ in the tree the change lands (a path from the root) and _what_ the change is.
We write `prev@path` for the subtree of `prev` reached by following `path` down from its root.

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
meta?  ::= ε  |  priority-rank  |  weight   // absent for RR, priority for Strict, weight for WFQ
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

- `Add(path, pol, meta?)` appends `pol` as a new arm under `prev@path`.
  Since arm order at any internal node is `=R`-irrelevant (§3.2), we have the freedom to just _append_ the new arm, leaving other arms undisturbed.
  The third argument `meta?` is the new arm's per-arm metadata: a weight for a `WFQ` parent, a priority rank for a `Strict` parent, absent (`ε`) for an `RR` parent.
  `init_slot_D` reads `meta?` when seeding the new arm's `slot_state` (§3.4.1).
  Add is _non-disturbing_: it writes only the new arm's `slot_state`; every existing arm's `slot_state` is preserved verbatim.
  The new arm carries its own `meta?`, leaving the others' untouched.
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
  It is well-defined only when `path` is non-empty and each internal node strictly above `prev@path` has a single arm (the one continuing toward `prev@path`), so the discarded ancestor chain carries only scheduling metadata, no traffic.
  The chain may have been shaping `prev@path`'s pop order, and `ChangeRoot` discards that influence.
  The richer reconfiguration of pruning a tree down to a subtree that originally shared ancestors with packet-bearing relatives is realized as the `PruneDownTo` idiom (§4), which first drains and `Remove`s those off-path subtrees, reducing the chain above `prev@path` to the unary shape `ChangeRoot` requires.

A `Strict*` node is one introduced by `Designate`, a plain `Strict` is compiled from user input.
Operationally we model the distinction as a one-bit `designated` flag that each node carries.
`Designate` sets it, `Undesignate` clears it.
The star is just shorthand for that bit being set, not a separate discipline.
Semantically the two are identical; every push, pop, and well-formedness check treats `Strict*(A, B)` exactly as `Strict(A, B)`.
The star exists only so that `Undesignate`'s precondition ("path lands on a `Strict*`") and the hardware story in §6 ("`Strict*` adds no PE depth") can be stated structurally.
The §3.2 DSL that the operator writes can parse only `Strict`, never `Strict*`, so a `Strict*` is unreachable in any user-written `pol` and arises only in the middle of a planner sequence, between a `Designate` and its eventual `Undesignate`.

When `prev = next` the grammar emits no diff at all: the reconfiguration is the empty sequence (§4), and the live control is left untouched.

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
This is what the operator means when they say "after this diff, the running pol is <this>"
For each production we state `den(δ)` in closed form and prove the following informal characterization from the operational rule.

> _Characterization._
> Assume `⌊C⌋` is defined and `[[δ]](C)` is defined.
> Then `⌊[[δ]](C)⌋ =R den(δ)(⌊C⌋)`.

In words: applying a diff to a live control yields a new control whose running pol is the one the operator intended.
This is rule 3 of `⌊·⌋` (§3.2), made concrete for each production by the closed-form `den(δ)`.
Working at this level, §4's planner searches for a sequence of edits that, when viewed via `den` and composed together, carries `prev` to `next`.

`den(δ)` is partial for the same syntactic reasons that `[[δ]]` is (paths must resolve, leaf labels must be fresh, weights must match), but, because `pol` carries no live contents or transaction, it never fails for operational reasons like `Remove`'s emptiness or `Add`'s classifier disjointness.
Effects that live entirely in `z` are pol-invisible: `den(Quiesce) = id_pol`, so by rule 3, `⌊[[Quiesce]](C)⌋ = ⌊C⌋`; the running pol after a `Quiesce` is unchanged, even though `[[Quiesce]]` has done real work on `z`.
The `designated` bit on a `Strict*` node is likewise pol-invisible (it is operational, not part of any `pol`), so `den(Designate(path, B))` wraps `pol@path = A` into `Strict(A, B)`, and `den(Undesignate(path))` consumes a `Strict(A, B)` at `path` and yields `B`.
Composed in that order, they act as a replacement at `path`.

_Atomicity_ can be restated crisply: when `δ` is compatible with `C`, `[[δ]]` replaces the live control `C` with `[[δ]](C)` between two consecutive `push`/`pop` operations.
Modeling `[[δ]]` as a single (partial) function `control ⇀ control` bakes indivisibility into the abstraction; the obligations below ensure that the result of this single step is correct.

For each production we state `den(δ)`, prove the characterization theorem above, and discharge two further obligations, all assuming `[[δ]](C)` is defined.
Throughout, we write `C` for the pre-edit control and `C'` for the post-edit control.

- _Pol-level characterization_ pins down the structural skeleton up to `=R`: the closed-form recursion `den(δ)`, plus the proof that `⌊C'⌋ =R den(δ)(⌊C⌋)` whenever both sides are defined. `den` returns a definite pol (a definite representative of an `=R`-class); the implementation is free to lay out `C'` so that `⌊C'⌋` is any other representative of the same class.
  For our running example, `δ = Add([], spotify, mid)`, the closed form gives `den(δ)(Strict(gmail, zoom)) = Strict(gmail, zoom, spotify)`, and the theorem says `⌊C'⌋ =R Strict(gmail, spotify, zoom) = next` for any `C` with `⌊C⌋ =R Strict(gmail, zoom)`.
  Note that we are only `=R`, not `=`, to the user's request, but that is okay.
  Transaction-only effects (e.g., `Quiesce`'s shrinking of `z`'s domain) lie outside the scope of `den` and are fixed by the per-production operational rule below, not by the characterization.
  The characterization says nothing about contents.
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
  - `C'@π.z` extends `C@π.z` to admit packets that classify into the new subtree, mapping them to child index `k` at `π` (with descent handed off to the new subtree's `z` pieces below). Pre-existing mappings are untouched.
- _At every node inside the new subtree:_ the local control is what §3.2's compiler produces for that node.

##### Characterization

The pol-level effect of `Add(path, pol, meta?)` is the structural map on `pol`, given by recursion on `path`:

```
den(Add([],        pol, meta?)) (D ts) = D ( ts ++ [pol] )
den(Add(i :: rest, pol, meta?)) (D ts) = D ( ts[ den(Add(rest, pol, meta?)) (ts[i]) / i ] )
```

The base case applies once `path` has walked down to the new arm's parent: `pol` is appended as the last child.
Recall from §3.3 that `Add`'s `path` resolves in `prev` and names the new arm's parent, so the base case lands at that parent.
The recursive case walks one step deeper into child `i` and writes the result back in place.
The `meta?` argument is threaded through the recursion but does not appear in the closed form: it is per-arm bookkeeping consumed by `init_slot_D` at the operational level (§3.2's note that pol's grammar carries no structural mark of meta), so it is pol-invisible.

_Proof of characterization._
We argue that the structural skeleton of `C'` matches `den(Add(path, pol, meta?))(⌊C⌋)`, which justifies rule 3 of `⌊·⌋` (§3.2) for this production.
The operational rule above leaves every pre-existing arm structurally intact (the `z` extensions along the ancestor chain are pol-invisible) and adds a new arm at `π` whose subtree is exactly what §3.2's compiler emits for the operator-supplied `pol`.
Walking `path` from the root through `⌊C⌋`, this matches the recursion: at each proper ancestor we recurse into the child on the path; at `π` we append `pol` to the child list, exactly as the closed form prescribes.
Equality is on the nose, but we state the characterization mod `=R` for uniformity with the other productions.
So `⌊C'⌋ =R den(Add(path, pol, meta?))(⌊C⌋)`.

Our running example computes `den(Add([], spotify, mid)) Strict(gmail, zoom) = Strict((gmail, zoom) ++ [spotify]) = Strict(gmail, zoom, spotify) =R Strict(gmail, spotify, zoom) = next`, as intended.
The closed form appends `spotify` at the end, while the operator wrote it in the middle slot.
Both layouts are `=R`-equivalent: priority lives in each arm's `meta?` (§3.2), not in the slot index, so the runtime is free to lay out the children in any slot order it likes. The realized control is `Strict(gmail_hi, zoom_lo, spotify_mid)`, with `spotify` appended at slot 2 even though its priority sits between `gmail`'s and `zoom`'s.
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

#### 3.4.2. `Remove(path)`

`Remove` is the grammar's one structural deletion.
It unhooks the subtree `prev@path` from its parent, drops the vacated slot, and renumbers any higher siblings.
It is defined only when `prev@path` is _empty_.
Retiring or replacing a subtree that still holds packets is therefore not `Remove`'s job alone: it is realized as a _sequence_ that first drains the subtree and only then removes it (see §4).

##### Operational transition

Let `path = π ++ [k]`, where `π` is the path to the parent of the removed subtree and `k` is the slot index at that parent.
Let `D` be the discipline at `π`.
The transition `C' = [[Remove(path)]](C)` is defined when (i) `path` is non-empty and resolves to a node in `C`, (ii) the subtree at `path` is empty, and (iii) `C@path` does not carry the `Strict*` flag (the planner reaches a `Strict*` only through `Undesignate`, §3.4.4).
We state it per node.

The topology loses the arm at slot `k` of `π`; arms at slots `0, ..., k-1` keep their indices; arms at slots `k+1, ...` shift down by one.
The local controls update as follows.

- _At every node outside the removed subtree, other than `π`:_ the local control is preserved verbatim.
  This includes every proper ancestor of `π` (whose `z` is unchanged) and every sibling arm at `π` other than slot `k`, together with the subtrees under them.
- _At `π`:_
  - `C'@π.node_state = C@π.node_state` (unchanged).
  - `C'@π.slot_states = C@π.slot_states[-/k]`: the entry at slot `k` is dropped; entries at slots `> k` shift down by one to track the renumbered arms.
  - `C'@π.pifo = C@π.pifo[-/k]`: the precondition (subtree at `path` empty) plus `|- C` forces `C@π.pifo` to contain no entry equal to `k`, so this renumbering deletes no values; it only decrements entries `> k` by one to track the renumbered arms.
  - `C'@π.z` is restricted on inputs and renumbered on outputs. Packets that `C@π.z` would have routed to slot `k` are no longer in `C'@π.z`'s domain; for surviving inputs, an output of `(i, r)` with `i > k` becomes `(i - 1, r)`. Slots `< k` are untouched on either axis.

A standalone `Remove` thus restricts classification at `π` so that packets bound for the removed subtree are dropped before they reach a non-existent slot.
In the typical planner usage (`Retire` of §4.1), a preceding `Quiesce` has already restricted the root's `z` on these packets, so they never descend as far as `π`; the input restriction at `π` is then redundant but harmless, while the output renumbering at `π` is still needed.

##### Characterization

The pol-level effect of `Remove(path)` is the structural map on `pol`, given by recursion on `path`:

```
den(Remove([k]))         (D ts) = D ( ts[-/k] )
den(Remove(i :: rest))   (D ts) = D ( ts[ den(Remove(rest)) (ts[i]) / i ] )      when rest is non-empty
```

The base case fires once `path` has been walked down to the parent of the removed arm: the `k`-th child is dropped from the arm list.
`Remove`'s `path` resolves in `prev` and names the subtree being deleted, so the recursion bottoms out one step shallower, at that subtree's parent.
The recursive case walks one step deeper into child `i` and writes the result back in place.

`den(Remove(path))` is defined when `path` is non-empty and resolves in the input pol.
The emptiness precondition that `[[Remove(path)]]` imposes is operational, not visible at `pol` level.

_Proof of characterization._
We argue that the structural skeleton of `C'` matches `den(Remove(path))(⌊C⌋)`, justifying rule 3 of `⌊·⌋` (§3.2) for this production.
The operational rule above leaves every node outside the removed subtree structurally intact: every proper ancestor of `π` is preserved verbatim (the `z` is untouched), as is every sibling arm at `π` other than slot `k`.
At `π`, the child list shrinks by dropping slot `k`.
Walking `path` from the root through `⌊C⌋`, this matches the recursion: at each proper ancestor we recurse into the child on the path; at `π` (where the remaining path has length one) we drop the named child from the arm list, exactly as the closed form prescribes.
Equality is on the nose, but we state the characterization mod `=R` for uniformity with the other productions.
So `⌊C'⌋ =R den(Remove(path))(⌊C⌋)`.

##### Soundness

`|- C` gives `|- C'`.

At `π`: by `|- C`, the count of `k`-entries in `C@π.pifo` equals the number of packets stored under slot `k`, which the precondition fixes at zero, so `C@π.pifo` contains no entry equal to `k`.
The renumbering `[-/k]` therefore deletes no values from `C@π.pifo`; it only decrements entries `> k`.
For each surviving slot `i' < k` of `C'@π`, the matched count is inherited from slot `i'` of `C@π`: the subtree is the same and the pifo entries equal to `i'` were left alone by `[-/k]`.
For each surviving slot `i' >= k` of `C'@π`, the matched count is inherited from slot `i' + 1` of `C@π`: the same subtree, with the pifo entries that were `i' + 1` now renumbered to `i'`.

At every proper ancestor of `π`, the local pifo and local child subtrees are unchanged in packet count.
The subtree under the relevant child of `π` lost zero packets (the removed subtree was empty), so the ancestor's count for the child it forwards through is exactly what it was.
No ancestor pifo is rewritten.

At every other node, the local control is preserved verbatim, and its well-formedness count is inherited.

##### State preservation

Outside the edit site the local control (and thus its `state`) is preserved verbatim, including at each proper ancestor of `π`, where nothing changes at all, and at every surviving sibling of slot `k`, with the full subtree under it.
At `π`, no `init`-rule fires: `Remove` introduces no new arm or node, so there is nothing to seed.
`node_state` is preserved verbatim.
`slot_states` drops the entry at slot `k`; every other entry is preserved verbatim, with its position shifted to match the new arm order.
This is "state preservation" for a deletion: surviving siblings keep what they had.

##### Notes

_Atomicity._
No in-flight packet straddles the diff.
The precondition forces the subtree at `path` to be empty at the diff instant, so no packet sits in the about-to-be-deleted region.
Every other packet sits in some pre-existing `pifo` that survives into `C'`, at the same slot index (if `< k`) or one slot lower (if `> k`); each such packet is held under the same surviving subtree as before, just at the renumbered position.
Every surviving `pifo` entry similarly points to the same child subtree as before, after renumbering.
A `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
The restriction at `C'@π.z` comes into play only for `push`es that arrive after the diff fires.

_Deeper paths._
The walkthrough above is stated for an arbitrary `path`.
The descent from the root to `π` passes through ancestors whose local control is untouched; the edit is confined to `π` and the (vanished) subtree at slot `k`.
Removing a deeper subtree differs only in how far we walk before reaching `π`.

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

#### 3.4.3. `Quiesce(path)`

`Quiesce` is the grammar's sole tool for halting traffic into a subtree.
It does not modify the subtree's structure or its contents.
The patch lives at the root: the root's `z` is altered so that any packet bound for any leaf under `prev@path` is refused admission.
Topology, disciplines, slot states, and pifo entries are all unchanged, so `Quiesce` is `pol`-invisible.

##### Operational transition

Let `r` denote the root, i.e., the empty path `[]`.
The transition `C' = [[Quiesce(path)]](C)` is defined when `path` is non-empty and resolves to a node in `C`.
There is no precondition on the subtree's contents.
We state the transition per node.

The topology, the discipline at every node, and every arm's slot index are unchanged.
The local controls update as follows.

- _At every node other than the root:_ the local control is preserved verbatim.
  This includes every proper ancestor of `path` other than the root, the node `C@path` itself, and every descendant of `C@path`.
- _At the root:_
  - `C'@r.node_state = C@r.node_state` (unchanged).
  - `C'@r.slot_states = C@r.slot_states` (unchanged).
  - `C'@r.pifo = C@r.pifo` (unchanged): no pifo entry is rewritten, and any entry currently pointing toward the quiesced subtree continues to be honored on `pop`.
  - `C'@r.z` restricts `C@r.z` on its inputs: packets whose classifier predicate matches any leaf label under `prev@path` are no longer in `C'@r.z`'s domain. For surviving inputs, the output is `C@r.z`'s output verbatim.

The restriction at the root suffices because every descent begins there: a packet that the root refuses is never offered to any deeper `z`, so no ancestor's `pifo` ever acquires a new index pointing toward the quiesced subtree.

##### Characterization

`Quiesce` is `pol`-invisible: its entire effect lies in `z`'s domain, which `pol` does not record.
The closed form is the identity:

```
den(Quiesce(path)) p = p
```

defined whenever `path` is non-empty and resolves in `p`.

_Proof of characterization._
We argue that `⌊C'⌋ = ⌊C⌋`, justifying rule 3 of `⌊·⌋` (§3.2) for this production.
The operational rule above leaves the `node_state`, `slot_states`, `pifo`, and child list at every node (including the root) structurally intact; only the root's `z` is restricted, and `z` is not visible at `pol`-level.
Walking any path from the root through `⌊C'⌋`, every discipline and every arm matches `⌊C⌋` exactly.
Equality is on the nose, so `⌊C'⌋ =R ⌊C⌋ = den(Quiesce(path))(⌊C⌋)`.

As §3.4's preface anticipated, this is the showcase case of a diff whose real semantic content lives entirely in `z`: the characterization theorem honestly reports it as a no-op on `pol`, because nothing structural moved.

##### Soundness

`|- C` gives `|- C'`.
Every node's `node_state`, `slot_states`, `pifo`, and child subtree are preserved verbatim, so every well-formedness count at every node is inherited from `C`.
The root's `z` restriction touches no `pifo` entry and no stored packet; it only refuses future `push`es.
Nothing needs repair.

##### State preservation

Outside the edit site every local control is preserved verbatim.
At the root, `node_state` and `slot_states` are unchanged; only `z` is restricted, and `z` is not part of `state`.
No `init`-rule fires anywhere: `Quiesce` neither spawns nodes nor introduces new arms.

##### Notes

_Atomicity._
No in-flight packet straddles the diff.
Every `pifo` at every node is preserved verbatim, with every packet still held under the same subtree at the same slot index.
A `pop` immediately after the diff returns exactly what a `pop` immediately before would have.
The restriction at `C'@r.z` comes into play only for `push`es that arrive after the diff fires.

_Deeper paths._
The argument does not depend on the depth of `path`.
A deeper `Quiesce` simply names a subtree closer to the leaves; the root's `z` is restricted on the same set of leaf-label predicates either way, since classification flows top-down from the root.

_Why patch the root, not the immediate parent?_
Classification is a top-down walk: every packet that would have descended through any ancestor of `prev@path` first passes through the root's `z`.
Restricting the root is therefore the smallest change that halts every relevant packet.
If we just rejected packets at `prev@path`'s root, the ancestors of `prev@path` would still enqueue indices relevant to the packet pointing eventually to `prev@path`.
This is a recipe for ill-formedness!
So we would then need to walk up the entire ancestor chain, chaning `z`s along the way.
Editing the root of the whole scheduler is a very neat solution in comparison.

_Cooperates with `Remove`._
`Quiesce` does not by itself delete or drain the subtree; it only stops new traffic.
The natural sequence `Quiesce(path); let the subtree at prev@path drain to empty; Remove(path)` is the `Retire` idiom of §4.1: after the `Quiesce`, no new packet enters `prev@path`; the drain consumes the packets already there through ordinary `pop` operations; and the `Remove` is then defined, since `prev@path` is empty.
`Quiesce`'s restriction of the root's `z` makes the input restriction at `π` that `Remove` performs (§3.4.2) redundant in this sequence, but `Remove`'s output renumbering at `π` is still needed.

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

Two authoring modes produce sequences, and the operator chooses freely between them.
In _declarative mode_, the operator writes a `pol` and, to reconfigure, writes a second `pol`; a differ proposes a sequence, and the operator either accepts it (in which case we apply the sequence to the running control) or declines it.
The differ is intentionally simple: it sees only pol-level diffs whose translation to a sequence is straightforward, and falls back to the generic `Designate([], next) ; Undesignate([])` pair (§5) for anything richer.
Multi-step reconfigurations with operator choice (e.g., `Retire` vs. `SlowRetire` below), graft-style local edits the differ cannot infer, and other confined strategies are outside its scope.
In _imperative mode_, the operator writes both their desired `next` and a `(φ; δ)*` sequence intended to reach it, drawing on a small vocabulary of _idioms_ (§4.1) and on raw atomic diffs.
Before running the sequence we check it for the operator at the pol level: we fold each diff's `den(δ_i)` (§3.4) along the sequence, producing a chain `⌊prev⌋ -[den(δ_1)]-> p_1 -[den(δ_2)]-> ... -[den(δ_n)]-> p_n`, and accept the request iff every `den(δ_i)` is defined on the intermediate pol it sees and `p_n =R next`.
Guards play no role in this check; they govern the operational timing of when each `δ_i` fires on the live control, and are pol-invisible (§3.4).
If the chain fails to reach `next`, or some `den(δ_i)` is undefined on its intermediate pol, we reject the request.

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

- **`PruneDownTo(path)`** = `Retire(p_1) ; ... ; Retire(p_m) ; (true; ChangeRoot(path))`, where `p_1, ..., p_m` are the off-path subtrees along the route from the root to `path`. This idiom is the operator-facing way to say "abandon everything except this subtree."

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

This paper's transition planner engages with one `prev -> next` pair at a time.
If the operator submits a follow-up `nextnext` while a `prev -> next` sequence is still mid-flight (i.e., while some guard `φ` has not yet become true), our answer is the simplest possible one: `nextnext` is queued and the transition planner does not begin work on it until the in-flight sequence completes.
See `paper/discussion-separable-nextnext.md` for a stronger possibility.

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
