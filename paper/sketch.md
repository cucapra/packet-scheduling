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
- What about transitioning to `SP(gmail, RR(zoom, spotify))`? It is not as easy. We atomically enter into a _transitionary period_ during which the scheduler still accepts and emits packets. After certain conditions are met, we atomically leave this transitionary period and enter into the user-requested policy.
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
- What vPIFO does not do. As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change. The configuration tables it relies on (the Operation Generation Table and the PIFO Instance Address Table) are described only for the initialization stage. The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- vPIFO's own §8 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly. We are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer _above_ a PIFO substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose.

### 2.3 Revisiting our running examples

- Moving from `SP(gmail, zoom)` to `SP(gmail, zoom, spotify)`. This is the easy case: the diff is an append, and the existing siblings' ranks are preserved. In principle, a substrate could install the new child without a transitionary period. In vPIFO's setup, this could be a targeted append to their Operation Generation Table and PIFO Instance Address Table, leaving the rest of the scheduler running. vPIFO as published cannot do this, but for a more basic reason than one might guess: it offers no runtime update mechanism at all, cheap or otherwise. Even when the user's intent is a single append, there is no path from "new SDL program" to "live hardware" that does anything finer than a full reinitialization. That is what our layer adds: given the (small) diff here, we can drive a compatible substrate to install it in place.
- Moving from `SP(gmail, zoom)` to `SP(gmail, RR(zoom, spotify))` is where the real problem lives. The diff is structural: an internal node appears between `SP` and `zoom`, and `zoom` is re-parented. No single atomic operation realizes this on hardware that maps depth-to-PE; some packets `zoom` are potentially already enqueued under the old shape. We must atomically enter a transitionary `link` policy that is well-defined on both old and new arriving packets, drop or recirculate the residue, and then atomically jump to the policy actually requested by the user.

## 3. Formalizing the Transition Phase `link`

### 3.1 Background: PIFO trees, formally

We build on the PIFO-tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3], so we review the pieces of their formalism that this paper actually leans on.

#### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO-tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

#### The two user-observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when admitting the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, and finally emitting a packet from the leaf. These are the _only_ user-visible interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of them: every push/pop is observed under exactly one of `prev` or `next`, never a half-edited intermediate scheduler.

#### Well-formedness

A PIFO tree `q` is well-formed (`|- q`) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck: an index `i` is enqueued in the parent _exactly when_ there is a packet underneath waiting to be released by it. Lemma 3.9 of _Formal Abstractions_ shows that `push` always preserves `|- q`, and that `pop` does too whenever `q` is non-empty (which is precisely the condition under which `pop` is defined). Our §3.4 proof obligation is the analogous statement one level up: any structural edit applied to a well-formed tree `prev` using the grammar from §3.3 yields a well-formed tree `next`.

#### Control

_FA_ factors the scheduling _policy_ (which, given an arriving packet and the current state, produces the path that `push` will follow) into a _control_ object. A control over `t` is a triple `(s, q, z)`: a current state `s` drawn from some fixed set, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z` that, given a state and a packet, returns a path of the right topology together with an updated state. We inherit this factoring without modification; the reconfiguration problem this paper tackles is the change of `q`'s topology, with the rest of the control updated in lockstep.

### 3.2 Extending the model

The model of §3.1 suffices to state our diff grammar, but the transition proofs of §3.4 lean on a small extension to it. We isolate such conveniences here, so that later sections can cite them rather than re-derive them in passing.

#### Tombstones

Sometimes we need to delete a child PIFO tree. If the child is nonempty, then the parent still has occurrences of `k` (the index used by the parent to refer to the deleted child) in its index-PIFO `p`. Expunging those occurrences is possible but expensive, since they sit scattered through `p` by rank. Further, removing such instances of `k` in the parent actually creates additional removal obligations in the parent's _own_ ancestors. Instead of getting into all this, we let a node carry a finite set `T` of _tombstone_ indices, disjoint from its live child indices, and relax the §3.1 invariant to match.

If a PIFO has index `k` but `k` is in that PIFO's set `T`, we call `k` a _phantom_. We call other indices _live_.

`pop` gains a single rule: if popping `p` yields a phantom, `pop` again. This keeps `pop` from getting stuck on a phantom, just as the §3.1 invariant kept it from getting stuck on an empty child.

A tombstone thus lets an edit delete a subtree in one transaction without rewriting its parent, at the price of a bounded number of phantom-pops.

#### Well-formedness modulo tombstones

We say `q` is _well-formed modulo tombstones_, written `|-_T q`, when at every internal node with index-PIFO `p`, children `qs`, and tombstone set `T`:

- for every _live_ index `i`, the occurrences of `i` in `p` equal the packets held under `qs[i]`, exactly as in §3.1; and
- every phantom index in `p` is also in `T`.

Two facts make the relaxation of `pop` harmless:

- _Phantoms only vanish._ A phantom-pop removes one occurrence of a tombstone index, and a `push` never adds one, because the transition that adds `k` to the tombstone set `T` also installs a new scheduling transaction that routes nothing new to `k`. So each node's phantom count is monotonically non-increasing.
- _Reclamation recovers `|-`._ Once a tombstoned index's phantom count hits zero, `p` no longer needs to name the index in `T`; the dead slot may be recycled and its higher-indexed siblings renumbered, leaving an ordinary `|-` tree. Until then `|-_T` is exactly the invariant we need, and `|-` is the special case with `T` empty on all PIFOs.

### 3.3 A Grammar for Tree Diffs

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

We make no claim that the sniffer is canonical or minimal; we claim only that whatever it returns is a sound description of the transition from `prev` to `next`, and that §3.4 will show every variant absorbs cleanly into a well-formed tree.

### 3.4 Edits Preserve Well-Formedness

Every transition from `prev` to `next` factors through a third regime that we name `link`:

```
prev  --atomic-->  link  --atomic-->  next
```

We call each atomic regime-change a _snap_. The two arrows are atomic in the §1 sense: every user-observable push/pop happens entirely under `prev`, `link`, or `next`, and never straddles a snap. `link` is itself a packet-scheduling regime over a well-formed PIFO tree, with its own `push`/`pop`. For many edits `link` is _degenerate_: zero-duration, sharing topology and contents with `next` at the instant the entry snap completes, so the two arrows fuse and `prev` abuts `next` directly. For others `link` is substantive, with real duration and stated `push`/`pop`, and the exit snap waits for some stated enabling condition.

We first make `snap`, `link`, and `transition` precise and state the conditions every transition must discharge (§3.4.1). We then instantiate the definition: `ArmAdded` as a warm-up, where `link` is degenerate (§3.4.2); the full menu for `ArmRemoved`, where it is substantive (§3.4.3); and the remaining variants after.

#### 3.4.1. Transitions, formally

The transitionary regime looks daunting only if we expect it to be a new kind of object. It is not. `link` is an ordinary control in the sense of §3.1: a triple `(s, q, z)` with the `push`/`pop` already defined there. What is new is not a semantics but the way two controls are spliced in time, and that is what we now pin down.

##### Snap

A _snap_ `σ` atomically replaces the live control with another, between two user operations, so that every operation is served by exactly one control, by construction. A snap may move, synthesize, or drop contents, but we require it to be _sound_: `|-_T C` implies `|-_T σ(C)` (the modulo-tombstones judgment of §3.2; for a tombstone-free control this is just `|-`).

##### Transition

A _transition_ realizing a diff, from the running control `C_prev` to the target `C_next`, is a tuple `(σ_in, L, φ, σ_out)`:

- an _entry snap_ `σ_in`, with link control `L = σ_in(C_prev)`;
- the _link control_ `L`, run under the `push`/`pop` of §3.1, evolving without our intervention to some `L'` as packets flow;
- an _exit condition_ `φ`, a predicate on `L`'s state and, optionally, on an external clock;
- an _exit snap_ `σ_out`, fired at the first instant `φ` holds, with `σ_out(L') = C_next`.

##### Soundness

The transition is _sound_ when its four _conditions_ hold:

- (i) _entry-soundness_, so that `|- C_prev` gives `|-_T L`
- (ii) _closure_, so that `L`'s own `push`/`pop` preserve `|-_T L`
- (iii) _exit-soundness_, so that `|-_T L'` gives `|-_T C_next`
- (iv) _liveness_, that `φ` eventually holds.

Conditions (i)-(iii) are _safety_, keeping the live scheduler well-formed modulo tombstones at every instant; liveness (iv) is what drives the transition to `next`. Exit-soundness lands us in `next` well-formed modulo tombstones; where the exit leaves phantoms, §3.2 reclamation upgrades `|-_T C_next` to plain `|- C_next` within bounded time, so we reach `next` truly, not merely observably.

##### Degenerate `link`

When `L = C_next` (exactly, or modulo tombstones that §3.2 reclaims in bounded time) and `φ = true`, the exit snap is the identity and fires at once: the two snaps fuse, and `prev` abuts `next` with no observable transitionary regime. Closure (ii) and liveness (iv) are then vacuous, and entry-soundness (i) and exit-soundness (iii) collapse into one well-formedness check. See `ArmAdded` (§3.4.2; no tombstones) and `ArmRemoved` Choice 1 (§3.4.3; modulo tombstones).

##### The menu

A diff does not determine its transition. In general many tuples `(σ_in, L, φ, σ_out)` realize the same `prev`-to-`next` change at different costs in lossiness and latency, and each is independently sound. We call this set of sound transitions the _menu_. SOTA offers exactly one item, a whole-tree stop-the-world `link`; §4 is largely about choosing more intelligently from the menu.

##### Theorem (the transitionary period is just scheduling)

For every diff variant, and every realization given in §3.4.2 onward, the tuple `(σ_in, L, φ, σ_out)` is a sound transition. Hence at every instant the live scheduler is a well-formed control (`|-` of §3.1, or `|-_T` of §3.2 where a realization tombstones), and the user-observable trace is a sequence of ordinary `push`/`pop` operations served first by `C_prev`, then by `L`, then by `C_next`. The "semantics of `link`" is therefore nothing more than the §3.1 semantics of the control we install; each variant's job is to supply the four components and discharge the four conditions.

#### 3.4.2. `ArmAdded(path, arm, weight?)`

Example: `SP(gmail, zoom)` --atomic--> `SP(gmail, zoom, spotify)`.
The diff computed according to the grammar in §3.3 is: `ArmAdded { path = [2]; arm = spotify }`.
By the grammar of §3.3, `ArmAdded` means an arm is _truly_ added: the new arm carries fresh traffic that intersects nothing `prev` was already serving (it was being rejected thus far, say). No `prev`-era packet therefore belongs under the new arm, the `link` is degenerate, and the two snaps fuse into one.

We work entirely with the control triple `(s, q, z)` of §3.1 and develop the new control `(s', q', z')` that the snap installs. We write `c` for the parent node named by `path` (here the root `SP`) and `k` for the index of the new slot (here `2`). We take the append case first, where `spotify` lands at the end of the child list, and handle a mid-order insert afterward.

_The tree, `q -> q'`._ At `c = Internal(qs, p)` we append the new arm to the child list, so its slot `k` is the new last index: `q' = Internal(qs ++ [a], p)`. Here `a` is the _empty_ PIFO tree having whatever topology `arm` described. The parent's index-PIFO `p` is left exactly as it was, because it only ever named the old indices `0..k-1` and so does not name `k` at all.

_The transaction, `z -> z'`._ `z'` is the scheduling transaction compiled from `next`. It is a _conservative extension_ of `z`: for any packet that does not classify into the new arm it defers to `z`, and for a packet that does classify into the new arm it returns a path whose step at `c` selects `k` and then descends through the PIFO tree `a`. Because `k` was not a legal index under `prev`, `z` could never have emitted such a path, so the two transactions differ only on routes that land in the previously-nonexistent subtree.

_The state, `s -> s'`._ `s'` agrees with `s` everywhere, except that it records the initial local state for the new slot: the new arm's own scheduling state at its from-scratch value (it is empty), plus whatever per-slot bookkeeping `c`'s scheduler keeps (an RR cursor, the slot's weight taken from the edit's `weight?`, a virtual-finish accumulator, etc.). No existing slot's state is disturbed.

_The snap is atomic, and `link` is degenerate._ Two facts do the work.

- First, `q'` is well-formed. The parent `c` has exactly zero occurrences of `k` (its index-PIFO `p` does not yet feature `k`, as established above) and the new arm `a` holds zero packets or indices, so the well-formedness obligation (§3.1) reads `0 = 0`. Every other slot is the same child it was in `q`, with the same packets beneath it and the same entries in `p`, so its obligation is inherited verbatim from `|- q`. So `q'` needs no repair.
- Second, no in-flight packet straddles the snap: every packet resident at the snap instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`.
  - A `pop` issued immediately after the snap returns exactly what a `pop` issued immediately before would have: the empty new slot contributes nothing, and the existing arms keep their contents and their relative priority, since neither `p` nor their subtrees changed.
  - The first `push` governed by `z'` _that routes to `k`_ is the first packet ever to occupy `a`, which Lemma 3.9 from _FA_ admits while preserving `|- q'`.

_Deeper paths._ The example edits the root, but `path` may be any prefix; `ArmAdded { path = [1, 2]; arm = ... }` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes that `q` and `q'` share verbatim, and, because the new arm is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor PIFO is rewritten, and the edit is confined to `c` and the fresh arm below it.

_Mid-order insertion._ We led with an append because it is the cleanest case, but the sniffer can place the new arm at any index, e.g: `SP(gmail, zoom)` --atomic--> `SP(gmail, **spotify**, zoom)`. When `k` is not the last index, the children formerly at indices `>= k` shift up by one to make room, and `p` becomes a `p'` that follows the renumbering: every entry naming an old index `>= k` is bumped up by one. This relabels index _values_ only; it moves no packets and changes no ranks, so each old slot keeps its matched count of occurrences and packets, now under a shifted name. At the instant after the snap, `p'` still does not name `k` (the old `>= k` entries were all bumped to `>= k+1`), so the `0 = 0` argument at slot `k` goes through unchanged and the snap is again atomic with degenerate `link`.

#### 3.4.3. `ArmRemoved(path, arm)`

Example: `SP(gmail, zoom, spotify)` --> `SP(gmail, zoom)`, the operator is decommissioning `spotify`. The diff is `ArmRemoved { path = [2]; arm = spotify }`. As before, we write `c` for the parent (in our example, the root `SP`) and `k` for the doomed slot (in our example, `k=2`); write `d` for the doomed arm (the `spotify` subtree) and `R` for the set of packets buffered under `d` at the relevant instant. The new difficulty, absent from `ArmAdded`, is that `d` may be non-empty: by well-formedness `c`'s index-PIFO `p` holds exactly one occurrence of `k` for each packet in `R`, so we cannot simply delete `d` and leave `p` with indices to a child that is gone.

Dual to §3.4.2's freshness convention, we read `ArmRemoved` as a genuine _retirement_: under `next` the doomed class is no longer admitted anywhere, and is not re-homed onto a surviving arm.

`ArmRemoved` is the first edit whose `link` is genuinely substantive. We present three choices for how to negotiate the transition:

1. _Drop._ Discard the doomed arm's buffered packets and land in `next` in one snap. Lossy, but instant.
2. _Drain._ Snap into `link` by suspending new traffic to the doomed arm, let it empty under normal service, then snap out to `next` once the arm has drained. Lossless, but the wait is unbounded.
3. _Drain with deadline._ Like (2), but set a `T`-millisecond deadline for the second snap (at which point, drop). Lossless if it drains in time, otherwise bounded latency.

##### Choice 1.

_The tree, `q -> q'`._ Remove `d` from parent `c`'s child list and, in the _same_ transaction, add `k` to `c`'s tombstone set (§3.2). We do not rewrite `c`'s index-PIFO `p`; its occurrences of `k` become phantoms that `pop` discards on sight, so we pay nothing to hunt them down.

_The transaction, `z -> z'`._ `z'` is `next`'s transaction; the retired class is admitted nowhere, and every surviving route is unchanged, since `next` is `prev` minus the arm.

_The state, `s -> s'`._ Drop `d`'s scheduling state and `c`'s per-slot bookkeeping for `k`; nothing else moves.

_The snap._ `q'` is well-formed modulo tombstones (`|-_T q'`, §3.2): the surviving slots `0, 1` keep their packets and their occurrences in `p`, so their balance is inherited from `prev`'s `|- q` verbatim, and the leftover occurrences of `k` are exactly the phantom indices that `pop` now knows how to skip. No surviving packet straddles the snap; the packets of `R` simply cease to exist. So a `pop` after the snap behaves exactly as a `next` pop, skipping phantoms; we are in `next` modulo tombstones at once, and §3.2 reclamation upgrades `|-_T q'` to plain `|- next` within bounded time (at most `|R|` phantom-pops). This is §3.4.1's degenerate case modulo tombstones: the entry and exit snaps coincide and `link` is empty. _Cost:_ `R` is dropped. _Latency:_ none.

##### Choice 2.

The entry snap installs `next`'s transaction while leaving topology and contents untouched. With `k` last, `next`'s surviving arms keep `prev`'s indices `0, 1`, so `next`'s `z` emits only those indices and never `k`; lifted onto `prev`'s still-present topology it is a well-typed transaction that simply declines to route into `d`. (A mid-slot removal needs the same cosmetic index relabeling as §3.4.2's mid-insert, so that `next`'s routing is expressed in `prev`'s current numbering until the exit GC renumbers for real.) The tree and state are untouched: `d` is still present, still holding `R`, and `c`'s `p` still holds `R`-many occurrences of `k`.

This entry snap carries `prev` into a substantive `link` whose topology is `prev`'s and whose transaction is `next`'s. Entry-soundness (i) and closure (ii) of §3.4.1 hold:

- _Entry-soundness_ (i): the entry snap preserves `|- q` trivially, since it touches only `z`, not the tree.
- _Closure_ (ii): `link` is closed under its own `push`/`pop`. A `push` follows `z'`, so `d` gains no new members and the surviving arms behave as in `next`; Lemma 3.9 keeps `|- q`. A `pop` proceeds over `prev`'s tree; whenever it serves a packet of `R` it decrements `R` and `c`'s `k`-occurrence count by one in lockstep, preserving the invariant. `R` therefore only shrinks and never grows.

The exit condition `φ` here is "`R` empty," reached in the natural course of `pop`s. The instant it holds, the invariant forces `c`'s `k`-occurrence count to zero, so the exit snap (unhook the now-empty `d`, drop slot `k`, renumber any higher siblings) moves no packet and rewrites no live occurrence: it is a pure rewiring into `next`, so exit-soundness (iii) holds. _Cost:_ none; nothing is dropped. _Latency:_ unbounded: a steady stream of `gmail` or `zoom` traffic can starve `spotify` indefinitely, so the exit may never fire. This is the choice that fails _liveness_ (iv): it satisfies conditions (i)-(iii) but cannot promise to reach `next`.

##### Choice 3.

The entry snap and the `link` are identical to choice 2; only the exit condition changes, to "`R` empty _or_ a wall-clock budget of `T` milliseconds elapsed, whichever comes first." If `R` drains before `T`, the exit is choice 2's free rewiring and nothing is lost. If `T` fires first with `R` non-empty, the exit snap is choice 1's drop applied to the residue: discard the leftover packets of `R` and tombstone their occurrences in `p` as it unhooks `d`. _Cost:_ at most the packets of `R` that did not drain in time. _Latency:_ bounded by `T`. The deadline is precisely what restores the _liveness_ condition (iv) that choice 2 lacked.

### 3.5 Preserving this proof down to hardware

After this, there is a simple and mechanical compilation from the tree diff language to the IL-gen language. At the IL-gen language level, primitives look like `spawn`, `adopt`, etc, and it _is_ possible to create malformed trees. But we can informally argue that our proof at the tree-diff level carries over to the IL level because:

- We show our command-to-commands compilation, and assert that it's uncontroversial.
- We wrap IL-level commands into transactional commits, so that no user would actually get to see the malformed state (and, key to our point, no push/pop would be able to reach the scheduler when it is malformed).

We reuse this trick to again argue that our proof is preserved from the IL level to the h/w level.

## 4. Identifying Better Transitions

Now that we're on firm ground, we can go back and revisit our diff-sniffer. Our give-up case is the whole-tree `ArmReplaced { path = []; arm = next }`; §3.4 showed that this routes through the maximally pessimistic `link`, whose realizations span a full-tree drain (lossless, slow) and a stop-the-world drop (lossy); the latter is SOTA. Either way the blast zone is the entire tree. Here we show how we can identify diffs more precisely, and how we can give up at a deeper part of the tree if we need to, so that the transition routes through a `link` confined to a small subtree and leaves the rest of the scheduler running.

There are lots of examples to show here, and possibly some beefing-up of compare.ml itself. TK.

## 5. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.

## 6. Evaluation

## 7. Related Work

## 8. Conclusion
