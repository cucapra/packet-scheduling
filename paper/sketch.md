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
- What vPIFO does not do. As published, vPIFO has no notion of a diff between old and new policies, no formal semantics for what a policy change means, and no account of in-flight packets during a change. The SDL IR is for _rank computation_ compiled to P4 or CPU, quite different from our structural/topological one.
- Our two running examples make the gap concrete. The easy append, to get to `SP(gmail, zoom, spotify)`, is exactly the kind of edit vPIFO's substrate _could_ have absorbed in place: a targeted append to the Operation Generation Table and the PIFO Instance Address Table would register the one new PIFO instance and its operations, leaving every running instance untouched. vPIFO as published does not engage with this question; their only option is a full reinitialization. Driving such a substrate to install the edit in place, straight from the (small) diff, is exactly what our layer adds. The harder restructuring, to `SP(gmail, RR(zoom, spotify))`, is not even clearly within reach: it inserts an internal node and re-parents `zoom`, so on a substrate that (correctly) maps tree depth to PEs, some in-flight packets already sit under the old shape, and it is unclear that vPIFO's substrate could realize the change at runtime at all. vPIFO neither claims it can nor explains how it would.
- vPIFO's own §8 ("Runtime Updating of the Scheduling Policy") names our exact problem as future work: "Ensuring correct scheduling of packets during the transitional phase between modifications is part of our future work." The accompanying sentence says the runtime interface itself (P4-runtime-style) is still under development.
- The relationship, stated plainly. We are not competing with vPIFO and do not claim a better PIFO substrate. We supply the layer _above_ a PIFO substrate (which could well be vPIFO). That is, the formal transition between two policies, the small patch that realizes it, and the transitionary semantics. The works compose.

## 3. Formalizing the Transition Phase `link`

We recap the PIFO-tree model (§3.1), extend it with tombstones (§3.2), fix a grammar of policy diffs (§3.3), show that every diff induces a sound transition through a well-formed `link` (§3.4), and argue that the guarantee survives down to hardware (§3.5).

### 3.1 Background: PIFO trees, formally

We build on the PIFO-tree model of Mohan et al. [Formal Abstractions, OOPSLA '23, §3], so we review the pieces of their formalism that this paper actually leans on.

#### Topology vs. contents

A _topology_ `t` is a finite tree carrying no data: either a single node `*` or `Node(ts)` for a list of child topologies. A _PIFO tree_ of topology `t`, written `q : PIFOTree(t)`, layers data onto `t`. A leaf `Leaf(p)` holds a packet-carrying PIFO `p`. An internal node `Internal(qs, p)` carries two things: a list `qs` of well-formed PIFO-tree children whose topologies match the corresponding sub-topologies of `t`, and a PIFO `p` whose entries are child indices into `qs`. This separation between the topology and the carried contents is key to making the diff grammar of §3.3 well-defined: a structural edit is a change to the topology `t`, distinct from the running contents.

#### The two user-observable operations

`push(q, pkt, pt)` enqueues `pkt` along a precomputed path `pt = (i_1, r_1) :: ... :: (i_n, r_n) :: r_{n+1}`. The path is richly decorated: it tells the PIFO of each internal node along the path what child index to enqueue and what rank to use for that enqueue. At the leaf level it tells the leaf's PIFO what rank to use when enqueuing the packet itself. `pop(q)` returns the most favorably ranked packet by popping the root to yield a child index, recursing into that child, until finally emitting a packet from the leaf. These are the _only_ user-visible interactions with a scheduler, which is why §1's notion of an _atomic_ transition is stated in terms of `push`/`pop` observability: every `push`/`pop` happens agains a well-defined scheduler, never a half-edited intermediate scheduler.

#### Well-formedness

A PIFO tree `q` is well-formed (`|- q`, following _Formal Abstractions_' notation) when, at every internal node with index-PIFO `p` and children `qs`, the number of occurrences of `i` in `p` equals the number of packets held under `qs[i]`, for every legal `i`. This is the invariant that keeps `pop` from getting stuck. Lemma 3.9 of _FA_ shows that `push` always preserves `|- q`, and that `pop` preserves it when `q` is non-empty (which is precisely the condition under which `pop` is defined).

#### Control

_FA_ factors the scheduling _policy_ (which, given an arriving packet and the current state, produces the path that `push` will follow) into a _control_ object. A control over `t` is a triple `(s, q, z)`: a current state `s` drawn from some fixed set, the PIFO tree `q` of topology `t` itself, and a _scheduling transaction_ `z` that, given a state and a packet, returns a path of the correct shape together with an updated state.

### 3.2 Extending the model

The model of §3.1 suffices to state our diff grammar, but the transition proofs of §3.4 lean on a small extension to it. We discuss such conveniences here.

#### Tombstones

Sometimes we need to delete a child PIFO tree. If the child had packets, then the parent still has occurrences of `k` (the index used by the parent to refer to the deleted child) in its index-PIFO `p`. Expunging those occurrences is possible but expensive: (i) they sit scattered through `p` by rank, and (ii) removing such instances of `k` from `p` actually bubbles up and creates additional removal obligations in the parent's _own_ ancestors.

Instead of getting into all this, we let a node carry a finite set `T` of _tombstone_ indices. If a PIFO has entry `k` but `k` is in that PIFO's set `T`, we call `k` a _phantom_. We call non-phantom entries _live_. `pop` gains a single rule: if a `pop` of the whole tree surfaces a phantom, discard it and `pop` the whole tree again. The restart must happen at the root, not at the PIFO that held the phantom. Reaching that PIFO already spent one pointer in each ancestor along the descent (recall that tombstoning a child does not rewrite its parent, so an ancestor's pointer count still includes the phantoms beneath it). Re-popping _locally_ would therefore draw a second entry from under those ancestors while they shed only one apiece, leaving them with too many pointers into the subtree, i.e. a broken tree. Restarting from the root instead makes each phantom-discard cost exactly one entry at every node on a single root-to-phantom path, so all counts stay balanced and `pop` never gets stuck on a phantom. A tombstone thus lets us delete a subtree in one transaction without rewriting its parent. The cost is a bounded number of phantom-pops. The device is borrowed from the database community, where a deletion is recorded as a _tombstone_ and storage is reclaimed lazily by later compaction rather than rewritten in place.

We say `q` is _well-formed modulo tombstones_, written `|-_T q`, when at every internal node with index-PIFO `p`, children `qs`, and tombstone set `T`, the occurrences of every _live_ index `i` in `p` equal the packets held under `qs[i]`, exactly as in §3.1.

Two facts make our changes to `pop` harmless:

- _Phantoms only vanish._ A phantom-pop removes one occurrence of a phantom index, and a `push` never adds one (the transition that adds `k` to the tombstone set `T` also installs a new scheduling transaction that routes nothing new to `k`). So each node's phantom count is monotonically non-increasing.
- _Reclamation recovers `|-`._ Once a tombstoned index's phantom count hits zero, `p` no longer needs to name the index in `T`; the dead slot may be recycled and its higher-indexed siblings renumbered, leaving an ordinary `|-` tree. Until then `|-_T` is exactly the invariant we need, and `|-` is the special case with `T` empty on all PIFOs. Of course, if `q` has no tombstones, then `|-_T q` implies `|- q`.

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
- `SubPol (path)` fires when `next` appears verbatim as a subtree of `prev` at `path`. The transition prunes `prev` down to that subtree and discards everything around it. The retained subtree `next` keeps its in-flight contents; the surrounding structure is garbage-collected.
- `SuperPol (path)` is the converse: it fires when `prev` appears verbatim as a subtree of `next` at `path`. The transition grafts the new surrounding structure around `prev`, which is re-parented at `path` and keeps its in-flight contents.

Let us revisit the two transitions from §1 and see what our diff-sniffer (§4), produces.

- `SP(gmail, zoom)` -> `SP(gmail, zoom, spotify)`. The two roots agree on constructor (`SP`); their child lists differ only in that `spotify` has been appended at index 2. The sniffer returns `ArmAdded { path = [2]; arm = spotify }`.
- `SP(gmail, zoom)` -> `SP(gmail, RR(zoom, spotify))`. The roots again agree on constructor, and the child lists differ at exactly one slot: index 1, where the leaf `zoom` has been swapped for the subtree `RR(zoom, spotify)`. The sniffer recurses into the slot and, finding _no finer single edit_, falls back and claims that the entire `zoom` arm has been replaced: `ArmReplaced { path = [1]; arm = RR(zoom, spotify) }`. The path `[1]` tells the substrate that everything outside child 1 of the root is untouched. The transitionary `link` only needs to handle the residue under `zoom`, so `gmail` and the root `SP` keep running.

Note that ours is a _single-edit sniffer._ Every non-`Same` variant describes exactly one structural change at one path. A multi-arm divergence is not expressible directly; the diff-sniffer collapses it to `ArmReplaced` at the closest enclosing path. If `ArmReplaced` has `path = []`, that means "I could not localize the changes any further than the whole tree."

Indeed, in the `SP(gmail, zoom)` -> `SP(gmail, RR(zoom, spotify))` example we could have "succeeded" with: `ArmReplaced { path = []; arm = SP(gmail, RR(zoom, spotify)) }`. In general we can _always_ succeed with `ArmReplaced { path = []; arm = _the whole new tree_}`. This is functionally our give-up-entirely token; §4 is in large part the story of pushing give-ups deeper into the tree and therefore minimizing the splash zone of the change.

We make no claim that the sniffer is canonical or minimal; we claim only that whatever it returns is a sound description of the transition from `prev` to `next`, and that §3.4 will show every variant absorbs cleanly into a well-formed tree.

### 3.4 Edits Preserve Well-Formedness

Every transition from `prev` to `next` factors through a third regime that we name `link`:

```
prev  --atomic-->  link  --atomic-->  next
```

We call each atomic regime-change a _snap_. The two arrows are atomic in the §1 sense: every user-observable `push`/`pop` happens entirely under `prev`, `link`, or `next`, never straddling the instantaneous snap between them. `link` is itself a packet-scheduling regime over a well-formed PIFO tree. For some edits `link` is _degenerate_: zero-duration, sharing topology and contents with `next` at the instant the entry snap completes, so the two arrows fuse and `prev` abuts `next` directly. For others `link` is substantive, with real duration and stated `push`/`pop` semantics, and the exit snap waits for some stated enabling condition.

We first make `snap`, `link`, and `transition` precise and state the conditions every transition must discharge (§3.4.1). We then instantiate the definition: `ArmAdded` as a warm-up, where `link` is degenerate (§3.4.2); the full repertoire for `ArmRemoved`, where it is substantive (§3.4.3); and the remaining variants after. §3.5 carries our hard-won guarantees down to hardware.

#### 3.4.1. Transitions, formally

The transitionary regime looks daunting only if we expect it to be a new kind of object. It is not. `link` is an ordinary control in the sense of §3.1: a triple `(s, q, z)` with the `push`/`pop` already defined there. What is new is not a semantics but the way two controls are spliced in time, and that is what we now pin down.

##### Snap

A _snap_ `σ` atomically replaces the live control with another, between two user operations, so that every operation is served by exactly one control, by construction. A snap may move, synthesize, or drop contents, but we require it to be _sound_: `|-_T C` must imply `|-_T σ(C)`.

##### Transition

A _transition_ realizing a diff, from the running control `C_prev` to the target `C_next`, is a tuple `(σ_in, L, φ, σ_out)`:

- an _entry snap_ `σ_in`, with link control `L = σ_in(C_prev)`
- the _link control_ `L`, run under the `push`/`pop` of §3.1, evolving without our further intervention into some `L'` as packets flow
- an _exit condition_ `φ`, a predicate on `L`'s state and, optionally, on an external clock
- an _exit snap_ `σ_out`, fired at the first instant `φ` holds, with `σ_out(L')` being `C_next`, perhaps featuring some phantoms that are reclaimed with time

##### Soundness

The transition is _sound_ when its four _conditions_ hold:

- (i) _entry-soundness_: `|-_T C_prev` gives `|-_T L`
- (ii) _closure_: `L`'s own `push`/`pop` preserve `|-_T L`
- (iii) _exit-soundness_: `|-_T L'` gives `|-_T C_next`
- (iv) _liveness_: `φ` eventually holds.

Conditions (i)-(iii) are _safety_, keeping the live scheduler well-formed modulo tombstones at every instant; condition (iv) is what drives the transition to `next`. Exit-soundness lands us in `next` well-formed modulo tombstones. Although the exit may leave phantoms in the tree, §3.2 shows that reclamation upgrades `|-_T C_next` to plain `|- C_next` within bounded time, so we reach `next` truly, not merely observably.

##### Degenerate `link`

When `L = C_next` (either exactly, or modulo tombstones) and `φ = true`, the exit snap is the identity and fires at once: the two snaps fuse, and `prev` abuts `next` with no observable transitionary regime. Closure (ii) and liveness (iv) are then vacuous, and entry-soundness (i) and exit-soundness (iii) collapse into one well-formedness check. See `ArmAdded` (§3.4.2; no tombstones) and `ArmRemoved`'s Choice 1 (§3.4.3; modulo tombstones).

##### The repertoire

A diff does not totally determine its transition. In general many tuples `(σ_in, L, φ, σ_out)` realize the same `prev`-to-`next` change at different costs in lossiness and latency, and each is independently sound. We call this set of sound transitions the _repertoire_. SOTA offers exactly one item, a whole-tree stop-the-world `link`; §4 is largely about choosing more intelligently from the repertoire.

##### Theorem (the transitionary period is just scheduling)

For every diff variant, and every realization given in §3.4.2 onward, the tuple `(σ_in, L, φ, σ_out)` is a sound transition. Hence at every instant the live scheduler is a well-formed control (`|-` of §3.1, or `|-_T` of §3.2 where a realization has tombstones), and the user-observable trace is a sequence of ordinary `push`/`pop` operations served first by `C_prev`, then by `L`, then by `C_next`. The "semantics of `link`" is therefore nothing more than the §3.1 semantics of the control we install; each variant's job is to supply the four components (`σ_in`, `L`, `φ`, `σ_out`) and discharge the four conditions (entry-soundness, closure, exit-soundness, liveness).

#### 3.4.2. `ArmAdded(path, arm, weight?)`

Example: `SP(gmail, zoom)` --atomic--> `SP(gmail, zoom, spotify)`.
The diff computed according to the grammar in §3.3 is: `ArmAdded { path = [2]; arm = spotify }`.
By the grammar of §3.3, `ArmAdded` means that the new arm carries fresh traffic that intersects nothing `prev` was already serving (it was being rejected thus far, say). No `prev`-era packet therefore belongs under the new arm.

We instantiate the template of §3.4.1. We write `c` for the parent named by `path` (here the root `SP`) and `k` for the new slot's index (here `2`); we take the append case first and a mid-order insert afterward.

##### The four components of the transition

The _entry snap_ `σ_in` installs a new control `(s', q', z')`, which we build piece by piece.

- _The state, `s -> s'`._ `s'` agrees with `s` everywhere, except that it records the initial local state for the new slot: the new arm's own scheduling state, plus whatever per-slot bookkeeping `c`'s scheduler keeps (an RR cursor, the slot's weight taken from the edit's `weight?`, a virtual-finish accumulator, etc.). No existing slot's state is disturbed.
- _The tree, `q -> q'`._ At `c = Internal(qs, p)` we append the new arm to the child list, so its slot `k` is the new last index: `q' = Internal(qs ++ [a], p)`. Here `a` is the _empty_ PIFO tree having whatever topology `arm` described. The parent's index-PIFO `p` is left exactly as it was, because it only ever named the old indices `0..k-1` and so does not name `k` at all.
- _The transaction, `z -> z'`._ `z'` is the scheduling transaction compiled from `next`. It is a _conservative extension_ of `z`: for any packet that does not classify into the new arm it defers to `z`, and for a packet that does classify into the new arm it returns a path whose step at `c` selects `k` and then descends through the PIFO tree `a`. Because `k` was not a legal index under `prev`, `z` could never have emitted such a path, so the two transactions differ only on routes that land in the previously-nonexistent subtree.

_The other three components._ The arm `a` is empty, so the control that `σ_in` just installed is already `next`: the `link` is degenerate, with `L = C_next`, `φ = true`, and `σ_out` the identity function.

##### The four conditions of soundness

Closure (ii) and liveness (iv) are vacuous. Entry-soundness (i) and exit-soundness (iii) collapse into a single well-formedness check. Let us discharge it.

We must show that `|-_T C_prev` gives `|-_T C_next`. The parent `c` has exactly zero occurrences of `k` (its index-PIFO `p` does not name `k`, having only ever named `0..k-1`), and the new arm `a` holds zero packets or indices, so the well-formedness obligation at slot `k` reads `0 = 0`. Every other slot is the child it was in `q`, with the same packets beneath it and the same entries in `p`, so its obligation is inherited verbatim. Nothing needs repair.

##### Notes

_Atomicity._ No in-flight packet straddles the snap: every packet resident at the snap instant lives in the shared structure `qs`, carried into `q'` unchanged, and none is under `k`. So a `pop` immediately after the snap returns exactly what a `pop` immediately before would have, the empty new slot contributing nothing and the existing arms keeping their contents and relative priority; and the first `push` that `z'` routes to `k` is the first packet ever to occupy `a`, which Lemma 3.9 from _FA_ admits while preserving `|- q'`.

_Deeper paths._ The example edits the root, but `path` may be any prefix; `ArmAdded { path = [1, 2]; arm = ... }` adds a slot inside a grandchild of the root. Nothing in the argument changes. The descent from the root to `c` passes only through nodes that `q` and `q'` share verbatim, and, because the new arm is empty, it adds zero packets beneath every ancestor of `c`. So each ancestor's occurrence-tally for the child it forwards through is exactly what it was, no ancestor PIFO is rewritten, and the edit is confined to `c` and the fresh arm below it.

_Mid-order insertion._ We led with an append because it is the cleanest case, but the sniffer can place the new arm at any index, e.g: `SP(gmail, zoom)` --atomic--> `SP(gmail, **spotify**, zoom)`. When `k` is not the last index, the children formerly at indices `>= k` shift up by one to make room, and `p` becomes a `p'` that follows the renumbering: every entry naming an old index `>= k` is bumped up by one. This relabels index _values_ only; it moves no packets and changes no ranks, so each old slot keeps its matched count of occurrences and packets, now under a shifted name. At the instant after the snap, `p'` still does not name `k` (the old `>= k` entries were all bumped to `>= k+1`), so the `0 = 0` argument at slot `k` goes through unchanged and the snap is again atomic with degenerate `link`.

#### 3.4.3. `ArmRemoved(path, arm)`

Example: `SP(gmail, zoom, spotify)` --> `SP(gmail, zoom)`, the operator is decommissioning `spotify`. The diff-sniffer gives us `ArmRemoved { path = [2]; arm = spotify }`. As before, we write `c` for the parent (in our example, the root `SP`) and `k` for the index of the doomed subtree (in our example, `k=2`); write `d` for the doomed subtree itself (in our example, the `spotify` subtree) and `R` for the set of packets buffered under `d` at the relevant instant. The new challenge, absent from `ArmAdded`, is that `d` may be non-empty: by well-formedness `c`'s index-PIFO `p` holds one occurrence of `k` for each packet in `R`, so we cannot simply delete `d` and leave `p` with indices to a child that is gone.

Dual to §3.4.2's freshness convention, we read `ArmRemoved` as a genuine _retirement_: under `next`, packets from the doomed class are no longer admitted anywhere, and are not re-homed onto a surviving arm.

`ArmRemoved` is the first edit whose `link` is genuinely substantive. We present three choices for how to negotiate the transition:

1. _Drop._ Discard the doomed arm's buffered packets and land in `next` in one snap. Lossy but instant.
2. _Drain._ Snap into `link` by suspending new traffic to the doomed arm, let it empty under normal service, then snap out to `next` once the arm has drained. Lossless, but the wait is unbounded.
3. _Drain with deadline._ Like (2), but set a `B`-millisecond deadline for the second snap (at which point, drop). Lossless if it drains in time, otherwise bounded latency.

Each is a transition in the sense of §3.4.1; we give its four components and discharge its four conditions.

##### Choice 1.

_The four components._ The entry snap `σ_in` installs `(s', q', z')`. Piece by piece: `q'` removes `d` from `c`'s child list and, in the _same_ transaction, adds `k` to `c`'s tombstone set (§3.2), leaving `p`'s occurrences of `k` as phantoms that `pop` discards on sight (so we pay nothing to hunt them down); `z'` is `next`'s transaction, which denies entry to the retired class and leaves every surviving route unchanged; and `s'` drops `d`'s scheduling state and `c`'s bookkeeping for `k`. Since the packets of `R` are dropped, the result is already `next` modulo the phantoms, so the `link` is degenerate: `L = C_next`, `φ = true`, and `σ_out` is the identity function.

_The four conditions._ Closure (ii) and liveness (iv) are vacuous, and entry-soundness (i) and exit-soundness (iii) collapse into a single well-formedness check: show that `|-_T C_prev` gives `|-_T C_next`. The surviving slots (`0` and `1` in our example) keep their packets and their occurrences in `p`, so their well-formedness obligation is inherited from `prev`'s `|- q` verbatim, and the leftover occurrences of `k` are exactly the phantoms (§3.2). The packets of `R` cease to exist, so a `pop` after the snap behaves exactly as a `next` pop, skipping phantoms, and §3.2 reclamation upgrades `|-_T q'` to plain `|- next` within bounded time (at most `|R|` phantom-pops).

_Cost:_ `R` is dropped. _Latency:_ none.

##### Choice 2.

_The four components._ The entry snap `σ_in` installs `next`'s transaction while leaving topology and contents untouched: `d` is still present, still holding `R`, and `c`'s `p` still holds `R`-many occurrences of `k`. With `k` last, `next`'s surviving arms keep `prev`'s indices `0, 1`, so `next`'s `z` emits only those and never `k`; lifted onto `prev`'s still-present topology, `z` is _well-typed but simply declines to route into `d`_. (A mid-slot removal needs the cosmetic index relabeling of §3.4.2's mid-insert until the exit GC renumbers for real.) The link control `L` is thus `prev`'s topology under `next`'s transaction, and _it is substantive_. The exit condition `φ` is "`R` empty", and `σ_out` unhooks the now-empty `d`, drops slot `k`, and renumbers any higher siblings.

_The four conditions._

- _Entry-soundness_ (i): the entry snap preserves `|- q` trivially, since it touches only `z`, not the tree.
- _Closure_ (ii): `L` is closed under its own `push`/`pop`. A `push` follows `z'`, so `d` gains no new members and the surviving arms behave as in `next` (Lemma 3.9). A `pop` over `prev`'s tree decrements `R` and `c`'s `k`-occurrence count by one in lockstep whenever it serves a packet of `R`, so `R` only shrinks and never grows.
- _Exit-soundness_ (iii): the moment `φ` holds, the invariant forces `c`'s `k`-occurrence count to zero, so `σ_out` moves no packets and rewrites no live occurrences: a pure rewiring into `next`.
- _Liveness_ (iv): _fails!_ A steady stream of `gmail` or `zoom` traffic can starve `spotify` indefinitely, so `φ` may never fire; this choice satisfies (i)-(iii) but cannot promise to reach `next`.

_Cost:_ none; nothing is dropped. _Latency:_ unbounded.

##### Choice 3.

_The four components._ As Choice 2, except the exit. `φ` becomes "`R` empty _or_ a wall-clock budget of `B` milliseconds elapsed, whichever comes first," and `σ_out` branches: if `R` drained, it is Choice 2's free rewiring; if the deadline fired with `R` non-empty, it is Choice 1's drop applied to the residue (discard the leftover packets of `R` and tombstone their occurrences in `p` as it unhooks `d`).

_The four conditions._ Entry-soundness (i) and closure (ii) are exactly Choice 2's. Exit-soundness (iii) holds on both branches: the drained branch is Choice 2's rewiring, the deadline branch is Choice 1's tombstoning drop, each sound. Liveness (iv) now _holds_: the deadline bounds `φ`, so we always reach `next`.

_Cost:_ at most the packets of `R` that did not drain in time. _Latency:_ bounded by `B`.

#### [AM: leaving for now, but here we'd discuss the other elements of the grammar, giving an element a subsubsection if warranted]

### 3.5 Preserving this proof down to hardware

§3.4 proves soundness at the tree-diff level, where the primitives are coarse (`ArmAdded`, `ArmRemoved`, ...) and every edit carries a well-formed tree to a well-formed tree. To run on hardware, each edit is lowered, by a simple and mechanical compilation, into a sequence of fine-grained instructions in our IR: `Spawn`, `Adopt`, `Emancipate`, `Assoc`, `Deassoc`, `Map`, `Unmap`, `GC`, `Designate`, and the like. A single IR instruction, unlike a whole tree-diff edit, _can_ leave the tree transiently malformed: a freshly `Spawn`ed node is not yet `Adopt`ed by its parent, a class may be `Unmap`ped before its old subtree is `GC`ed, and so on.

We do not prove soundness at the IL level, but instead informally make the case for why the §3.4 proof survives the lowering. There are two reasons.

- The compilation is _faithful_: each tree-diff edit expands to a fixed instruction sequence that, when run to completion, realizes exactly that edit. We give the command-to-commands translation and take its faithfulness to be uncontroversial.
- Our substrate runs each such sequence as a single _transactional commit_: no `push` or `pop` interleaves with a commit's instructions, so the transiently-malformed intermediate trees are never observed. That commit is precisely how the substrate _realizes_ the atomic snap of §3.4.1: the snap was defined as an instantaneous control replacement between two user operations, and the commit is what collapses a multi-instruction lowering into one such instant. Every `push`/`pop` therefore still lands on a well-formed control (`prev`, `link`, or `next`), exactly as §3.4 proved; the IR's transient malformedness lives entirely inside commits, invisible to the user.

The same argument carries from the IR down to hardware: the hardware executes a committed sequence atomically with respect to user operations, so what it exhibits is again what §3.4 proved. The compilation itself, and the substrate machinery that makes a commit atomic, are the subject of §5.

## 4. Identifying Better Transitions

§3 established that our grammar is _safe_: whatever diff the sniffer emits, the transition it induces is sound, routing through a well-formed `link` and landing in `next`. With safety settled, the question this section takes up is whether we wield the grammar _well_.

Our fallback is to emit the whole-tree `ArmReplaced { path = []; arm = next }`. It admits several realizations (lossless but slow, lossy but atomic, and others [AM: leaving this vague until I actually write up ArmReplaced in §3]), but all of them route through a `link` over the entire tree: the splash zone is as big as possible; no part of the scheduler can keep running unaffected.

This section shows how to do better: sniff diffs more precisely, and, when we must give up, give up _deep_ in the tree rather than at the root. Either way the transition routes through a `link` confined to a small subtree, leaving the rest of the scheduler untouched.

As a preview, take the harder of our two running examples, `SP(gmail, zoom)` to `SP(gmail, RR(zoom, spotify))`. Rather than give up at the root, the sniffer localizes it to a _partial_ replacement, `ArmReplaced { path = [1]; arm = RR(zoom, spotify) }`: only one child of `SP` is touched. The `link` is then confined to `zoom`'s subtree, while `gmail` and the root `SP` keep running untouched.

Many examples remain to work through here, and possibly some strengthening of `compare.ml` itself. TK.

## 5. Compiling to Hardware

Leaving for Zhiyuan. We should emphasize that:

- We have rolled our own PIFO substrate; in practice you can use ours or swap it out (e.g., with vPIFO). This is not the point of the contribution. We compose well with any PIFO substrate.
- Focus on the gadgetry we built to handle transitions nicely.
- [AM: question for Zhiyuan: §3.5 leans on our substrate executing each lowered instruction sequence as an atomic transactional commit, and that commit is exactly what realizes the §3.4.1 "snap". But we also claim that we compose with _any_ PIFO substrate. So what do we actually require from a substrate? Must it support atomic commits / an atomic install that hides the transiently-malformed intermediate states? Do you know if vPIFO supports this? If a substrate cannot hide those states, does composition break? What do we genuinely need to assume?]

## 6. Evaluation

## 7. Related Work

## 8. Conclusion
