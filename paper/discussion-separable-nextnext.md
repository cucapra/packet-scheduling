# Discussion: serving a separable `nextnext` faster

## The setting

The paper's transition planner (§4) takes a single `prev -> next` request and emits a sequence `(φ_0 ; δ_0) ; (φ_1 ; δ_1) ; ... ; (φ_n ; δ_n)` of guarded atomic diffs.
Every diff carries a guard; a guard `φ` is a predicate on the state of the live control, and the paired `δ` fires when its `φ` becomes true.
The constant `φ = true` is allowed; the typical non-trivial guard waits for some subtree to drain.

A `prev -> next` sequence with at least one non-trivial guard is _in flight_ from the moment `δ_0` fires until the moment its last guard becomes true.
The control during this window is neither `prev` nor `next`: it is a `link` (§4).

We are interested in what happens if the operator submits a follow-up request, `nextnext`, while a `prev -> next` sequence is still in flight.

## The cop-out answer that's in sketch.md

In §4 we promise the simplest possible thing: `nextnext` is queued and the planner does not begin work on it until the in-flight `prev -> next` sequence completes.

## Why a stronger answer is sometimes possible

The cop-out is conservative.
A `link` control can be `*`-separable in the informal sense that its still-pending guarded diffs touch only a region `R` of the tree, leaving the rest of the tree in totally reasonable shape to receive the next transition.
If `nextnext`'s footprint is disjoint from `R`, then a sequence realizing `nextnext` can be interleaved with the in-flight sequence rather than waiting behind it.
Every interleaving step is still a sound §3 atomic diff applied to a sound §3 control; we are not weakening §3 or §4.

## A worked example

Let `P` be any discipline (the example does not depend on its scheduling policy, only on its tree shape; we assume `P` is not WFQ purely so the `Add` below does not need to carry a weight).

- `p1 = P(A, B)` is running at t1.
- `p2 = P(C, B)` is requested at t2.
  The planner emits `(true, Designate([0], C)) ; (true, Quiesce(A)); (A empty, Undesignate([0]))`.
  The first two diffs fire at t2. Together they install the super-node `{A -> C}` at `P`'s 0th slot, with `A` set to receive no new packets and with `C` admitting all new traffic that used to classify into `A`. So the tree looks like `P({A->C}, B)`.
  The pending diff (which would collapse `{A->C}` into just `C`) waits for `A` to drain.
- `p3 = P(C, B, D)` is requested at t3.
  The `prev -> next` planner would have emitted `Add([2], D)` to go from `p2` to `p3`.
- A drains to empty at t4 > t3, satisfying the pending guard.

The live control between t2 and t4 realizes the link `P(Strict(A, C), B)`.
The still-pending part of the in-flight sequence consists of one guarded diff `(A empty, Undesignate([0]))`, whose footprint is in `P`'s slot 0.

### Cop-out trace

- t2: fire `Designate([0], C)`.
  Live control realizes `P(Strict(A, C), B)`.
- t3: `nextnext = p3` arrives.
  Queue it.
  New traffic for `D` is rejected during the wait.
- t4: `A` drains.
  Fire `Remove([0, 0])`.
  Super-node collapses.
  Live control realizes `P(C, B) = p2`.
- t4+: planner begins work on the queued `p2 -> p3` request and fires `Add([2], D)`.
  Live control realizes `P(C, B, D) = p3`.
  D begins to admit traffic from this point on.

### Clever trace

- t2: fire `Designate([0], C)`.
  Live control realizes `P(Strict(A, C), B)`, with the pending guarded diff `(A empty, Remove([0, 0]))`.
- t3: `nextnext = p3` arrives.
  The planner observes that `Add([2], D)` and the pending `Remove([0, 0])` are footprint-disjoint, and that `Add([2], D)` is an append (no index renumbering at root).
  Fire `Add([2], D)` immediately.
  Live control now realizes `P(Strict(A, C), B, D)`.
  D admits traffic from this point on.
  The pending guarded diff is unchanged.
- t4: `A` drains.
  Fire `Remove([0, 0])`.
  Super-node collapses.
  Live control realizes `P(C, B, D) = p3`.

### What the clever version actually buys

Both traces _land_ at `p3` at the same instant: t4.
The system genuinely realizes `p3` only when `A` has drained, and that does not happen earlier in the clever version.
What the clever version buys is the early bring-up of `D`'s admission, from t4 down to t3.
New packets that classify into `D` and arrive between t3 and t4 are served in the clever version and rejected (by the partial-`z` extension of §3.1.2; see also `Quiesce`, §3.3) in the cop-out version, because `D`'s classifier predicates do not enter `z` until the Add fires.

This is not "we install `p3` faster."
It is "we install the parts of `p3` that are independent of the still-draining region, faster."

## Why the example is sound

The Add at t3 is itself a §3.4.1 atomic diff applied to the live control at t3.
It needs to satisfy §3.4.1's preconditions on that live control, not on `p2` and not on `p3`:

- The path `[2]` lands at an internal node (the root), and by assumption `P` is not WFQ, so no weight is required.
- `D`'s leaf label is fresh against the live tree, which contains `Strict(A, C)` at slot 0 and `B` at slot 1.
- `D`'s classifier predicates must be disjoint from the domain of the live `z` at t3.
  Crucially, the live `z` at t3 routes the old `A`-bound packets to the super-node's high-priority slot and the old `A`-classified new arrivals to the survivor `C`.
  The Add can fire only if `D`'s classifier does not overlap with any of `C`'s, `B`'s, or the residual `A`'s.
  This is a real precondition; it is the same precondition the cop-out version would face at t4 (because `C`'s classifier is in `z` at t4 too).
  The clever version does not weaken this constraint; it just checks it earlier.

State preservation at the root is undisturbed: `Designate` at t2 inserted a super-node into root slot 0 without touching root's `node_state` or the `slot_state` for slot 1, so at t3 the root's bookkeeping is exactly what it was at t1, and `init_slot_D` for the new `D`-slot reads off that bookkeeping as §3.4.1 prescribes.

Realization: applying `den(Add([2], D))` to `P(Strict(A, C), B)` gives `P(Strict(A, C), B, D)`, which matches what `⌊.⌋` reads off the live control after the Add.
At t4, `Remove([0, 0])` collapses the single-armed Strict to its lone survivor `C`, giving `P(C, B, D) = p3`.

The atomicity of each individual step is inherited from §3.4: the Add at t3 is atomic by §3.4.1, the Remove at t4 is atomic by §3.4.2.
The clever version is not introducing a new compound atomic step; it is just admitting that two perfectly ordinary atomic steps from two different `prev -> next` sequences can be interleaved if their footprints do not collide.

## When is this possible in general?

The example works because three conditions all hold simultaneously:

1. **Footprint disjointness.**
   The pending guarded diff `Remove([0, 0])` acts inside the super-node at root slot 0.
   The new diff `Add([2], D)` acts on the root's child list at index 2 and on a fresh subtree below it.
   No position is in both footprints, and neither edit's footprint is a prefix of the other's.
2. **Index stability across the new edit.**
   `Add([2], D)` happens to be an append at the root; it does not renumber any sibling.
   Had the new edit been `Add([1], D)` (a mid-insertion), the root's child list would shift, and the pending `Remove`'s path `[0, 0]` would have to be re-resolved on the shifted tree.
   It happens that any path of the form `[0, ...]` is unaffected by an insertion at root index 1 or later, so this particular example would still go through.
   But in general, the planner must reason about path stability, not just disjointness.
3. **Classifier disjointness.**
   `D`'s classifier predicates must be disjoint from everything currently in `z`, including the survivor `C`.
   This is the same constraint Add carries everywhere, but in the clever setting it is evaluated against the link control, not against the eventual `p2` or `p3`.

A defensible general formulation of the clever rule:

> Let `S` be the set of positions touched by the pending guarded diffs of the in-flight sequence (footprints of paths, edit sites, and the renumbering shadows of any pending arity-changing edits).
> A new `nextnext` request may be admitted into the in-flight sequence if a transition plan for it can be chosen whose every step's footprint is disjoint from `S` _at the moment that step fires_, and whose every step satisfies its §3.4 preconditions on the live control at that moment.

This is not a closed-form check.
It is a property of an _interleaving plan_, not of `nextnext` alone, and the planner has to commit to a plan and then verify the property along it.
In the easy case (the example above), the plan is "fire the whole new sequence now" and the property is one local disjointness check.

## Holes and edge cases

- **Index renumbering at shared parents.**
  If a pending edit and a new edit both modify the same parent's child list (rather than independent subtrees beneath disjoint slots), one will shift the indices the other uses, even when their paths are not in a prefix relation.
  The example sidesteps this by appending.
  A planner doing the clever interleave at a shared parent must commit to an ordering and resolve the indices accordingly.
  We have not tried to characterize when this is or is not safe in general.
- **Classifier interference, especially via `Designate`.**
  A pending `Designate` installs a survivor whose classifier inherits part of the retiring subtree's domain.
  Any new `Add` or `Designate` in flight must check disjointness against the survivor's classifier as well as the original tree's; the survivor is part of the live `z` from the moment its `Designate` fired.
- **No earlier semantic landing.**
  The clever version does not make the system realize `p3` earlier.
  It admits a part of `p3` (here, `D`'s classifier and queueing) earlier, while the rest of `p3` continues to wait on the in-flight drain.
  An operator reading the system as "what does `⌊.⌋` of the live control say?" will see a fresh intermediate link policy (`P(Strict(A, C), B, D)`) for the duration.
  This is a perfectly ordinary §3 control, but it is not `p3` and it is not the original `prev -> next` link either.
- **Queueing depth.**
  Allowing multiple in-flight sequences requires the planner to track the union of all pending guarded diffs and to check each new arrival against the whole set.
  The cop-out version maintains a single in-flight sequence and a simple queue of pending `next` requests.
  The clever version trades a more complex planner state for the latency win.
- **Cancellation and supersession.**
  If a `nextnext` arrives that semantically supersedes an in-flight change (e.g., undoes the `prev -> next` request entirely), the right move is not interleaving but cancelling the in-flight sequence.
  This is a separate question from separability and is not addressed here.
- **The win is bounded by the drain time.**
  The clever version's advantage over the cop-out version is exactly the time from t3 to t4, the duration of the residual drain.
  If the in-flight sequence has no pending guards left (drain is already complete) at the moment `nextnext` arrives, the cop-out version is already as fast as the clever one.

## What we say in the paper

Nothing, beyond the short note in §4 pointing here.
The cop-out answer is the one we commit to, prove sound (trivially, since the queued sequence is just a fresh `prev -> next` once the previous one finishes), and run in the implementation.
The clever version is a real and reachable extension, and the example above survives a careful sniff test, but characterizing it in generality is more work than we want to do in this paper.
