(** The planner converts a pair of [Rio_core.Pol] policies into a guarded
    sequence of atomic [Rio_delta.Delta] productions whose composed effect
    rewrites the first into the second. See [planner.ml] for the design notes
    and paper sketch.md sec4 for the underlying [(phi ; delta)*] grammar. *)

module Delta = Rio_delta.Delta

type guard =
  | True  (** Fire immediately, no precondition. *)
  | Empty of Delta.path
      (** Fire once the subtree at [path] in the previous policy has drained all
          in-flight packets. *)

type step = guard * Delta.t
type t = step list

val analyze : Rio_core.Pol.t -> Rio_core.Pol.t -> t
(** [analyze p1 p2] proposes a guarded sequence carrying [p1] to [p2]. Returns
    [[]] when the two policies are equal. Per-step deltas are atomic
    [Rio_delta.Delta] productions; sub-policy collapses ([p2] sits inside [p1])
    expand into the [PruneDownTo] idiom; structural give-ups (constructor
    mismatches, multi-arm divergences) expand into the [Replace] idiom at the
    affected slot. *)

val replace : next:Rio_core.Pol.t -> ?meta:float -> unit -> t
(** The paper's [Replace] idiom:
    [Designate(p, next) ; Quiesce(p ++ [0]) ; (Empty (p ++ [0])) Undesignate(p)].
    After [Designate], the loser sits at child index 0 of the freshly minted
    SP*, so [Quiesce] and the [Empty] guard both target [p ++ [0]]; only
    [Undesignate]'s own path stays at [p]. With [?meta] supplied, a trailing
    [(True) ChangeMeta(p, meta)] step rebinds the slot's per-arm meta (SP rank
    or WFQ weight); sequencing after [Undesignate] suffices, since by the time
    [ChangeMeta] runs the loser has drained and the parent edge already points
    at the survivor. Emitted with [path = []] at the local level; the caller
    prepends the target index via [prepend_seq]. Also serves as [analyze]'s
    give-up sequence when no smaller atomic edit applies. *)

val retire : unit -> t
(** The paper's [Retire] idiom: [Quiesce(p) ; (Empty p) Remove(p)]. Emitted with
    [path = []] at the local level; the caller prepends the target index via
    [prepend_seq]. *)

val slow_retire : unit -> t
(** The paper's [SlowRetire] idiom: [(Empty p) Remove(p)]. Like [retire] but
    skips the explicit [Quiesce]: waits for the subtree to drain naturally, then
    removes its arm. [analyze] never emits this on its own; it's exposed for
    callers that already know the subtree is being drained upstream. *)

val prune_down_to : prev:Rio_core.Pol.t -> path:Delta.path -> unit -> t
(** The paper's [PruneDownTo] idiom: collapse [prev] down to the strict subtree
    at [path]. Expands as a sequence of [Retire]s on off-path siblings along the
    route (highest-index-within-level first, outer-level first), followed by a
    final [(True, ChangeRoot [0;...;0])] re-root. The all-zeros target path
    holds because after every off-path sibling has retired, the survivor is the
    sole remaining child at index 0 at each level. *)

val is_sub_policy : Rio_core.Pol.t -> Rio_core.Pol.t -> Delta.path option
(** [is_sub_policy p1 p2] returns [Some path] when [p1] is structurally equal to
    the subtree of [p2] at [path], with [path = []] meaning [p1 = p2]. Returns
    [None] when [p1] is not embedded anywhere in [p2]. Used by [Ir.patch] to
    recover the [PruneDownTo] target path from a planner sequence whose final
    [ChangeRoot] is all-zeros. *)

val to_string : t -> string
(** Test-output formatter. *)
