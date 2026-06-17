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
(** [analyze p1 p2] sniffs a guarded sequence carrying [p1] to [p2]. Returns
    [[]] when the two policies are equal. Per-step deltas are atomic
    [Rio_delta.Delta] productions; structural give-ups (constructor mismatches,
    multi-arm divergences) expand into the [Replace] idiom at the affected slot.
*)

val replace : next:Rio_core.Pol.t -> ?meta:float -> unit -> t
(** The paper's [Replace] idiom:
    [Designate(p, next) ; Quiesce(p) ; (Empty p) Undesignate(p)]. With [?meta]
    supplied, a trailing [(Empty p) ChangeMeta(p, meta)] step rebinds the slot's
    per-arm meta (SP rank or WFQ weight) once the loser has drained. Emitted
    with [path = []] at the local level; the caller prepends the target index
    via [prepend_seq]. Also the sniffer's give-up sequence. *)

val retire : arm:Rio_core.Pol.t -> unit -> t
(** The paper's [Retire] idiom: [Quiesce(p) ; (Empty p) Remove(p, arm)]. Emitted
    with [path = []] at the local level; the caller prepends the target index
    via [prepend_seq]. *)

val slow_retire : arm:Rio_core.Pol.t -> unit -> t
(** The paper's [SlowRetire] idiom: [(Empty p) Remove(p, arm)]. Like [retire]
    but skips the explicit [Quiesce]: waits for the subtree to drain naturally,
    then removes its arm. [analyze] never emits this on its own; it's exposed
    for callers that already know the subtree is being drained upstream. *)

val to_string : t -> string
(** Test-output formatter. *)
