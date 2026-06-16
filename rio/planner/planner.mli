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
    multi-arm divergences) expand into the paper's give-up idiom
    [Designate ; Quiesce ; Undesignate] sequence at the affected slot. *)

val retire : arm:Rio_core.Pol.t -> unit -> t
(** The paper's [Retire] idiom: [Quiesce(p) ; (Empty p) Remove(p, arm)]. Emitted
    with [path = []] at the local level; the caller prepends the target index
    via [prepend_seq]. *)

val to_string : t -> string
(** Test-output formatter. *)
