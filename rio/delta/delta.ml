type path = int list

(** Atomic productions of the paper's [delta] grammar (paper/sketch.md sec3.3).
    Each constructor names a single edit that, when composed via the planner's
    guarded sequences (paper/sketch.md sec4), advances one policy to another.

    The [meta] field on [Add] carries the new arm's per-arm metadata,
    interpreted by the parent's discipline: a priority rank for SP, a weight for
    WFQ. It is [None] for RR.

    [Replace] is intentionally absent: the planner expands a slot-swap as
    [Designate ; Quiesce ; Undesignate] (the give-up idiom of sec4.2). *)
type t =
  | Add of {
      path : path;
      arm : Rio_core.Pol.t;
      meta : float option;
    }
      (** Insert [arm] as a new child at [path]. [meta] is the new arm's per-arm
          metadata: a rank for SP, a weight for WFQ, [None] for RR. *)
  | Remove of path  (** Remove the child at [path]. *)
  | ChangeMeta of {
      path : path;
      new_meta : float;
    }
      (** Rebind the per-arm metadata of the child at [path] to [new_meta] (rank
          for SP, weight for WFQ; never reaches RR). *)
  | ChangeRoot of path
      (** [next] is a strict subtree of [prev] at [path]: pruning [prev] down to
          that subtree gives [next]. *)
  | Designate of {
      path : path;
      survivor : Rio_core.Pol.t;
    }
      (** Introduce [survivor] alongside the slot at [path] under a designated
          super-node favoring the existing arm (the loser).
          [den(Designate) = id]; the super-node collapses once the loser
          quiesces. *)
  | Quiesce of path
      (** Drain the subtree at [path]: no further enqueues, dequeues continue
          until empty. [den(Quiesce) = id]. *)
  | Undesignate of path
      (** Collapse the designated super-node at [path] onto its survivor. *)

(* Tack [i] onto the front of every embedded [path]. Used by the planner when
   descending: a child's sequence comes back parent-relative, and we tack on
   the child's own index to make it grandparent-relative. *)
let prepend_path i = function
  | Add { path; arm; meta } -> Add { path = i :: path; arm; meta }
  | Remove p -> Remove (i :: p)
  | ChangeMeta { path; new_meta } -> ChangeMeta { path = i :: path; new_meta }
  | ChangeRoot p -> ChangeRoot (i :: p)
  | Designate { path; survivor } -> Designate { path = i :: path; survivor }
  | Quiesce p -> Quiesce (i :: p)
  | Undesignate p -> Undesignate (i :: p)

(* Pretty-printers used to format test-failure messages. *)

let path_to_string = function
  | [] -> "(root)"
  | [ i ] -> Printf.sprintf "(child %d)" i
  | p ->
      let s = String.concat "->" (List.map string_of_int p) in
      Printf.sprintf "(path %s)" s

let string_of_arm path arm =
  Printf.sprintf "%s at %s" (Rio_core.Pol.to_string arm) (path_to_string path)

let string_of_arm_w path arm weight =
  Printf.sprintf "%s (weight %g) at %s"
    (Rio_core.Pol.to_string arm)
    weight (path_to_string path)

let to_string = function
  | Add { path; arm; meta = None } ->
      Printf.sprintf "Add: %s" (string_of_arm path arm)
  | Add { path; arm; meta = Some m } ->
      Printf.sprintf "Add: %s" (string_of_arm_w path arm m)
  | Remove p -> Printf.sprintf "Remove at %s" (path_to_string p)
  | ChangeMeta { path; new_meta } ->
      Printf.sprintf "ChangeMeta: %s -> %g" (path_to_string path) new_meta
  | ChangeRoot p -> Printf.sprintf "ChangeRoot at %s" (path_to_string p)
  | Designate { path; survivor } ->
      Printf.sprintf "Designate: %s" (string_of_arm path survivor)
  | Quiesce p -> Printf.sprintf "Quiesce at %s" (path_to_string p)
  | Undesignate p -> Printf.sprintf "Undesignate at %s" (path_to_string p)
