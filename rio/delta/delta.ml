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
  | Remove of {
      path : path;
      arm : Rio_core.Pol.t;
    }
      (** Remove the child at [path]; [arm] records the dropped subtree's shape
          (carried so confinement reasoning can see what left). *)
  | ChangeMeta of {
      path : path;
      new_meta : float;
    }
      (** Rebind the per-arm metadata of the child at [path] to [new_meta] (rank
          for SP, weight for WFQ; never reaches RR). *)
  | Graft of path
      (** [prev] sits as a strict subtree of [next] at [path]:
          [next = ctx[prev]] where [ctx]'s hole is at [path]. *)
  | ChangeRoot of path
      (** [next] is a strict subtree of [prev] at [path]: pruning [prev] down to
          that subtree gives [next]. *)
  | Designate of {
      path : path;
      arm : Rio_core.Pol.t;
    }
      (** Introduce [arm] alongside the slot at [path] under a designated
          super-node favoring the existing arm. [den(Designate) = id]; the
          super-node collapses once the loser quiesces. *)
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
  | Remove { path; arm } -> Remove { path = i :: path; arm }
  | ChangeMeta { path; new_meta } -> ChangeMeta { path = i :: path; new_meta }
  | Graft p -> Graft (i :: p)
  | ChangeRoot p -> ChangeRoot (i :: p)
  | Designate { path; arm } -> Designate { path = i :: path; arm }
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
  | Remove { path; arm } -> Printf.sprintf "Remove: %s" (string_of_arm path arm)
  | ChangeMeta { path; new_meta } ->
      Printf.sprintf "ChangeMeta: %s -> %g" (path_to_string path) new_meta
  | Graft p -> Printf.sprintf "Graft at %s" (path_to_string p)
  | ChangeRoot p -> Printf.sprintf "ChangeRoot at %s" (path_to_string p)
  | Designate { path; arm } ->
      Printf.sprintf "Designate: %s" (string_of_arm path arm)
  | Quiesce p -> Printf.sprintf "Quiesce at %s" (path_to_string p)
  | Undesignate p -> Printf.sprintf "Undesignate at %s" (path_to_string p)
