(** A decorated source tree: mirrors [Frontend.Policy.t] but annotates every
    node with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. WFQ edges additionally carry the per-arm
    weight. The original [Frontend.Policy.t] is recoverable by erasing the
    decorations. *)

open Instr

type t =
  | FIFO of vpifo * clss
  | UNION of vpifo * (step * t) list
  | SP of vpifo * (step * t) list
  | RR of vpifo * (step * t) list
  | WFQ of vpifo * (step * t * float) list

val root_vpifo : t -> vpifo
(** The vPIFO at the root of [d]. *)

val pol_ty : t -> pol_ty
(** The IR-side [pol_ty] of [d]. *)

val arity : t -> int
(** Number of immediate children. Errors on FIFO. *)

val nth_step : t -> int -> step
(** The k-th edge's adopt-step. Errors on FIFO. *)

val nth_child : t -> int -> t
(** The k-th child subtree. Errors on FIFO. *)

val count_vpifos : t -> int
(** One vPIFO per node — the size of the tree. *)

val count_steps : t -> int
(** One step per parent→child edge. *)

val subtree_vpifos : t -> vpifo list
(** All vPIFO IDs in [d], pre-order. *)

val subtree_classes : t -> clss list
(** All leaf classes in [d], pre-order. Each ancestor of [d] holds an [Assoc]
    for every class in this list. *)

val subtree_class_assocs : t -> (vpifo * clss list) list
(** Pair every node's vPIFO with the classes it was [Assoc]'d to during
    compilation: a leaf carries its single class, an internal node carries the
    union of its descendants' classes. *)

val walk : t -> int list -> t
(** Subtree at [path]; [[]] is [d] itself. Errors on a path that goes through a
    FIFO leaf. *)

val ancestor_chain : t -> int list -> (vpifo * step) list
(** For each ancestor of the node at [path], return its vPIFO and the step it
    uses to reach the next node on the path. Length equals [List.length path].
*)

val rewrite_at : t -> int list -> (t -> t) -> t
(** Apply [f] to the subtree at [path], leaving the surrounding structure
    (including WFQ weights along the path) untouched. *)

val insert_arm : int -> step -> t -> t -> t
(** [insert_arm k new_step new_child d] splices [new_child] in at index [k] in
    the children of [d], using [new_step] as the parent→child step. Errors on
    FIFO and WFQ. *)

val drop_arm : int -> t -> t
(** Remove the child at index [k]. Errors on FIFO. *)

val set_weight : int -> float -> t -> t
(** Set the weight of the [k]-th WFQ child. Errors on anything but WFQ. *)

val replace_arm : int -> t -> t -> t
(** Replace child at index [k], preserving the parent→child step (and WFQ
    weight). Errors on FIFO. *)
