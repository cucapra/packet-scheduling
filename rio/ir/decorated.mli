(** A decorated source tree: mirrors [Rio_core.Policy.t] but annotates every
    node with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. WFQ edges additionally carry the per-arm
    weight. The original [Rio_core.Policy.t] is recoverable by erasing the
    decorations. *)

open Instr

type t =
  | FIFO of vpifo * clss
  | SP of vpifo * (step * t * float) list
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
(** One vPIFO per node. *)

val count_steps : t -> int
(** One step per parent-to-child edge. *)

val subtree_vpifos : t -> vpifo list
(** All vPIFO IDs in [d], pre-order. *)

val subtree_classes : t -> clss list
(** All leaf classes in [d], pre-order. Each ancestor of [d] holds an [Assoc]
    for every class in this list. *)

val subtree_class_assocs : t -> (vpifo * clss list) list
(** Pair every node's vPIFO with the classes it was [Assoc]'d with during
    compilation: a leaf carries its single class, an internal node carries the
    union of its descendants' classes. *)

val walk : t -> int list -> t
(** Subtree at [path]; [[]] is the tree itself. Errors on a path that goes
    through a FIFO leaf. *)

val ancestor_chain : t -> int list -> (vpifo * step) list
(** For each ancestor of the node at [path], return its vPIFO and the step it
    uses to reach the next node on the path. Length equals [List.length path].
*)

val rewrite_at : t -> int list -> (t -> t) -> t
(** Apply [f] to the subtree at [path], leaving the surrounding structure
    (including WFQ weights along the path) untouched. *)

val insert_arm : int -> step -> t -> t -> t
(** [insert_arm k new_step new_child d] splices [new_child] in at index [k] in
    the children of [d], using [new_step] as the parent-to-child step. Errors on
    FIFO, SP, and WFQ; use [insert_arm_sp] / [insert_arm_wfq] for those. *)

val insert_arm_sp : int -> step -> t -> float -> t -> t
(** [insert_arm_sp k new_step new_child new_rank d] splices [new_child] in at
    index [k] in the children of SP-rooted [d], pairing it with [new_step] and
    [new_rank]. Errors on anything but SP. *)

val insert_arm_wfq : int -> step -> t -> float -> t -> t
(** [insert_arm_wfq k new_step new_child new_weight d] splices [new_child] in at
    index [k] in the children of WFQ-rooted [d], pairing it with [new_step] and
    [new_weight]. Errors on anything but WFQ. *)

val drop_arm : int -> t -> t
(** Remove the child at index [k]. Errors on FIFO. *)

val set_meta : int -> float -> t -> t
(** Set the [k]-th child's per-arm meta (rank for SP, weight for WFQ). Errors on
    RR and FIFO. *)

val replace_arm : int -> t -> t -> t
(** Replace child at index [k], preserving the parent-to-child step (and WFQ
    weight). Errors on FIFO. *)

val to_policy : t -> Rio_core.Policy.t
(** Erase decorations to recover the underlying source policy. Inverse of the
    pairing produced by [Ir.of_policy]. *)
