(** Compilation produces [frag]s grouped by instruction kind so that the
    top-level commit can be flattened with all spawns first, then all adopts,
    etc. A parent's local frag is interleaved kindwise with each child frag via
    [combine]. *)

open Instr

type t = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  set_policies : instr list;
  change_arities : instr list;
  set_arm_metas : instr list;
  root_v : vpifo;
  classes : clss list;
}

val empty : root_v:vpifo -> classes:clss list -> t
(** A frag carrying no instructions; just [root_v] and [classes]. *)

val to_commit : t -> commit
(** Flatten a frag's seven instruction kinds in canonical order: spawns, adopts,
    assocs, maps, set_policies, change_arities, set_arm_metas. *)

val combine : t -> t list -> t
(** [combine local children] interleaves a parent's [local] frag with each of
    [children] kindwise. [root_v] and [classes] are inherited from [local]. *)

val stub : Decorated.t -> t
(** A stub frag stands in for an already-installed subtree during a [Graft]
    splice. Carries [root_v] and [classes] so the parent can [Adopt] and
    propagate routing state, but emits no instructions. *)
