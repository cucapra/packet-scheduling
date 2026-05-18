(** Compilation produces [frag]s grouped by instruction kind so that the
    top-level program can be flattened with all spawns first, then all adopts,
    etc. A parent's local frag is interleaved kindwise with each child frag via
    [combine]. *)

open Instr

type t = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  change_pols : instr list;
  change_weights : instr list;
  root_v : vpifo;
  classes : clss list;
}

val empty : root_v:vpifo -> classes:clss list -> t
(** A frag carrying no instructions; just [root_v] and [classes]. *)

val to_program : t -> program
(** Flatten a frag's six instruction kinds in canonical order: spawns, adopts,
    assocs, maps, change_pols, change_weights. *)

val combine : t -> t list -> t
(** [combine local children] interleaves a parent's [local] frag with each of
    [children] kindwise. [root_v] and [classes] are inherited from [local]. *)

val stub : Decorated.t -> t
(** A stub frag stands in for an already-installed subtree during a [SuperPol]
    splice. Carries [root_v] and [classes] so the parent can [Adopt] and
    propagate routing state, but emits no instructions. *)
