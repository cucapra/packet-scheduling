(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

type pe = int
type vpifo = int
type step = int
type clss = string

type pol_ty =
  | FIFO
  | RR
  | SP
  | WFQ
  | UNION

type instr =
  | Spawn of vpifo * pe
      (** [Spawn (v, pe)]: add a fresh empty vPIFO [v] to [pe]. *)
  | Adopt of step * vpifo * vpifo
      (** [Adopt (s, parent, child)]: tell [parent] that [child] is its child;
          [s] is the step [parent] uses to refer to [child]. *)
  | Assoc of vpifo * clss
      (** [Assoc (v, c)]: [v] begins to accept packets of class [c]. *)
  | Deassoc of vpifo * clss
      (** [Deassoc (v, c)]: [v] stops accepting packets of class [c]. *)
  | Map of vpifo * clss * step
      (** [Map (v, c, s)]: in [v]'s brain, map class [c] to step [s]. *)
  | Change_pol of vpifo * pol_ty * int
      (** [Change_pol (v, pol, n)]: set [v]'s policy to [pol] with [n] arms. *)
  | Change_weight of vpifo * step * int
      (** [Change_weight (v, s, w)]: the child reached via step [s] has weight
          [w]. *)

type program = instr list

val string_of_pol_ty : pol_ty -> string
val string_of_instr : instr -> string
val string_of_program : program -> string

exception UnsupportedPolicy of string
(** Raised when the input [Frontend.Policy.t] falls outside MS1 compiler's
    supported shape — i.e., anything that is NOT a one-level work-conserving
    policy. The string carries a human-readable reason. *)

val of_policy : Frontend.Policy.t -> program
(** Compile a [Frontend.Policy.t] to IR. Supported shapes are:
    - [FIFO c]
    - [UNION children], [RR children], [Strict children],
      [WFQ (children, weights)] where every child is a [FIFO c] (a single-class
      leaf with no children).

    Any other shape (a nested policy node, [EDF], …) raises [UnsupportedPolicy].
    Declared classes that do not appear in the returned policy are silently
    dropped, per the DSL semantics. *)
