(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

(** A decorated source tree: mirrors [Frontend.Policy.t] but annotates every
    node with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. WFQ edges additionally carry the per-arm
    weight. The original [Frontend.Policy.t] is recoverable by erasing the
    decorations. Lives in its own submodule so the constructors can mirror
    [Frontend.Policy.t]'s names directly ([Decorated.RR], [Decorated.SP], …) *)
module Decorated : sig
  type t =
    | FIFO of vpifo * clss
    | UNION of vpifo * (step * t) list
    | SP of vpifo * (step * t) list
    | RR of vpifo * (step * t) list
    | WFQ of vpifo * (step * t * float) list
end

type compiled = {
  prog : program;
  decorated : Decorated.t;
}
(** The result of compiling a [Frontend.Policy.t]. Carries enough state that a
    subsequent [patch] call can extend the in-flight runtime without recompiling
    from scratch:
    - [prog]: the IR program. When this record came from a fresh compile, [prog]
      is the full program; when it came from [patch], [prog] is the *delta
      only*.
    - [decorated]: the decorated source tree. Doubles as the source-policy
      record (recoverable by erasing decorations) so [patch] can diff against an
      incoming policy without storing a separate [Frontend.Policy.t]. *)

val of_policy : Frontend.Policy.t -> compiled
(** Compile a [Frontend.Policy.t] to IR. Supports trees built from [FIFO],
    [UNION], [RR], [SP], and [WFQ]. Each node at depth [d] is placed on PE [d] —
    so all siblings (and cousins) share a PE. Builds the decorated source tree
    alongside the instruction program; a follow-up [patch] can derive the
    next-free IDs by walking [decorated]. *)

val patch : prev:compiled -> next:Frontend.Policy.t -> compiled option
(** Incrementally extend [prev] to handle policy [next], returning the IR delta.
    The returned record's [prog] is the *delta only* — the new instructions to
    add to a runtime that's already executing [prev.prog]. [decorated] is
    rebuilt to reflect [next]. Returns [None] when the change is too complex for
    this scope. The supported transitions are:
    - [next] is structurally equal to [prev]'s policy: returns [Some] with an
      empty [prog].
    - [next] adds exactly one arm at any position of a [UNION], [RR], or [SP]
      parent (per [Rio_compare.Compare.OneArmAdded]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_pol] (and [Change_weight] for [SP],
      both for the new arm and for any existing arms whose positional priority
      shifts) instructions needed to splice the new arm in.
    - Anything else returns [None] for now. *)

(** JSON exporter for IR programs. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_program : program -> Yojson.Basic.t
  (** Serialize a program as a JSON array of instruction objects. *)
end
