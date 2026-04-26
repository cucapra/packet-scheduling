(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

type path = int list
(** Tree position of a node within a [Frontend.Policy.t]. Examples: [[]] is the
    root; [[0; 2]] is the third child of the first child of the root. *)

val vpifo_of_path : path -> vpifo
(** vPIFO ID for the node at [path], as a pure function of position.

    Encoding: base-10, leading sentinel digit [1], then the path's child indices
    written out as base-10 digits. So [vpifo_of_path []] is [1],
    [vpifo_of_path [0]] is [10], [vpifo_of_path [2]] is [12], and
    [vpifo_of_path [0; 1]] is [101].

    Max arity is therefore 10, which is a little silly but let's go with it for
    now haha *)

val step_of_path : path -> int -> step
(** [step_of_path parent_path child_index] is a Step ID that the node at
    [parent_path] will use to reach its [child_index]th child. Same encoding as
    [vpifo_of_path] but with leading sentinel [2], so the step and vPIFO ID
    spaces never collide. E.g. [step_of_path [] 0] is [20], [step_of_path [] 2]
    is [22], [step_of_path [0] 1] is [201]. *)

type compiled = {
  prog : program;
  policy : Frontend.Policy.t;
}
(** The result of compiling a [Frontend.Policy.t]. Carries enough state that a
    subsequent [patch] call can extend the in-flight runtime without recompiling
    from scratch:
    - [prog]: the IR program. When this record came from a fresh compile, [prog]
      is the full program; when it came from [patch], [prog] is the *delta
      only*.
    - [policy]: the [Frontend.Policy.t] this record was built from. Lets a
      future [patch] call diff against an incoming policy. Because IDs are
      content-addressed, [policy] is also enough to recover any node's IDs on
      demand via [vpifo_of_path] / [step_of_path] — no separate identity table
      is needed. *)

val of_policy : Frontend.Policy.t -> compiled
(** Compile a [Frontend.Policy.t] to IR. Supports trees of any height built from
    [FIFO], [UNION], [RR], [SP], and [WFQ]. Each node at depth [d] is placed on
    PE [d] — so all siblings (and cousins) share a PE. *)

val patch : prev:compiled -> next:Frontend.Policy.t -> compiled option
(** Incrementally extend [prev] to handle policy [next], returning the IR delta.
    The returned record's [prog] is the *delta only* — the new instructions to
    splice into a runtime that's already executing [prev.prog]. [policy] is set
    to [next].

    Returns [None] when the change is too complex for this scope. The supported
    transitions are:
    - [next] is structurally equal to [prev.policy]: returns [Some] with an
      empty [prog].
    - [next] adds exactly one arm at the end of a [UNION], [RR], or [SP] parent
      in [prev.policy] (per [Rio_compare.Compare.OneArmAppended]): returns
      [Some] with the [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_pol] (and
      [Change_weight] for [SP]) instructions needed to splice the new arm in.

    Anything else — including the broader [Rio_compare.Compare.ArmsAdded] case
    (mid-insert, multi-arm add, weighted-arm add) and any [VeryDifferent] /
    [SuperPol] result — returns [None]. *)

(** JSON exporter for IR programs. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_program : program -> Yojson.Basic.t
  (** Serialize a program as a JSON array of instruction objects. *)
end
