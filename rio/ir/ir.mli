(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

(** A decorated source tree: mirrors [Frontend.Policy.t] but annotates every
    node with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. WFQ edges additionally carry the per-arm
    weight. The original [Frontend.Policy.t] is recoverable by erasing the
    decorations.

    Lives in its own submodule so the constructors can mirror
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
  pes : pe list;
}
(** The result of compiling a [Frontend.Policy.t]. Carries enough state that a
    subsequent [patch] call can extend the in-flight runtime without recompiling
    from scratch:

    - [prog]: the IR program. When this record came from a fresh compile, [prog]
      is the full program; when it came from [patch], [prog] is the *delta
      only*.
    - [decorated]: the decorated source tree. Doubles as the source-policy
      record (recoverable by erasing decorations) so [patch] can diff against an
      incoming policy without storing a separate [Frontend.Policy.t].
    - [pes]: the PE assignment, indexed by depth. Every node at depth [d] lives
      on PE [List.nth pes d]. A fresh [of_policy] produces
      [[0; 1; …; max_depth]]; [patch] (notably [SuperPol]) may introduce
      non-contiguous PEs to honor the "same depth ⇒ same PE" invariant without
      re-spawning previously installed nodes. *)

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
    rebuilt to reflect [next].

    Returns [None] when the change is too complex for this scope. The supported
    transitions are:

    - [next] is structurally equal to [prev]'s policy: returns [Some] with an
      empty [prog].
    - [next] adds exactly one arm at any position of a [UNION], [RR], or [SP]
      parent (per [Rio_compare.Compare.OneArmAdded]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_pol] (and [Change_weight] for [SP],
      both for the new arm and for any existing arms whose positional priority
      shifts) instructions needed to splice the new arm in.
    - [next] differs from [prev] only in the weight of one [WFQ] arm (per
      [Rio_compare.Compare.WeightChanged]): returns [Some] with a single
      [Change_weight] instruction for the affected slot.
    - [next] removes exactly one arm at any position of a [UNION], [RR], or [SP]
      parent (per [Rio_compare.Compare.OneArmRemoved]): returns [Some] with the
      [Change_weight] (for [SP] siblings whose positional priority shifts down),
      [Change_pol], [Unmap], [Deassoc], [Emancipate], and [GC] instructions
      needed to detach the arm and clean up routing state cached on its ancestor
      chain.
    - [next] swaps in a different subtree at exactly one position (per
      [Rio_compare.Compare.OneArmReplaced]): returns [Some] with the new arm's
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_pol]/[Change_weight] instructions, a
      [Designate] that fuses the old and new roots into a super-node riding on
      the existing parent step, [Deassoc]s that drain the old classes out of the
      displaced subtree and its ancestors, [Assoc]/[Map] entries that route the
      new classes to the same step, and a [GC] per node of the displaced subtree
      so it's collected once it underflows. The whole-tree case ([path = []])
      rides on the fake root's single step instead of an internal parent — the
      shape of the emitted instructions is the same.

    - [next] is wholesale different from [prev] (per
      [Rio_compare.Compare.VeryDifferent []]): same handler as the whole-tree
      [OneArmReplaced] case above. The fake root's classifier is rewritten from
      [prev]'s classes to [next]'s, the old root is [Designate]d so its
      in-flight traffic drains, and every prev vpifo is [GC]'d. A
      [VeryDifferent] result with a non-empty path (a deep multi-arm divergence)
      still returns [None].

    - [next] is structurally equal to a strict subtree of [prev] at a non-empty
      path (per [Rio_compare.Compare.SubPol]): returns [Some] with an
      [Emancipate] detaching that subtree from its parent, a [Change_root]
      pointing the runtime at it, and a [GC] per displaced node so the
      surrounding structure is collected. The whole-tree case ([path = []])
      returns [None].

    - [prev]'s policy appears as a strict subtree of [next] at a non-empty path
      (per [Rio_compare.Compare.SuperPol]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_pol]/[Change_weight] instructions
      for the new structure surrounding [prev], a single [Adopt] that grafts
      [prev]'s existing root in at the splice point, and a [Change_root] that
      retargets the runtime at [next]'s new top. [prev]'s in-flight nodes are
      not respawned. The whole-tree case ([path = []]) returns [None].

    Anything else returns [None]. *)

(** JSON exporter for IR programs. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_program : program -> Yojson.Basic.t
  (** Serialize a program as a JSON array of instruction objects. *)
end
