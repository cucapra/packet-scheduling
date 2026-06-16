(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

(** A decorated source tree: mirrors [Rio_core.Policy.t] but annotates every
    node with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. WFQ edges additionally carry the per-arm
    weight. The original [Rio_core.Policy.t] is recoverable by erasing the
    decorations. Lives in its own submodule so the constructors can mirror
    [Rio_core.Policy.t]'s names directly ([Decorated.RR], [Decorated.SP], …) *)
module Decorated : sig
  type t =
    | FIFO of vpifo * clss
    | UNION of vpifo * (step * t) list
    | SP of vpifo * (step * t) list
    | RR of vpifo * (step * t) list
    | WFQ of vpifo * (step * t * float) list
end

type compiled = {
  commit : commit;
  decorated : Decorated.t;
  pes : pe list;
}
(** The result of compiling a [Rio_core.Policy.t]. Carries enough state that a
    subsequent [patch] call can extend the in-flight runtime without recompiling
    from scratch:
    - [commit]: the IR commit. When this record came from a fresh compile,
      [commit] is the full instruction list; when it came from [patch], [commit]
      is the *delta only*.
    - [decorated]: the decorated source tree. Doubles as the source-policy
      record (recoverable by erasing decorations) so [patch] can diff against an
      incoming policy without storing a separate [Rio_core.Policy.t].
    - [pes]: the PE assignment, indexed by depth. Every node at depth [d] lives
      on PE [List.nth pes d]. A fresh [of_policy] produces a very boring [pes]:
      [[0; 1; …; max_depth]]. But [patch] (notably [Graft]) may introduce
      non-contiguous PEs to honor the "same depth ⇒ same PE" invariant without
      re-spawning previously installed nodes. *)

val of_policy : Rio_core.Policy.t -> compiled
(** Compile a [Rio_core.Policy.t] to IR. Supports trees built from [FIFO],
    [UNION], [RR], [SP], and [WFQ]. Each node at depth [d] is placed on PE [d] —
    so all siblings (and cousins) share a PE. Builds the decorated source tree
    alongside the instruction commit; a follow-up [patch] can derive the
    next-free IDs by walking [decorated]. *)

val patch : prev:compiled -> next:Rio_core.Policy.t -> compiled option
(** Incrementally extend [prev] to handle policy [next], returning the IR delta.
    The returned record's [commit] is the *delta only* — the new instructions to
    add to a runtime that's already executing [prev.commit]. [decorated] is
    rebuilt to reflect [next]. Returns [None] when the change is too complex for
    this scope. The supported transitions are:
    - [next] is structurally equal to [prev]'s policy: returns [Some] with an
      empty [commit].
    - [next] adds exactly one arm at any position of a [UNION], [RR], or [SP]
      parent (per [ArmAdded]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_arity] (and [Set_arm_meta] for [SP],
      both for the new arm and for any existing arms whose positional priority
      shifts) instructions needed to splice the new arm in. The parent's policy
      type is fixed at lPIFO birth, so no [Set_policy] is emitted against it.
    - [next] differs from [prev] only in the weight of one [WFQ] arm (per
      [WeightChanged]): returns [Some] with a single [Set_arm_meta] instruction
      for the affected slot.
    - [next] removes exactly one arm at any position of a [UNION], [RR], or [SP]
      parent (per [ArmRemoved]): returns [Some] with the [Set_arm_meta] (only
      for [SP] siblings whose positional priority shifts down), [Change_arity],
      [Unmap], [Deassoc], [Emancipate], and [GC] instructions needed to detach
      the arm and clean up routing state cached on its ancestor chain.
    - [next] swaps in a different subtree at exactly one position (per
      [ArmReplaced]): returns [Some] with the new arm's
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Set_policy]/[Set_arm_meta] instructions, a
      [Designate] that fuses the old and new roots into a super-node riding on
      the existing parent step, [Deassoc]s that drain the old classes out of the
      displaced subtree and its ancestors, [Assoc]/[Map] entries that route the
      new classes to the same step, an [Undesignate] paired with a [GC] on the
      old root that collapses the super-node and releases its PE slot, and a
      [GC] per remaining node of the displaced subtree.
    - [next] is structurally equal to a strict subtree of [prev] at a non-empty
      path (per [ChangeRoot]): returns [Some] with an [Emancipate]/[Adopt] pair
      on the port root that re-points its single step from [prev]'s old real
      root to the new one, [Unmap]/[Deassoc] entries on the port root for any
      classes that no longer apply, and one [GC] per displaced node so the
      surrounding structure is collected. No [Emancipate] from the surviving
      subtree's old parent is emitted: the parent itself is among the [GC]'d
      nodes, which severs the edge. The whole-tree case ([path = []]) returns
      [None].
    - [prev]'s policy appears as a strict subtree of [next] at a non-empty path
      (per [Graft]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Set_policy]/[Set_arm_meta] instructions for
      the new structure surrounding [prev], a single [Adopt] that grafts
      [prev]'s existing root in at the splice point, and an [Emancipate]/
      [Adopt] pair that repoints the port root's single step from [prev]'s old
      real root to [next]'s new top. [prev]'s in-flight nodes are not respawned.
      The whole-tree case ([path = []]) returns [None]. *)

(** JSON exporter for IR commits. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_commit : commit -> Yojson.Basic.t
  (** Serialize a commit as a JSON array of instruction objects. *)
end
