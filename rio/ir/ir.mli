(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

(** A decorated source tree: mirrors [Rio_core.Pol.t] but annotates every node
    with the [vpifo] assigned to it and every parent-to-child edge with the
    [step] handed out at adoption time. SP edges additionally carry a per-arm
    priority rank; WFQ edges carry a per-arm weight. The original
    [Rio_core.Pol.t] is recoverable by erasing the decorations. Lives in its own
    submodule so the constructors can mirror [Rio_core.Pol.t]'s names directly
    ([Decorated.RR], [Decorated.SP], …) *)
module Decorated : sig
  type t =
    | FIFO of vpifo * clss
    | SP of vpifo * (step * t * float) list * bool
    | RR of vpifo * (step * t) list
    | WFQ of vpifo * (step * t * float) list
end

type compiled = {
  steps : (Rio_planner.Planner.guard * commit) list;
  decorated : Decorated.t;
  pes : pe list;
}
(** The result of compiling a [Rio_core.Pol.t]. Carries enough state that a
    subsequent [patch] call can extend the in-flight runtime without recompiling
    from scratch:
    - [steps]: the IR output as a list of guarded commits. Each step is a
      [(guard, commit)] pair: the substrate fires the [commit]'s opcodes once
      the [guard] becomes true (a [True] guard fires immediately; an [Empty p]
      guard waits until the subtree at [p] has drained its in-flight packets). A
      fresh compile from [of_policy] is a single-element list under [True]. An
      incremental [patch] returns one step per planner-emitted production, so
      [Retire] is two steps (Quiesce under [True], Remove under [Empty]),
      [Replace] is three or four steps (Designate under [True], Quiesce under
      [True], Undesignate under [Empty], and an optional trailing ChangeMeta
      under [True]), [PruneDownTo] is several steps capped by a final
      [ChangeRoot] under [True], and so on.
    - [decorated]: the decorated source tree. Doubles as the source-policy
      record (recoverable by erasing decorations) so [patch] can diff against an
      incoming policy without storing a separate [Rio_core.Pol.t].
    - [pes]: the PE assignment, indexed by depth. Every node at depth [d] lives
      on PE [List.nth pes d]. A fresh [of_policy] produces a very boring [pes]:
      [[0; 1; …; max_depth]]. But [patch] (notably [Graft]) may introduce
      non-contiguous PEs to honor the "same depth ⇒ same PE" invariant without
      re-spawning previously installed nodes. *)

val string_of_steps : (Rio_planner.Planner.guard * commit) list -> string
(** Pretty-print a guarded commit list. Each step prints as
    [@<guard>: <pretty-printed commit>] on its own line. *)

val of_policy : Rio_core.Pol.t -> compiled
(** Compile a [Rio_core.Pol.t] to IR. Supports trees built from [FIFO], [RR],
    [SP], and [WFQ]. Each node at depth [d] is placed on PE [d]; so all siblings
    (and cousins) share a PE. Builds the decorated source tree alongside the
    instruction commit; a follow-up [patch] can derive the next-free IDs by
    walking [decorated]. *)

val patch : prev:compiled -> next:Rio_core.Pol.t -> compiled option
(** Incrementally extend [prev] to handle policy [next], returning the IR delta.
    The returned record's [commit] is the *delta only* — the new instructions to
    add to a runtime that's already executing [prev.commit]. [decorated] is
    rebuilt to reflect [next]. Returns [None] when the change is too complex for
    this scope. The supported transitions are:
    - [next] is structurally equal to [prev]'s policy: returns [Some] with an
      empty [commit].
    - [next] adds exactly one arm at any position of a [RR] or [SP] parent (per
      [Delta.Add]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Change_arity] (and [Set_arm_meta] for [SP],
      carrying the new arm's priority rank) instructions needed to splice the
      new arm in. Existing SP arms keep their ranks; no positional cascade is
      emitted. The parent's policy type is fixed at lPIFO birth, so no
      [Set_policy] is emitted against it.
    - [next] differs from [prev] only in the per-arm meta (rank for [SP], weight
      for [WFQ]) of one slot (per [ChangeMeta]): returns [Some] with a single
      [Set_arm_meta] instruction for the affected slot.
    - [next] removes exactly one arm at any position of a [RR] or [SP] parent
      (planner expands this as the [Retire] idiom
      [Quiesce(p) ; (Empty p) Remove(p, arm)], recognized by [patch]): returns
      [Some] with the [Change_arity], [Unmap], [Deassoc], [Emancipate], and [GC]
      instructions needed to detach the arm and clean up routing state cached on
      its ancestor chain. Existing SP siblings keep their ranks.
    - [next] swaps in a different subtree at exactly one position (planner
      expands this as the [Replace] idiom
      [Designate(p) ; Quiesce(p ++ [0]) ; (Empty (p ++ [0])) Undesignate(p) (;
       (True) ChangeMeta(p))?]): returns [Some] with a multi-step
      [compiled.steps]. The [Designate] step spawns a fresh SP* super-node
      (sharing PE n with the loser and the survivor's root), compiles the new
      arm fresh, and rewires the parent edge to point at SP*. The [Quiesce] step
      tears down loser-only routing and swings shared classes from loser to
      survivor at SP*. The [Undesignate] step (gated on the loser being empty)
      collapses SP* by rewiring the parent edge back to the survivor and GCs SP*
      \+ the loser subtree.
    - [next] is structurally equal to a strict subtree of [prev] at a non-empty
      path (planner expands this as the [PruneDownTo] idiom
      [Retire(p_1) ; ... ; Retire(p_m) ; (True) ChangeRoot([0;...;0])],
      recognized by [patch] via the trailing all-zeros [ChangeRoot]; the
      original target path is recovered via [Planner.is_sub_policy]): returns
      [Some] with an [Emancipate]/[Adopt] pair on the port root that re-points
      its single step from [prev]'s old real root to the new one,
      [Unmap]/[Deassoc] entries on the port root for any classes that no longer
      apply, and one [GC] per displaced node so the surrounding structure is
      collected. No [Emancipate] from the surviving subtree's old parent is
      emitted: the parent itself is among the [GC]'d nodes, which severs the
      edge. The whole-tree case ([path = []]) returns [None].
    - [prev]'s policy appears as a strict subtree of [next] at a non-empty path
      (per [Graft]): returns [Some] with the
      [Spawn]/[Adopt]/[Assoc]/[Map]/[Set_policy]/[Set_arm_meta] instructions for
      the new structure surrounding [prev], a single [Adopt] that grafts
      [prev]'s existing root in at the splice point, and an [Emancipate]/
      [Adopt] pair that repoints the port root's single step from [prev]'s old
      real root to [next]'s new top. [prev]'s in-flight nodes are not respawned.
      The whole-tree case ([path = []]) returns [None]. *)

val patch_designate :
  prev:compiled -> path:int list -> arm:Rio_core.Pol.t -> compiled
(** Per-production lowering for [Delta.Designate]. Stands up a fresh SP*
    super-node at [path]: the existing subtree becomes child 0 (the loser,
    favored), and a freshly compiled [arm] becomes child 1 (the survivor). SP*,
    loser, and survivor's root all live on the loser's original PE. Emits
    [arm]'s compile instructions, [Spawn] for SP*, two [Adopt]s under SP*,
    [Designate (loser_v, survivor_v)] (the §6 ISA marker), an
    [Emancipate]/[Adopt] pair that rewires the parent edge from loser to SP*,
    routing entries at SP* (loser classes via [loser_step], survivor-only via
    [surv_step]), chain-above [Assoc]/[Map] for survivor-only classes, and
    [Set_policy (sp_v, SP_star, 2)] + per-arm meta. *)

val patch_quiesce : prev:compiled -> path:int list -> compiled
(** Per-production lowering for [Delta.Quiesce]. Tears down the class-routing
    entries that direct new traffic into the subtree at [path], along its
    ancestor chain. In-flight packets continue to dequeue. [den(Quiesce) = id]
    so the decorated tree is unchanged. SP*-aware: when [path]'s parent is a
    designated SP, shared classes between loser and survivor are preserved above
    SP* and rerouted at SP* from [loser_step] to [surv_step] so the survivor can
    take over once the loser drains. *)

val patch_undesignate : prev:compiled -> path:int list -> compiled
(** Per-production lowering for [Delta.Undesignate]. Collapses the designated
    SP* super-node at [path]: rewires the parent edge from SP* to the survivor
    (child 1), emits [Undesignate loser_v] as the §6 ISA marker, and GCs SP*
    + the loser subtree. *)

(** JSON exporter for IR commits. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_commit : commit -> Yojson.Basic.t
  (** Serialize a commit as a JSON array of instruction objects. *)
end
