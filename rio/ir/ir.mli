(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include module type of Instr

type node_id = int list
(** Tree position of a node within a [Frontend.Policy.t]. [[]] is the root;
    [[0; 2]] is the third child of the first child of the root; and so on. *)

type identities = {
  vpifos : (node_id, vpifo) Hashtbl.t;
  steps : (node_id * int, step) Hashtbl.t;
      (** [(parent_path, child_index)] → step ID assigned at the moment the
          parent adopted that child. *)
}
(** Identity tables: which vPIFO/step IDs were assigned to which positions in
    the source policy tree. Populated by [of_policy] and consulted by [patch]
    (Milestone 2 PR3) to splice new arms onto an existing root.

    [vpifos] is keyed by [node_id]: every node in the policy tree (every FIFO
    leaf and every internal UNION/RR/SP/WFQ root) gets exactly one entry.
    [steps] is keyed by [(parent_path, child_index)]: every adopt instruction
    gets exactly one entry, recording the step ID that was handed out the moment
    the parent adopted that child. *)

type compiled = {
  prog : program;
  policy : Frontend.Policy.t;
  identities : identities;
  next_vpifo : int;
  next_step : int;
}
(** The result of compiling a [Frontend.Policy.t]. Carries enough state that a
    subsequent [patch] call (Milestone 2) can extend the in-flight runtime
    without recompiling from scratch:

    - [prog]: the IR program. When this record came from a fresh compile, [prog]
      is the full program; when it came from [patch], [prog] is the *delta
      only*.
    - [policy]: the [Frontend.Policy.t] this record was built from. Lets a
      future [patch] call diff against an incoming policy.
    - [identities]: vPIFO and step IDs indexed by their tree position.
    - [next_vpifo], [next_step]: the next IDs the counters would hand out, so
      [patch] can keep allocating without colliding with previous assignments.
*)

val of_policy : Frontend.Policy.t -> compiled
(** Compile a [Frontend.Policy.t] to IR. Supports trees of any height built from
    [FIFO], [UNION], [RR], [SP], and [WFQ]. Each node at depth [d] is placed on
    PE [d] — so all siblings (and cousins) share a PE. Populates the identity
    tables and the [next_vpifo]/[next_step] counter snapshots so that a
    follow-up [patch] can extend this compile in place. *)

val patch : prev:compiled -> next:Frontend.Policy.t -> compiled option
(** Incrementally extend [prev] to handle policy [next], returning the IR delta.
    The returned record's [prog] is the *delta only* — the new instructions to
    splice into a runtime that's already executing [prev.prog]. [policy] is set
    to [next]; [identities] and the counter snapshots are extensions of
    [prev]'s, working on a clone, so [prev] is left untouched.

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
