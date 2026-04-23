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
    the source policy tree. Populated by [of_policy] (in PR2 — empty for now)
    and consulted by [patch] to splice new arms onto an existing root. *)

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
    PE [d] — so all siblings (and cousins) share a PE.

    Note: in this PR the returned [identities] tables are empty placeholders;
    they get populated in PR2. The instruction list and counter snapshots are
    fully accurate. *)

(** JSON exporter for compiled IR programs. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_compiled : compiled -> Yojson.Basic.t
  (** Serialize a [compiled] as a wrapped JSON object:
      {[
        { "instrs":     [ ...instructions... ],
          "identities": { "vpifos": { ... }, "steps": { ... } },
          "next_vpifo": <int>,
          "next_step":  <int> }
      ]}
      Identity table keys are stringified tree paths: dot-separated child
      indices (root = [""]). Step keys append ["/<child_index>"] to the parent's
      path. Empty in PR1. *)
end
