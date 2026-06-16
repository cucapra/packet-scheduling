(** IR types and string-conversion helpers. *)

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
  | Emancipate of step * vpifo * vpifo
      (** [Emancipate (s, parent, child)]: inverse of [Adopt]. Detach [child]
          from [parent]; [s] is the step that was used to reach [child]. *)
  | Assoc of vpifo * clss
      (** [Assoc (v, c)]: [v] begins to accept packets of class [c]. *)
  | Deassoc of vpifo * clss
      (** [Deassoc (v, c)]: [v] stops accepting packets of class [c]. *)
  | Map of vpifo * clss * step
      (** [Map (v, c, s)]: in [v]'s brain, map class [c] to step [s]. *)
  | Unmap of vpifo * clss * step
      (** [Unmap (v, c, s)]: inverse of [Map]. Forget [v]'s mapping from class
          [c] to step [s]. *)
  | Set_policy of vpifo * pol_ty * int
      (** [Set_policy (v, pol, n)]: at lPIFO birth, fix [v]'s policy type to
          [pol] and its initial arity to [n]. Issued exactly once per lPIFO at
          spawn time; the paper fixes the policy type at lPIFO birth and the
          initial arity is determined at the same moment, so the two facts ride
          on a single opcode. Later arity changes use [Change_arity]. *)
  | Change_arity of vpifo * int
      (** [Change_arity (v, n)]: set the live [v]'s arity to [n]. Shrinking
          drops rightmost slots; growing appends fresh ones. The policy type is
          unchanged. *)
  | Set_arm_meta of vpifo * step * float
      (** [Set_arm_meta (v, s, w)]: set the per-arm metadata on the child
          reached via step [s]. Today the payload is a single weight [w]; a
          later refactor will reinterpret [w] per [v]'s policy type (a weight
          for RR/WFQ; a priority rank for Strict). *)
  | GC of vpifo
      (** [GC v]: tell the garbage collector that [v] is available for
          collection. *)
  | Designate of vpifo * vpifo
      (** [Designate (v, survivor)]: form a super-node in which [v] designates
          [survivor] as its successor. While the super-node exists, traffic
          arriving at [v] continues to flow through [v]; once [v] underflows and
          is collected (per a prior [GC]), the super-node collapses to
          [survivor]. Chains across multiple [Designate]s are allowed. *)
  | Undesignate of vpifo
      (** [Undesignate v]: collapse the super-node headed by [v] (with [v] ->
          survivor) by rewiring the parent's index away from [v] and onto its
          survivor. [v] itself stays allocated; a paired [GC v] in the same
          commit releases its PE slot. *)

type commit = instr list

val string_of_pol_ty : pol_ty -> string
val string_of_instr : instr -> string
val string_of_commit : commit -> string
