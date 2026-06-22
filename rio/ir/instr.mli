(** IR types and string-conversion helpers. *)

type pe = int
type pifo = int
type step = int
type clss = string

type pol_ty =
  | FIFO
  | RR
  | SP
  | WFQ
  | SP_star
      (** Marker pol_ty announcing that a v is the head of a designated
          super-node. Emitted only via [Set_policy (v, SP_star, 2)] in the
          lowering of [Delta.Designate]; tells the substrate to allocate
          super-node hardware for [v]. Never produced by the source DSL or by
          [of_policy]. *)

type instr =
  | Spawn of pifo * pe
      (** [Spawn (v, pe)]: add a fresh empty PIFO [v] to [pe]. *)
  | Adopt of step * pifo * pifo
      (** [Adopt (s, parent, child)]: tell [parent] that [child] is its child;
          [s] is the step [parent] uses to refer to [child]. *)
  | Emancipate of step * pifo
      (** [Emancipate (s, parent)]: inverse of [Adopt]. Detach the child that
          [parent] reaches via step [s]. The substrate addresses children by
          per-edge index, so [(parent, s)] alone identifies the edge to sever.
      *)
  | Assoc of pifo * clss
      (** [Assoc (v, c)]: [v] begins to accept packets of class [c]. *)
  | Deassoc of pifo * clss
      (** [Deassoc (v, c)]: [v] stops accepting packets of class [c]. *)
  | Map of pifo * clss * step
      (** [Map (v, c, s)]: in [v]'s brain, map class [c] to step [s]. *)
  | Unmap of pifo * clss
      (** [Unmap (v, c)]: inverse of [Map]. Forget [v]'s mapping for class [c].
          The step the mapping pointed at is recoverable from [v]'s brain, so it
          is not carried on the opcode. *)
  | Set_policy of pifo * pol_ty * int
      (** [Set_policy (v, pol, n)]: at PIFO birth, fix [v]'s policy type to
          [pol] and its initial arity to [n]. Issued exactly once per PIFO at
          spawn time; the paper fixes the policy type at PIFO birth and the
          initial arity is determined at the same moment, so the two facts ride
          on a single opcode. Later arity changes use [Change_arity]. *)
  | Change_arity of pifo * int
      (** [Change_arity (v, n)]: set the live [v]'s arity to [n]. Shrinking
          drops rightmost slots; growing appends fresh ones. The policy type is
          unchanged. *)
  | Set_arm_meta of pifo * step * float
      (** [Set_arm_meta (v, s, w)]: set the per-arm metadata on the child
          reached via step [s]. Today the payload is a single weight [w]; a
          later refactor will reinterpret [w] per [v]'s policy type (a weight
          for RR/WFQ; a priority rank for Strict). *)
  | GC of pifo
      (** [GC v]: tell the garbage collector that [v] is available for
          collection. *)
  | Designate of pifo * pifo
      (** [Designate (v, survivor)]: form a super-node in which [v] designates
          [survivor] as its successor. While the super-node exists, traffic
          arriving at [v] continues to flow through [v]; once [v] underflows and
          is collected (per a prior [GC]), the super-node collapses to
          [survivor]. Chains across multiple [Designate]s are allowed. *)
  | Undesignate of pifo
      (** [Undesignate v]: collapse the super-node headed by [v] (with [v] ->
          survivor) by rewiring the parent's index away from [v] and onto its
          survivor. [v] itself stays allocated; a paired [GC v] in the same
          commit releases its PE slot. *)

type commit = instr list

val string_of_pol_ty : pol_ty -> string
val string_of_instr : instr -> string
val string_of_commit : commit -> string
