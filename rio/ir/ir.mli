(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

(* Re-export the IR types and string-conversion helpers from [Instr]. They
   live there (rather than here) so that other modules in this library can
   reference them without depending on [Ir]. *)
include module type of Instr

val of_policy : Frontend.Policy.t -> program
(** Compile a [Frontend.Policy.t] to IR. Supports trees of any height built from
    [FIFO], [UNION], [RR], [Strict], and [WFQ]. Each node at depth [d] is placed
    on PE [d] (so all siblings — and cousins — share a PE). Declared classes
    that do not appear in the returned policy are silently dropped, per the DSL
    semantics. *)

(** JSON exporter for IR programs. *)
module Json : sig
  val from_instr : instr -> Yojson.Basic.t
  (** Serialize a single instruction as a JSON object. *)

  val from_program : program -> Yojson.Basic.t
  (** Serialize a program as a JSON array of instruction objects. *)
end
