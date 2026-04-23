(** JSON exporter for IR programs. *)

val from_instr : Instr.instr -> Yojson.Basic.t
(** Serialize a single instruction as a JSON object. *)

val from_program : Instr.program -> Yojson.Basic.t
(** Serialize a program as a JSON array of instruction objects. *)
