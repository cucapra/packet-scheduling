(** JSON exporter for IR commits. *)

val from_instr : Instr.instr -> Yojson.Basic.t
(** Serialize a single instruction as a JSON object. *)

val from_commit : Instr.commit -> Yojson.Basic.t
(** Serialize a commit as a JSON array of instruction objects. *)
