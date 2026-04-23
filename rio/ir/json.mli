(** Per-instruction JSON encoding. The wrapped [from_compiled] exporter that
    consumers actually use lives in [Ir.Json] (see [ir.mli]). *)

val from_instr : Instr.instr -> Yojson.Basic.t
(** Serialize a single instruction as a JSON object. *)
