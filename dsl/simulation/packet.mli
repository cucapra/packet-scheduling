type t

val len : t -> float
val time : t -> Time.t
val flow : t -> Frontend.Ast.clss
val punch_in : t -> Time.t -> t
val punch_out : t -> Time.t -> t
val pkts_from_file : string -> t list
val write_to_csv : t list -> string -> unit
