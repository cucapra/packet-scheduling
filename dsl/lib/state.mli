type t

val create : int -> t
val lookup : string -> t -> float
val rebind : string -> float -> t -> t
val isdefined : string -> t -> bool
