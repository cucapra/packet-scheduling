type t

val create : int -> t
val clone : t -> t
val lookup : string -> t -> float
val lookup_opt : string -> t -> float option
val rebind : string -> float -> t -> t
val rebind_all : (string * float) list -> t -> t
val isdefined : string -> t -> bool
