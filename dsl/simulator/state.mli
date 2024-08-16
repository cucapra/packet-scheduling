type t

exception UnboundKey of string

val empty : t
val rebind : string -> float -> t -> t
val rebind_all : (string * float) list -> t -> t
val is_defined : string -> t -> bool
val lookup : string -> t -> float
val lookup_opt : string -> t -> float option
