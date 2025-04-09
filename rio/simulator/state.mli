type t

exception UnboundKey of string

val empty : t
val rebind : string -> int -> t -> t
val rebind_all : (string * int) list -> t -> t
val is_defined : string -> t -> bool
val lookup : string -> t -> int
val lookup_opt : string -> t -> int option
