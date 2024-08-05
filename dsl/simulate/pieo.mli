type 'a t

val create : ('a -> 'a -> int) -> 'a t
val push : 'a t -> 'a -> 'a t
val top_exn : 'a t -> 'a
val pop : 'a t -> ('a -> bool) -> ('a * 'a t) option
val pop_exn : 'a t -> 'a * 'a t
val pop_if : 'a t -> ('a -> bool) -> ('a * 'a t) option
val length : 'a t -> int
val of_list : 'a list -> ('a -> 'a -> int) -> 'a t
val count : 'a t -> ('a -> bool) -> int
