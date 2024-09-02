type 'a t

val create : ('a -> 'a -> int) -> 'a t
val of_list : 'a list -> ('a -> 'a -> int) -> 'a t
val size : 'a t -> ('a -> bool) -> int
val push : 'a t -> 'a -> 'a t
val pop : 'a t -> ('a -> bool) -> ('a * 'a t) option
