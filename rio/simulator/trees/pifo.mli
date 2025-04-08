type 'a t

val create : unit -> 'a t
val size : 'a t -> int
val push : 'a t -> 'a -> Rank.t -> 'a t
val pop : 'a t -> ('a * 'a t) option
