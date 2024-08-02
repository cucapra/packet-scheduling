type t

val cmp : t -> t -> int
val to_float : t -> float
val of_float : float -> t
val of_ints : int32 -> int32 -> t
val add_float : t -> float -> t
val epoch : t
val terminus : t
