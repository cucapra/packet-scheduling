type t =
  | Class of Ast.clss
  | Fifo of t list
  | RoundRobin of t list
  | Strict of t list
  | WeightedFair of (t * int) list
  | EarliestDeadline of t list
  | ShortestJobNext of t list
  | ShortestRemaining of t list
  | RateControlled of t list
  | LeakyBucket of t list * int * int
  | TokenBucket of t list * int * int
  | StopAndGo of t list * int

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss

val from_program : Ast.program -> t
val to_string : t -> string
