type clss = {name : string; used : bool}
type var = string

(* Changes to this type must also be reflected in `Policy.t` in policy.ml *)
type policy =
  | Class of clss
  | Fifo of policy list
  | RoundRobin of policy list
  | Strict of policy list
  | WeightedFair of (policy * int) list
  | EarliestDeadline of policy list
  | ShortestJobNext of policy list
  | ShortestRemaining of policy list
  | RateControlled of policy list
  | LeakyBucket of policy list * int * int
  | TokenBucket of policy list * int * int
  | StopAndGo of policy list * int
  | Var of var

type declare = clss list
type assignment = var * policy
type return = policy
type program = declare * assignment list * return
