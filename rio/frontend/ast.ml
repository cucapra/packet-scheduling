type clss = string
type var = string

(* Changes to this type must also be reflected in `Policy.t` in policy.ml *)
type set =
  | Class of clss
  | Union of set list

type stream =
  (* Set-to-Stream *)
  | Fifo of set
  | EarliestDeadline of set
  | ShortestJobNext of set
  | ShortestRemaining of set
  (* Stream-To-Stream *)
  | RoundRobin of stream list
  | Strict of stream list
  | WeightedFair of stream list * int list
  (* Non-Work Conserving *)
  | RateControlled of stream list
  | LeakyBucket of stream list * int * int
  | TokenBucket of stream list * int * int
  | StopAndGo of stream list * int
  (* Variables *)
  | Var of var

type policy = stream
type declare = clss list
type assignment = var * policy
type return = policy
type program = declare * assignment list * return
