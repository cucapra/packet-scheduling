type clss = string
type var = string

(* Changes to this type must also be reflected in `Policy.t` in policy.ml *)
type set =
  | Class of clss
  | Union of set list

type stream =
  (* Set-to-Stream *)
  | Fifo of set list
  | EarliestDeadline of set list
  | ShortestJobNext of set list
  | ShortestRemaining of set list
  (* Stream-To-Stream *)
  | RoundRobin of stream list
  | Strict of stream list
  | WeightedFair of (stream * float) list
  (* Non-Work Conserving *)
  | RateControlled of stream list
  | LeakyBucket of stream list * int * int
  | TokenBucket of stream list * int * int
  | StopAndGo of stream list * int
  (* Variables *)
  | Var of var

type declare = clss list
type assignment = var * stream
type return = stream
type program = declare * assignment list * return
