type clss = string
type var = string

type error_info = {
  row : int option;
  col : int option;
  char : char option;
}

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
  | WeightedFair of (stream * int) list
  (* Non-Work Conserving *)
  | RateControlled of stream list
  | LeakyBucket of stream list * int * int
  | TokenBucket of stream list * int * int
  | StopAndGo of stream list * int
  (* Variables *)
  | Var of var

type program = clss list * (var * stream) list * stream
