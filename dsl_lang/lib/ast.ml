type clss = string
type var = string

type declare =
| DeclareClasses of clss list

type policy =
| Class of clss
| Fifo of policy list
| RoundRobin of policy list
| Strict of policy list
| WeightedFair of (policy * int) list
| EarliestDeadline of policy list 
| ShortestJobNext of policy list
| ShortestRemaining of policy list
| RCSP of policy list
| LeakyBucket of (policy * int * int) list
| TokenBucket of (policy * int * int) list
| StopAndGo of policy list
| Var of var

type return =
| Return of policy

type assignment =
| Assn of var * policy

type program =
| Prog of declare * (assignment list) * return