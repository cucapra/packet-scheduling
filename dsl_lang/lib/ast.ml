type clss = string
type var = string

type policy =
| Class of clss
| Fifo of policy list
| Fair of policy list
| Strict of policy list
| Var of var

type declare = clss list

type assignment = var * policy

type return = policy

type program = declare * (assignment list) * return