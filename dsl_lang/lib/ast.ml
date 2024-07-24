type clss = string
type var = string

(* Policies *)

type policy =
| Class of clss
| Fifo of policy list
| Fair of policy list
| Strict of policy list
| Var of var

(* Program components *)

type internalcomp =
| DeclareComp of clss list
| AssnComp of var * policy
| RtnComp of policy

type declare = clss list

type assignment = var * policy

type return = policy

(* Types of instruction sequences *)

type seq = internalcomp list

type progseq = seq * return

type program =
| Prog of declare * (assignment list) * return