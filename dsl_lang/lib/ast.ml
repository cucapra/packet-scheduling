type clss = string

type var = string

type policy =
| Class of clss
| Fifo of clss list
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Transient of policy list
(* | Classes of policy list *)

type statement = 
| DeclareClasses of clss list (*classes A, B, C *)
| Assn of var * policy
| Return of policy 

type program =
| Prog of statement * statement (* declareClasses; return *)