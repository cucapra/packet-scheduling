type clss = string

type var = string

type policy =
| Class of clss
| Fifo of policy list
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Transient of policy list
(* | Classes of policy list *)

type statement = 
| Assn of var * policy
| Return of policy 
| DeclareClasses of clss list (*classes A, B, C *)