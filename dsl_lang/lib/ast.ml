type clss = string

type var = string

type policy =
| Class of clss
| Fifo of policy list (* should change back to clss list? *)
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Assn of var * policy
| Return of policy
| Seq of policy * policy

type declare =
| DeclareClasses of clss list
