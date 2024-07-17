type clss = string
type var = string

type declare =
| DeclareClasses of clss list

type policy =
| Class of clss
| Fifo of policy list 
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Var of var

type assignment = 
| Assn of var * policy

type return =
| Return of policy

type statement = 
| Declare of declare
| Assignment of assignment
| Seq of statement * statement
| Ret of return


type program =
| Prog of declare * (statement list) * return