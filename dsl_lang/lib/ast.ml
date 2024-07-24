type clss = string
type var = string

type declare =
| DeclareClasses of clss list

type policy =
| Class of clss
| Fifo of policy list
| Fair of policy list
| Strict of policy list
| Var of var

type return =
| Return of policy

type assignment =
| Assn of var * policy

type internalcomp =
| DeclareComp of clss list
| AssnComp of var * policy
| RtnComp of policy

type seq = internalcomp list
type progseq = seq * return

type program =
| Prog of declare * (assignment list) * return