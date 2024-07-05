type clss = Id

type var = string

type policy =
| Class of clss
| Fifo of policy list
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Transient of policy list

type exp = 
| Assn of var * policy
| Return of var (*unsure*)

