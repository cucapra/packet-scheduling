type clss = string

(* type var = string *)

type policy =
| Class of clss
| Fifo of policy list (* should change back to clss list? *)
| Fair of policy list (*same as round robin*)
| Strict of policy list
| Transient of policy list

(* type statement =
| Assn of var * policy *)
