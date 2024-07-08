type clss = string

type var = string

type typ =
| Fifo
| Fair (*same as round robin*)
| Strict
| Transient

type policy =
| Class of clss
| Policy of typ * policy list

type exp = 
| Assn of var * policy
| Return of policy (*unsure*)
| Classes of clss list (* classes A, B, C *)