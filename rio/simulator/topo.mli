type t =
  | Star
  | CStar of Frontend.Ast.clss (* for Rio trees *)
  | Node of t list

type addr = int list

val addr_to_string : addr -> string
val of_policy : Frontend.Policy.t -> t
