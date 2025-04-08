type 'a t =
  | Star
  | CStar of 'a (* for Rio trees *)
  | Node of 'a t list

type addr = int list

val addr_to_string : addr -> string
val of_policy : Frontend.Policy.t -> 'a t
