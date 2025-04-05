type t =
  | Star
  | CStar of Frontend.Ast.clss (* for Rio trees *)
  | Node of t list

type addr = int list

let rec addr_to_string = function
  | [] -> "ε"
  | h :: t -> Printf.sprintf "%d ∙ %s" h (addr_to_string t)

let rec of_policy = function
  | Frontend.Policy.Node (_, ps) -> Node (List.map of_policy ps)
  | Frontend.Policy.Leaf (_, _) -> Star
