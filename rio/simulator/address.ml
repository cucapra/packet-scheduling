type t =
  | Eps
  | Ptr of int * t

let rec to_string = function
  | Eps -> "ε"
  | Ptr (i, t) -> Printf.sprintf "%d ∙ %s" i (to_string t)
