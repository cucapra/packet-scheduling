type t = Star | Node of t list

let rec of_policy p =
  let open Policy in
  match p with
  | Class _ -> Star
  | Fifo plst
  | RoundRobin plst
  | Strict plst
  | EarliestDeadline plst
  | ShortestJobNext plst ->
      Node (List.map of_policy plst)
  | WeightedFair wplst -> Node (List.map (fun (p, _) -> of_policy p) wplst)

let rec size = function
  | Star -> 1
  | Node ts -> List.fold_left (fun acc x -> acc + size x) 0 ts

(* A few topologies to play with. *)
let one_level_quaternary = Node [ Star; Star; Star; Star ]
let one_level_ternary = Node [ Star; Star; Star ]
let one_level_binary = Node [ Star; Star ]
let two_level_binary = Node [ Node [ Star; Star ]; Star ]
let two_level_ternary = Node [ Star; Star; Node [ Star; Star; Star ] ]

let three_level_ternary =
  Node [ Star; Star; Node [ Star; Star; Node [ Star; Star; Star ] ] ]

let irregular = Node [ Star; Star; Star; Node [ Star; Star; Star ] ]
let flat_four = Node [ Star; Star; Star; Star ]
