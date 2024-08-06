type t = Star | Node of t list

let rec of_policy (p : Frontend.Policy.t) =
  match p with
  | Class _ -> Star
  | Fifo plst | RoundRobin plst | Strict plst -> Node (List.map of_policy plst)
  | WeightedFair wplst -> Node (List.map (fun (p, _) -> of_policy p) wplst)

let rec size = function
  | Star -> 1
  | Node ts -> List.fold_left (fun acc x -> acc + size x) 0 ts
