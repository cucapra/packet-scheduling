type t = (string * int) list

exception UnboundKey of string

let empty = []
let rebind k v t = (k, v) :: t
let rebind_all lst t = List.fold_left (fun t (k, v) -> rebind k v t) t lst
let is_defined = List.mem_assoc
let lookup_opt = List.assoc_opt

let lookup k t =
  match lookup_opt k t with
  | Some v -> v
  | None -> raise (UnboundKey k)
