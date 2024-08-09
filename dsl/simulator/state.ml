type t = (string, float) Hashtbl.t

exception UnboundKey of string

let create size = Hashtbl.create size
let clone = Hashtbl.copy
let lookup_opt k t = Hashtbl.find_opt t k

let lookup k t =
  match lookup_opt k t with
  | Some v -> v
  | None -> raise (UnboundKey k)

let rebind k v t =
  Hashtbl.remove t k;
  Hashtbl.add t k v;
  t

let rebind_all lst t = List.fold_left (fun t (k, v) -> rebind k v t) t lst
let isdefined mem t = Hashtbl.mem t mem
