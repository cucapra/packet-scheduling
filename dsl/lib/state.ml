type t = (string, float) Hashtbl.t

let create size = Hashtbl.create size

let lookup v t =
  try Hashtbl.find t v
  with Not_found -> failwith (Printf.sprintf "Uninitialized variable: %s" v)

let rebind k v t =
  Hashtbl.remove t k;
  Hashtbl.add t k v;
  t

let isdefined mem t = Hashtbl.mem t mem
