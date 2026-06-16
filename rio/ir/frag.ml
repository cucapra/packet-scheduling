open Instr

type t = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  set_policies : instr list;
  change_arities : instr list;
  change_weights : instr list;
  root_v : vpifo;
  classes : clss list;
}

let empty ~root_v ~classes =
  {
    spawns = [];
    adopts = [];
    assocs = [];
    maps = [];
    set_policies = [];
    change_arities = [];
    change_weights = [];
    root_v;
    classes;
  }

let to_commit (f : t) : commit =
  List.concat
    [
      f.spawns;
      f.adopts;
      f.assocs;
      f.maps;
      f.set_policies;
      f.change_arities;
      f.change_weights;
    ]

let combine (local : t) (children : t list) : t =
  let collect proj = proj local @ List.concat_map proj children in
  {
    spawns = collect (fun f -> f.spawns);
    adopts = collect (fun f -> f.adopts);
    assocs = collect (fun f -> f.assocs);
    maps = collect (fun f -> f.maps);
    set_policies = collect (fun f -> f.set_policies);
    change_arities = collect (fun f -> f.change_arities);
    change_weights = collect (fun f -> f.change_weights);
    root_v = local.root_v;
    classes = local.classes;
  }

let stub (prev_d : Decorated.t) : t =
  empty
    ~root_v:(Decorated.root_vpifo prev_d)
    ~classes:(Decorated.subtree_classes prev_d)
