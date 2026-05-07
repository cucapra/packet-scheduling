open Instr

type t = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  change_pols : instr list;
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
    change_pols = [];
    change_weights = [];
    root_v;
    classes;
  }

let to_program (f : t) : program =
  List.concat
    [ f.spawns; f.adopts; f.assocs; f.maps; f.change_pols; f.change_weights ]

let combine (local : t) (children : t list) : t =
  let collect proj = proj local @ List.concat_map proj children in
  {
    spawns = collect (fun f -> f.spawns);
    adopts = collect (fun f -> f.adopts);
    assocs = collect (fun f -> f.assocs);
    maps = collect (fun f -> f.maps);
    change_pols = collect (fun f -> f.change_pols);
    change_weights = collect (fun f -> f.change_weights);
    root_v = local.root_v;
    classes = local.classes;
  }

let stub (prev_d : Decorated.t) : t =
  empty
    ~root_v:(Decorated.root_vpifo prev_d)
    ~classes:(Decorated.subtree_classes prev_d)
