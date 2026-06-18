(** JSON exporter for IR commits. *)

let from_pol_ty (pt : Instr.pol_ty) : Yojson.Basic.t =
  `String (Instr.string_of_pol_ty pt)

let from_instr (i : Instr.instr) : Yojson.Basic.t =
  match i with
  | Instr.Spawn (v, pe) ->
      `Assoc [ ("op", `String "spawn"); ("v", `Int v); ("pe", `Int pe) ]
  | Instr.Adopt (s, parent, child) ->
      `Assoc
        [
          ("op", `String "adopt");
          ("step", `Int s);
          ("parent", `Int parent);
          ("child", `Int child);
        ]
  | Instr.Emancipate (s, parent, child) ->
      `Assoc
        [
          ("op", `String "emancipate");
          ("step", `Int s);
          ("parent", `Int parent);
          ("child", `Int child);
        ]
  | Instr.Assoc (v, c) ->
      `Assoc [ ("op", `String "assoc"); ("v", `Int v); ("class", `String c) ]
  | Instr.Deassoc (v, c) ->
      `Assoc [ ("op", `String "deassoc"); ("v", `Int v); ("class", `String c) ]
  | Instr.Map (v, c, s) ->
      `Assoc
        [
          ("op", `String "map");
          ("v", `Int v);
          ("class", `String c);
          ("step", `Int s);
        ]
  | Instr.Unmap (v, c, s) ->
      `Assoc
        [
          ("op", `String "unmap");
          ("v", `Int v);
          ("class", `String c);
          ("step", `Int s);
        ]
  | Instr.Set_policy (v, pt, n) ->
      `Assoc
        [
          ("op", `String "set_policy");
          ("v", `Int v);
          ("pol", from_pol_ty pt);
          ("n", `Int n);
        ]
  | Instr.Change_arity (v, n) ->
      `Assoc [ ("op", `String "change_arity"); ("v", `Int v); ("n", `Int n) ]
  | Instr.Set_arm_meta (v, s, w) ->
      `Assoc
        [
          ("op", `String "set_arm_meta");
          ("v", `Int v);
          ("step", `Int s);
          ("meta", `Float w);
        ]
  | Instr.GC v -> `Assoc [ ("op", `String "gc"); ("v", `Int v) ]
  | Instr.Designate (v, survivor) ->
      `Assoc
        [
          ("op", `String "designate"); ("v", `Int v); ("survivor", `Int survivor);
        ]
  | Instr.Undesignate v ->
      `Assoc [ ("op", `String "undesignate"); ("v", `Int v) ]

let from_commit (p : Instr.commit) : Yojson.Basic.t =
  `List [ `List (List.map from_instr p) ]
