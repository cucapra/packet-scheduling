(** JSON exporter for IR programs. *)

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
  | Instr.Change_pol (v, pt, n) ->
      `Assoc
        [
          ("op", `String "change_pol");
          ("v", `Int v);
          ("pol", from_pol_ty pt);
          ("n", `Int n);
        ]
  | Instr.Change_weight (v, s, w) ->
      `Assoc
        [
          ("op", `String "change_weight");
          ("v", `Int v);
          ("step", `Int s);
          ("weight", `Float w);
        ]

let from_program (p : Instr.program) : Yojson.Basic.t =
  `List (List.map from_instr p)
