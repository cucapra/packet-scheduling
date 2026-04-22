(** JSON encoding for the IR. The shape: a top-level JSON array of instruction
    records, each with an ["op"] discriminator and named fields. Policy tags are
    serialized as their string form (["FIFO"], ["UNION"], …). *)

let from_pol_ty (pt : Ir.pol_ty) : Yojson.Basic.t =
  `String (Ir.string_of_pol_ty pt)

let from_instr (i : Ir.instr) : Yojson.Basic.t =
  match i with
  | Spawn (v, pe) ->
      `Assoc [ ("op", `String "spawn"); ("v", `Int v); ("pe", `Int pe) ]
  | Adopt (s, parent, child) ->
      `Assoc
        [
          ("op", `String "adopt");
          ("step", `Int s);
          ("parent", `Int parent);
          ("child", `Int child);
        ]
  | Assoc (v, c) ->
      `Assoc
        [ ("op", `String "assoc"); ("v", `Int v); ("class", `String c) ]
  | Deassoc (v, c) ->
      `Assoc
        [ ("op", `String "deassoc"); ("v", `Int v); ("class", `String c) ]
  | Map (v, c, s) ->
      `Assoc
        [
          ("op", `String "map");
          ("v", `Int v);
          ("class", `String c);
          ("step", `Int s);
        ]
  | Change_pol (v, pt, n) ->
      `Assoc
        [
          ("op", `String "change_pol");
          ("v", `Int v);
          ("pol", from_pol_ty pt);
          ("n", `Int n);
        ]
  | Change_weight (v, s, w) ->
      `Assoc
        [
          ("op", `String "change_weight");
          ("v", `Int v);
          ("step", `Int s);
          ("weight", `Float w);
        ]

let from_program (p : Ir.program) : Yojson.Basic.t =
  `List (List.map from_instr p)
