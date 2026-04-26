(** JSON exporter for IR programs. *)

let from_pol_ty (pt : Instr.pol_ty) : Yojson.Basic.t =
  `String (Instr.string_of_pol_ty pt)

(* Every instruction serializes as a JSON object whose first field is
   ["op"]. Centralizing that here keeps the schema shape uniform — no arm
   can accidentally misspell the field, drop it, or move it. *)
let op name fields : Yojson.Basic.t = `Assoc (("op", `String name) :: fields)

let from_instr (i : Instr.instr) : Yojson.Basic.t =
  match i with
  | Instr.Spawn (v, pe) -> op "spawn" [ ("v", `Int v); ("pe", `Int pe) ]
  | Instr.Adopt (s, parent, child) ->
      op "adopt"
        [ ("step", `Int s); ("parent", `Int parent); ("child", `Int child) ]
  | Instr.Assoc (v, c) -> op "assoc" [ ("v", `Int v); ("class", `String c) ]
  | Instr.Deassoc (v, c) -> op "deassoc" [ ("v", `Int v); ("class", `String c) ]
  | Instr.Map (v, c, s) ->
      op "map" [ ("v", `Int v); ("class", `String c); ("step", `Int s) ]
  | Instr.Change_pol (v, pt, n) ->
      op "change_pol" [ ("v", `Int v); ("pol", from_pol_ty pt); ("n", `Int n) ]
  | Instr.Change_weight (v, s, w) ->
      op "change_weight"
        [ ("v", `Int v); ("step", `Int s); ("weight", `Float w) ]

let from_program (p : Instr.program) : Yojson.Basic.t =
  `List (List.map from_instr p)
