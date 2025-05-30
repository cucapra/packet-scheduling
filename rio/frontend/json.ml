let rec from_policy (p : Policy.t) : Yojson.Basic.t =
  match p with
  | FIFO cs -> `Assoc [ ("FIFO", `List (List.map (fun c -> `String c) cs)) ]
  | EDF cs -> `Assoc [ ("EDF", `List (List.map (fun c -> `String c) cs)) ]
  | Strict ps -> `Assoc [ ("Strict", `List (List.map from_policy ps)) ]
  | RR ps -> `Assoc [ ("RR", `List (List.map from_policy ps)) ]
  | WFQ (ps, ws) ->
      let f p w = `List [ from_policy p; `Float w ] in
      `Assoc [ ("WFQ", `List (List.map2 f ps ws)) ]
