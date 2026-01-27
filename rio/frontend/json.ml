let rec from_policy (p : Policy.t) : Yojson.Basic.t =
  match p with
  | FIFO cs -> `Assoc [ ("FIFO", `List (List.map (fun c -> `String c) cs)) ]
  | EDF cs -> `Assoc [ ("EDF", `List (List.map (fun c -> `String c) cs)) ]
  | Strict ps -> `Assoc [ ("Strict", `List (List.map from_policy ps)) ]
  | RR ps -> `Assoc [ ("RR", `List (List.map from_policy ps)) ]
  | WFQ (ps, ws) ->
      let f p w = `List [ from_policy p; `Float w ] in
      `Assoc [ ("WFQ", `List (List.map2 f ps ws)) ]

(* Normalize JSON for comparison - sorts commutative operations *)
let rec normalize_json (json : Yojson.Basic.t) : Yojson.Basic.t =
  match json with
  | `Assoc pairs -> (
      let normalized_pairs =
        List.map (fun (k, v) -> (k, normalize_json v)) pairs
      in
      match normalized_pairs with
      | [ ("FIFO", `List items) ] ->
          (* Sort FIFO classes since order doesn't matter within a union *)
          let sorted = List.sort compare items in
          `Assoc [ ("FIFO", `List sorted) ]
      | [ ("EDF", `List items) ] ->
          (* Sort EDF classes since order doesn't matter *)
          let sorted = List.sort compare items in
          `Assoc [ ("EDF", `List sorted) ]
      | [ ("RR", `List items) ] ->
          (* Sort RR children since it's commutative *)
          let sorted =
            List.sort
              (fun a b -> compare (normalize_json a) (normalize_json b))
              items
          in
          `List sorted |> fun x -> `Assoc [ ("RR", x) ]
      | [ ("WFQ", `List pairs) ] ->
          (* Sort WFQ pairs by the serialized representation since it's commutative *)
          let sorted =
            List.sort
              (fun a b -> compare (normalize_json a) (normalize_json b))
              pairs
          in
          `Assoc [ ("WFQ", `List sorted) ]
      | _ -> `Assoc normalized_pairs)
  | `List items -> `List (List.map normalize_json items)
  | other -> other

(* Serialize and compare functionality *)
let compare_programs prog_dir (file1 : string) (file2 : string) : bool =
  let prog_to_json file =
    prog_dir ^ file |> Parse.parse_file |> Policy.of_program |> from_policy
    |> normalize_json
  in
  let json1 = prog_to_json file1 in
  let json2 = prog_to_json file2 in
  Yojson.Basic.equal json1 json2
