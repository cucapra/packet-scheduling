let rec from_policy (p : Policy.t) : Yojson.Basic.t =
  match p with
  | FIFO cs -> `Assoc [ ("FIFO", `List (List.map (fun c -> `String c) cs)) ]
  | EDF cs -> `Assoc [ ("EDF", `List (List.map (fun c -> `String c) cs)) ]
  | Strict ps -> `Assoc [ ("Strict", `List (List.map from_policy ps)) ]
  | RR ps -> `Assoc [ ("RR", `List (List.map from_policy ps)) ]
  | WFQ (ps, ws) ->
      let f p w = `List [ from_policy p; `Float w ] in
      `Assoc [ ("WFQ", `List (List.map2 f ps ws)) ]

(* Serialize and compare functionality *)
let prog_to_jsonaf_with_dir prog_dir file =
  (* Convert a scheduling program file to a Jsonaf representation *)
  prog_dir ^ file |> Parse.parse_file |> Policy.of_program |> from_policy
  |> Yojson.Basic.to_string |> Jsonaf.of_string

let compare_programs prog_dir (file1 : string) (file2 : string) : bool =
  let json1 = prog_to_jsonaf_with_dir prog_dir file1 in
  let json2 = prog_to_jsonaf_with_dir prog_dir file2 in
  Jsonaf.exactly_equal json1 json2
(* TODO: move away from exactly_equal *)
