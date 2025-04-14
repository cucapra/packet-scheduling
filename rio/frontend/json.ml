let rec policy_to_json (ast : Policy.t) =
  match ast with
  | Class c -> `Assoc [ ("class", `String c) ]
  | Fifo sl -> `Assoc [ ("fifo", `List (List.map policy_to_json sl)) ]
  | RoundRobin sl -> `Assoc [ ("rr", `List (List.map policy_to_json sl)) ]
  | Strict sl -> `Assoc [ ("strict", `List (List.map policy_to_json sl)) ]
  | _ -> failwith "not implemented"

let main (prog : string) =
  prog |> Parse.parse_file |> Policy.of_program |> policy_to_json
  |> Yojson.Basic.to_string
