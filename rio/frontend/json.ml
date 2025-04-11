(* open Yojson.Basic *)

let rec policy_to_json (ast : Policy.t) =
  match ast with
  | Class c -> `Assoc [ ("class", `String c) ]
  | Fifo sl -> `Assoc [ ("fifo", `List (List.map policy_to_json sl)) ]
  | RoundRobin sl ->
      let lst = List.map policy_to_json sl in
      `Assoc [ ("rr", `List lst) ]
  | Strict sl -> `Assoc [ ("strict", `List (List.map policy_to_json sl)) ]
  | _ -> failwith "not implemented"

let main (prog : string) =
  let parsed = Parse.parse_file prog |> Policy.of_program in
  policy_to_json parsed |> Yojson.Basic.to_string
