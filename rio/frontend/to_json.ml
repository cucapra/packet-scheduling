(* open Yojson.Basic *)
open Ast
(* for main program, first call parse "filename" to produce the AST
then convert it into a string (using policy.to_string?)
then convert that string into json *)

(* let rec to_string (ast : Ast.t) : string =
  match ast with
  | set *)

let rec set_to_json (ast : Ast.set) = 
  match ast with
  | Class c -> `Assoc [("class", `String c)]
  | Union sets -> `Assoc [("union", `List (List.map set_to_json sets))]


let rec stream_to_json (ast : Ast.stream) =
  match ast with
  | Fifo s -> `Assoc [("fifo", (set_to_json s))]
  | RoundRobin sl -> let lst = List.map stream_to_json sl in
    `Assoc [("rr", `List (lst))]
  | Strict sl -> `Assoc [("strict",  `List (List.map stream_to_json sl))]
  | _ -> failwith "not implemented"


let main (prog : string) =
  let parsed = Parse.parse_file prog in
  match parsed with 
  | (_, _, return) -> (*set_to_json cl *)
  stream_to_json return |> Yojson.Basic.to_string
  (* Convert and print the policy on the next line *)
  (* parsed |> Policy.of_program |> Policy.to_string |> print_endline *)

