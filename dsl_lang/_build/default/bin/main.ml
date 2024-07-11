open Dsl_core

(* main function *)
let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "Usage: dune exec bin/main.exe <file>";
    exit 1)
  else ();
  Sys.argv.(1) |> Util.parse_file |> print_endline