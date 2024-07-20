open Dsl_core

(* The top-level entry point for running our interpreter. *)
(* Main function. *)
let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: run interpreter <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let res = 
  Util.parse_file filename in ignore (Eval.eval res)
