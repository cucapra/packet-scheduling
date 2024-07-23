open Dsl_core
open OUnit2

(* The top-level entry point for running our interpreter. *)
(* Main function. *)
(* let () =
  let _ =
    if Array.length Sys.argv <> 2 then
      (Printf.printf "Usage: run interpreter <file>\n";
       exit 0) in
  let filename = Sys.argv.(1) in
  let res = 
  Util.parse_file filename in ignore (Eval.eval res)    *)

let eval_prog (filename : string) =
  let res = Util.parse_file filename in 
  (Eval.eval res)  

let make_test (name : string) (filename : string) (val_str : string) =
  name >:: fun _ ->
  assert_equal val_str
    (Util.string_of_policy (eval_prog filename))
    ~printer:(fun x -> x)

let tests = 
  [
    make_test "single class policy" "../../dsl/progs/now/drop_a_class.sched" "A";   
    make_test "fifo sugar 1 class" "../../dsl/progs/now/fifo_1_class_sugar.sched" "A";
    make_test "fifo 1 class" "../../dsl/progs/now/fifo_1_class.sched" "A";
    make_test "fifo of 3" "../../dsl/progs/now/fifo_n_classes.sched" "fifo[ A, B, C, ]";
    make_test "rr of 1" "../../dsl/progs/now/rr_1_class.sched" "rr[ A ]";

  ]

let suite = "suite" >::: tests
let () = run_test_tt_main suite
