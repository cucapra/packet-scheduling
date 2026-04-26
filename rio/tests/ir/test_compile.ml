open Frontend
open OUnit2
open Ir

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> [Ir.program]. Stops short of pretty-printing — that's
   [test_pretty]'s job. We project [Ir.of_policy]'s [compiled] result down to
   its [.prog] field. *)
let compile filename =
  let c =
    prog_dir ^ filename |> Parser.parse_file |> Policy.of_program
    |> Ir.of_policy
  in
  c.prog

let make_test name filename (expected : program) =
  name >:: fun _ ->
  assert_equal ~printer:Ir.string_of_program expected (compile filename)

(* Single FIFO leaf [A] living on PE0 as v[1] (path [] → sentinel 1). *)
let fifo_a_expected : program = [ Spawn (1, 0); Assoc (1, "A") ]

(* SP root at path [] → v1; children at paths [0],[1],[2] → v10,v11,v12;
   adopt steps at ([],0..2) → 20,21,22. *)
let strict_abc_expected : program =
  [
    Spawn (1, 0);
    Spawn (10, 1);
    Spawn (11, 1);
    Spawn (12, 1);
    Adopt (20, 1, 10);
    Adopt (21, 1, 11);
    Adopt (22, 1, 12);
    Assoc (1, "A");
    Assoc (1, "B");
    Assoc (1, "C");
    Assoc (10, "A");
    Assoc (11, "B");
    Assoc (12, "C");
    Map (1, "A", 20);
    Map (1, "B", 21);
    Map (1, "C", 22);
    Change_pol (1, SP, 3);
    (* A is highest priority, then B, then C *)
    Change_weight (1, 20, 1.0);
    Change_weight (1, 21, 2.0);
    Change_weight (1, 22, 3.0);
  ]

let compile_tests =
  [
    make_test "fifo[A]" "work_conserving/fifo_A.sched" fifo_a_expected;
    make_test "drop unused class" "work_conserving/drop_class.sched"
      fifo_a_expected;
    make_test "strict[A, B, C]" "work_conserving/strict_ABC.sched"
      strict_abc_expected;
  ]

(* The remaining nested programs (complex_tree.sched, rr_strict_hier.sched)
   now compile too — adding goldens for them is a follow-up. *)
let suite = "compile tests" >::: compile_tests
let () = run_test_tt_main suite
