open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

let parse filename =
  prog_dir ^ filename |> Parser.parse_file |> Policy.of_program

let make_test name filename val_str =
  name >:: fun _ ->
  assert_equal val_str (filename |> parse |> Policy.to_string) ~printer:Fun.id

let make_fail name exn =
  let filename = "incorrect/" ^ name ^ ".sched" in
  name >:: fun _ -> assert_raises exn (fun () -> parse filename)

let wc_tests =
  [
    make_test "single class policy" "work_conserving/drop_class.sched" "fifo[A]";
    make_test "fifo 1 class" "work_conserving/fifo_A.sched" "fifo[A]";
    make_test "union of 3" "work_conserving/union_ABC.sched"
      "fifo[union[A, B, C]]";
    make_test "multiple assignments" "work_conserving/rr_union_hier.sched"
      "rr[fifo[union[BX, BY]], rr[fifo[RP], fifo[RT]]]";
    make_test "rr of 3" "work_conserving/rr_ABC.sched"
      "rr[fifo[A], fifo[B], fifo[C]]";
    make_test "rr and strict substitutions"
      "work_conserving/rr_strict_hier.sched"
      "strict[fifo[A], fifo[B], rr[strict[fifo[CW], fifo[CX]], rr[fifo[CU], \
       fifo[CV]]]]";
    make_test "strict of 3" "work_conserving/strict_ABC.sched"
      "strict[fifo[A], fifo[B], fifo[C]]";
    make_test "new" "work_conserving/fifo_multiple.sched" "fifo[union[A, B]]";
  ]

let error_tests =
  [
    make_fail "unbound_class" (Policy.UndeclaredClass "Z");
    make_fail "unbound_var" (Policy.UnboundVariable "r_police");
    make_fail "duplicate_classes" (Policy.DuplicateClass "B");
    make_fail "duplicate_samepol" (Policy.DuplicateClass "A");
  ]

let suite = "parsing tests" >::: wc_tests @ error_tests
let () = run_test_tt_main suite
