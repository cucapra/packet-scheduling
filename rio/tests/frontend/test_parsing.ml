open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

let parse filename =
  prog_dir ^ filename |> Parser.parse_file |> Policy.of_program

let make_test name filename val_str =
  name >:: fun _ ->
  assert_equal val_str (filename |> parse |> Policy.to_string) ~printer:Fun.id

let make_error_test name filename exn =
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
  ]

let error_tests =
  [
    make_error_test "undeclared class" "incorrect/unbound_class.sched"
      (Policy.UndeclaredClass "Z");
    make_error_test "unbound var in middle of list of assignments"
      "incorrect/unbound_var.sched" (Policy.UnboundVariable "r_police");
    make_error_test "class used twice in policy"
      "incorrect/duplicate_classes.sched" (Policy.DuplicateClass "B");
    make_error_test "class used twice in one fifo"
      "incorrect/duplicate_samepol.sched" (Policy.DuplicateClass "A");
    make_error_test "fifo for multiple classes without union"
      "incorrect/fifo_multiple.sched"
      (Parser.ParserError { row = Some 4; col = Some 17; char = None });
  ]

let suite = "parsing tests" >::: wc_tests @ error_tests
let () = run_test_tt_main suite
