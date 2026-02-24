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
    make_test "single class policy" "work_conserving/drop_a_class.sched"
      "fifo[A]";
    make_test "fifo 1 class" "work_conserving/fifo_1_class_sugar.sched"
      "fifo[A]";
    make_test "fifo 1 class" "work_conserving/fifo_1_class.sched" "fifo[A]";
    make_test "fifo of 3" "work_conserving/fifo_n_classes.sched"
      "fifo[union[A, B, C]]";
    make_test "rr of 1" "work_conserving/rr_1_class.sched" "rr[fifo[A]]";
    make_test "rr of 2" "work_conserving/rr_2_classes.sched"
      "rr[fifo[union[A, B]]]";
    make_test "multiple assignments" "work_conserving/rr_hier_merge_sugar.sched"
      "rr[fifo[union[BX, BY]], rr[fifo[RP], fifo[RT]]]";
    make_test "2 assignments w/ substitutions" "work_conserving/rr_hier.sched"
      "rr[fifo[B], rr[fifo[RP], fifo[RT]]]";
    make_test "3 classes with substitutions"
      "work_conserving/rr_n_class_hier.sched"
      "rr[fifo[A], fifo[B], rr[rr[fifo[CU], fifo[CV]], rr[fifo[CW], fifo[CX]]]]";
    make_test "rr of 3" "work_conserving/rr_n_classes.sched"
      "rr[fifo[A], fifo[B], fifo[C]]";
    make_test "rr and strict substitutions"
      "work_conserving/rr_strict_n_classes_hier.sched"
      "strict[fifo[A], fifo[B], rr[rr[fifo[CU], fifo[CV]], strict[fifo[CW], \
       fifo[CX]]]]";
    make_test "strict of 3" "work_conserving/strict_n_classes.sched"
      "strict[fifo[C], fifo[B], fifo[A]]";
  ]

let _nwc_tests =
  [
    make_test "leaky bucket of 2" "non_work_conserving/leaky_2_classes.sched"
      "leaky[[A, B], width = 5, buffer = 10]";
    make_test "token bucket of 2 round robins"
      "non_work_conserving/token_2_rr_children.sched"
      "token[[rr[A, B], rr[C, D]], width = 20, time = 50]";
    make_test "stop and go with 3 classes"
      "non_work_conserving/sg_3_classes.sched"
      "stopandgo[[stopandgo[[A, B], width = 10], stopandgo[[C], width = 10]], \
       width = 5]";
    make_test "rcsp for 4 classes" "non_work_conserving/rcsp_4_classes.sched"
      "rcsp[A, B, C, D]";
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

let suite = "parsing tests" >::: wc_tests @ error_tests (* @ nwc_tests *)
let () = run_test_tt_main suite
