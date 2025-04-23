open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"
let json_dir = "jsons/"

let prog_to_json file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program |> Json.from_policy

let make_test name file =
  let base = file |> Filename.basename |> Filename.remove_extension in
  let json = json_dir ^ Printf.sprintf "%s.json" base in

  name >:: fun _ ->
  assert_equal
    (Yojson.Basic.from_file json)
    (prog_to_json file) ~printer:Yojson.Basic.pretty_to_string
    ~cmp:Yojson.Basic.equal

let make_error_test name file exn =
  name >:: fun _ -> assert_raises exn (fun () -> prog_to_json file)

let tests =
  [
    make_test "single class policy" "work_conserving/drop_a_class.sched";
    make_test "fifo 1 class" "work_conserving/fifo_1_class_sugar.sched";
    make_test "fifo 2 class union" "work_conserving/fifo_2_class_union.sched";
    make_test "fifo n classes" "work_conserving/fifo_n_classes.sched";
    make_test "rr 1 class" "work_conserving/rr_1_class.sched";
    make_test "rr 2 classes" "work_conserving/rr_2_classes.sched";
    make_test "rr hierarchy" "work_conserving/rr_hier_merge_sugar.sched";
    make_test "rr even hierarchy" "work_conserving/rr_hier.sched";
    make_test "rr n classes hierarchy" "work_conserving/rr_n_class_hier.sched";
    make_test "rr n classes" "work_conserving/rr_n_classes.sched";
    make_test "rr and strict" "work_conserving/rr_strict_n_classes_hier.sched";
    make_test "strict n classes" "work_conserving/strict_n_classes.sched";
    make_test "complex tree" "work_conserving/complex_tree.sched";
  ]

let error_tests =
  [
    make_error_test "undeclared class" "incorrect/undeclared_classes.sched"
      (Policy.UndeclaredClass "Z");
    make_error_test "unbound variable" "incorrect/unbound_var.sched"
      (Policy.UnboundVariable "policy");
    make_error_test "unbound var in middle of list of assignments"
      "incorrect/unbound_var_hier.sched" (Policy.UnboundVariable "r_polic");
    make_error_test "class used twice in policy"
      "incorrect/duplicate_classes.sched" (Policy.DuplicateClass "B");
    make_error_test "class used twice in one fifo"
      "incorrect/duplicate_samepol.sched" (Policy.DuplicateClass "A");
    make_error_test "fifo for multiple classes without union"
      "incorrect/set_multiple.sched"
      (Parser.ParserError "Syntax error at line 4, character 17");
    make_error_test "rr for classes without fifo'ing first"
      "incorrect/set_hierarchical.sched"
      (Parser.ParserError "Syntax error at line 3, character 14");
  ]

let suite = "serialization tests" >::: tests @ error_tests
let () = run_test_tt_main suite
