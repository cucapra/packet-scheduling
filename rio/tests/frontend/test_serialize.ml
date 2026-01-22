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

let make_compare_test name file1 file2 expected =
  name >:: fun _ ->
  let result = Json.compare_programs prog_dir file1 file2 in
  assert_equal expected result ~printer:string_of_bool

let serialize_tests =
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

let compare_tests =
  [
    make_compare_test "same program twice" "work_conserving/fifo_1_class.sched"
      "work_conserving/fifo_1_class.sched" true;
    make_compare_test "different programs" "work_conserving/fifo_1_class.sched"
      "work_conserving/rr_1_class.sched" false;
    make_compare_test "rr 1 vs rr 2" "work_conserving/rr_1_class.sched"
      "work_conserving/rr_2_classes.sched" false;
  ]

let suite = "serialization tests" >::: serialize_tests @ compare_tests
let () = run_test_tt_main suite
