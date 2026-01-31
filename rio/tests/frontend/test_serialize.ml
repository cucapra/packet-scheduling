open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"
let json_dir = "jsons/"

let prog_to_json file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program |> Json.from_policy

let prog_to_policy file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program

let make_test name file =
  let base = file |> Filename.basename |> Filename.remove_extension in
  let json = json_dir ^ Printf.sprintf "%s.json" base in
  name >:: fun _ ->
  assert_equal
    (Yojson.Basic.from_file json)
    (prog_to_json file) ~printer:Yojson.Basic.pretty_to_string
    ~cmp:Yojson.Basic.equal

let make_compare_test name file1 file2 expected_diff =
  name >:: fun _ ->
  let policy1 = prog_to_policy file1 in
  let policy2 = prog_to_policy file2 in
  let actual_diff = Rio_compare.Compare.analyze policy1 policy2 in
  assert_equal expected_diff actual_diff ~printer:(fun d ->
      Rio_compare.Compare.to_string d)

let serialize_tests =
  [
    make_test "single class policy" "work_conserving/drop_a_class.sched";
    make_test "fifo 1 class" "work_conserving/fifo_1_class_sugar.sched";
    make_test "fifo 2 class union" "work_conserving/fifo_2_class_union.sched";
    make_test "fifo n classes" "work_conserving/fifo_n_classes.sched";
    make_test "rr 1 class" "work_conserving/rr_1_class.sched";
    make_test "rr 2 classes" "work_conserving/rr_2_classes.sched";
    make_test "rr 2 classes merged" "work_conserving/rr_2_classes_merged.sched";
    make_test "rr hierarchy" "work_conserving/rr_hier_merge_sugar.sched";
    make_test "rr even hierarchy" "work_conserving/rr_hier.sched";
    make_test "rr wide hierarchy" "work_conserving/rr_wide_hier.sched";
    make_test "rr 3 classes" "work_conserving/rr_3_classes.sched";
    make_test "rr and strict" "work_conserving/rr_strict_hier.sched";
    make_test "strict n classes" "work_conserving/strict_3_classes.sched";
    make_test "complex tree" "work_conserving/complex_tree.sched";
  ]

let compare_tests_same =
  [
    (* Same program twice *)
    make_compare_test "same program twice"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes.sched" Rio_compare.Compare.Same;
    (* Merely jumbled, in RR *)
    make_compare_test "merely jumbled in RR"
      "work_conserving/rr_hier_merge_sugar.sched"
      "work_conserving/rr_hier_merge_sugar_jumbled.sched"
      Rio_compare.Compare.Same;
    (* Merely jumbled, in WFQ *)
    make_compare_test "merely jumbled in WFQ"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_jumbled.sched" Rio_compare.Compare.Same;
    (* Different variable names, same structure *)
    make_compare_test "different variable names, same structure"
      "work_conserving/rr_hier.sched"
      "work_conserving/rr_hier_weird_var_names.sched" Rio_compare.Compare.Same;
  ]

let compare_tests_different =
  [
    (* WFQ(A,B,C) vs WFQ(A,B,D)*)
    make_compare_test "different WFQ" "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff.sched"
      Rio_compare.Compare.VeryDifferent;
    (* RR(A,B) vs RR(A,B,C) *)
    make_compare_test "RR with arm added" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes.sched"
      (Rio_compare.Compare.ArmsAdded
         { policy_type = "RoundRobin"; old_count = 2; new_count = 3 });
    (* RR(A,B) vs RR(C,A,B) *)
    make_compare_test "RR with arm added" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_CAB.sched"
      (Rio_compare.Compare.ArmsAdded
         { policy_type = "RoundRobin"; old_count = 2; new_count = 3 });
    (* WFQ(A,B) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "work_conserving/wfq_2_classes.sched"
      "work_conserving/wfq_3_classes.sched"
      (Rio_compare.Compare.ArmsAdded
         { policy_type = "WeightedFair"; old_count = 2; new_count = 3 });
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_DEF.sched" Rio_compare.Compare.VeryDifferent;
    (* SP(C, B, A) vs SP(B, C, A) *)
    make_compare_test "Strict with arms reordered"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes_jumbled.sched"
      Rio_compare.Compare.VeryDifferent;
    (* WFQ -- same classes but weights changed *)
    make_compare_test "WFQ with weights changed"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff_weights.sched"
      (Rio_compare.Compare.WeightsChanged
         { old_weights = [ 1.0; 2.0; 3.0 ]; new_weights = [ 2.0; 2.0; 4.0 ] });
  ]

let compare_tests_deep =
  [
    (* RR and Strict hierarchy with arm added deep in the tree *)
    make_compare_test "RR and Strict hierarchy with arm added deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_add_arm.sched"
      (* (Rio_compare.Compare.ArmsAdded
         { policy_type = "Strict"; old_count = 2; new_count = 3 }); *)
      Rio_compare.Compare.VeryDifferent;
  ]

let suite =
  "serialization tests"
  >::: serialize_tests @ compare_tests_same @ compare_tests_different
       @ compare_tests_deep

let () = run_test_tt_main suite
