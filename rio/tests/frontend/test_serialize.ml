open Frontend
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"
let json_dir = "jsons/"

let prog_to_json file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program |> Policy.to_json

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

let node ?(index = None) policy_type change =
  Rio_compare.Compare.NodeChange { policy_type; index; change }

let sub_node ?(index = None) policy_type change =
  Rio_compare.Compare.SubChange (node ~index policy_type change)

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
  let open Rio_compare.Compare in
  [
    make_compare_test "same program twice"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes.sched" Same;
    make_compare_test "merely jumbled in RR"
      "work_conserving/rr_hier_merge_sugar.sched"
      "work_conserving/rr_hier_merge_sugar_jumbled.sched" Same;
    make_compare_test "merely jumbled in WFQ"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_jumbled.sched" Same;
    make_compare_test "different variable names, same structure"
      "work_conserving/rr_hier.sched"
      "work_conserving/rr_hier_weird_var_names.sched" Same;
  ]

let compare_tests_different =
  let open Rio_compare.Compare in
  [
    (* SP(B,A) vs SP(C,B,A) *)
    make_compare_test "strict arm added"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes.sched"
      (node "SP"
         (ArmsAdded
            { old_count = 2; new_count = 3; details = "added fifo[C] at 1" }));
    (* TODO: is this index okay? *)
    (* SP(B,A) vs SP(B,C,A) *)
    make_compare_test "strict arm added in the middle"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes_BCA.sched"
      (node "SP"
         (ArmsAdded
            { old_count = 2; new_count = 3; details = "added fifo[C] at 2" }));
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes_ABC.sched" (node "SP" VeryDifferent);
    (* WFQ(A,B,C) vs WFQ(A,B,D) *)
    make_compare_test "different WFQ" "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff.sched"
      (node ~index:(Some [ 2 ]) "WFQ" (sub_node "FIFO" VeryDifferent));
    (* RR(A,B) vs RR(A,B,C) *)
    make_compare_test "RR with arm added" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes.sched"
      (node "RR"
         (ArmsAdded
            { old_count = 2; new_count = 3; details = "added fifo[C] at 2" }));
    (* RR(A,B) vs RR(C,A,B) *)
    make_compare_test "RR with arm added (CAB)"
      "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_CAB.sched"
      (node "RR"
         (ArmsAdded
            { old_count = 2; new_count = 3; details = "added fifo[C] at 2" }));
    (* WFQ(A,B) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "work_conserving/wfq_2_classes.sched"
      "work_conserving/wfq_3_classes.sched"
      (node "WFQ"
         (ArmsAdded
            {
              old_count = 2;
              new_count = 3;
              details = "added fifo[C] at 2 with weight 3";
            }));
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_DEF.sched" (node "RR" VeryDifferent);
    (* SP(C,B,A) vs SP(B,C,A) *)
    make_compare_test "Strict with arms reordered"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes_jumbled.sched"
      (node ~index:(Some [ 0 ]) "SP" (sub_node "FIFO" VeryDifferent));
    (* WFQ weights changed *)
    make_compare_test "WFQ with weights changed"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff_weights.sched"
      (node "WFQ" VeryDifferent);
    make_compare_test "WFQ with weights changed and arm added"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_very_diff.sched" (node "WFQ" VeryDifferent);
    (* Test arm removal *)
    make_compare_test "RR with arm removed" "work_conserving/rr_3_classes.sched"
      "work_conserving/rr_2_classes.sched"
      (node "RR" (ArmsRemoved { old_count = 3; new_count = 2 }));
    make_compare_test "WFQ with arm removed"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_2_classes.sched"
      (node "WFQ" (ArmsRemoved { old_count = 3; new_count = 2 }));
  ]

let compare_tests_superpol =
  let open Rio_compare.Compare in
  [
    make_compare_test "B is sub-pol of large tree"
      "work_conserving/FIFO_B.sched" "work_conserving/rr_hier_superpol.sched"
      (node ~index:(Some [ 1; 0 ]) "RR" SuperPol);
    make_compare_test "sub-policy of rr_hier"
      "work_conserving/rr_hier_subpol.sched" "work_conserving/rr_hier.sched"
      (node ~index:(Some [ 1 ]) "RR" SuperPol);
    make_compare_test "rr_hier is subpol of large tree"
      "work_conserving/rr_hier_subpol.sched"
      "work_conserving/rr_hier_superpol.sched"
      (node ~index:(Some [ 1; 1 ]) "RR" SuperPol);
  ]

let compare_tests_deep =
  let open Rio_compare.Compare in
  [
    make_compare_test "RR/Strict hierarchy with arm added deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_add_arm.sched"
      (node ~index:(Some [ 2 ]) "SP"
         (sub_node ~index:(Some [ 0 ]) "RR"
            (sub_node "SP"
               (ArmsAdded
                  {
                    old_count = 2;
                    new_count = 3;
                    details = "added fifo[CY] at 3";
                  }))));
    make_compare_test "RR/Strict hierarchy with RR swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_1.sched" Same;
    make_compare_test "RR/Strict hierarchy with SP swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_2.sched"
      (node ~index:(Some [ 2 ]) "SP"
         (sub_node ~index:(Some [ 0 ]) "RR"
            (sub_node ~index:(Some [ 0 ]) "SP" (sub_node "FIFO" VeryDifferent))));
    make_compare_test "RR/Strict hierarchy with arm removed deep"
      "work_conserving/rr_strict_hier_add_arm.sched"
      "work_conserving/rr_strict_hier.sched"
      (node ~index:(Some [ 2 ]) "SP"
         (sub_node ~index:(Some [ 0 ]) "RR"
            (sub_node "SP" (ArmsRemoved { old_count = 3; new_count = 2 }))));
  ]

let suite =
  "serialization tests"
  >::: serialize_tests @ compare_tests_same @ compare_tests_different
       @ compare_tests_superpol @ compare_tests_deep

let () = run_test_tt_main suite
