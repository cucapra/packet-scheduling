open Frontend
open OUnit2
open Rio_compare.Compare

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

let prog_to_policy file =
  prog_dir ^ file |> Parser.parse_file |> Policy.of_program

let make_compare_test name file1 file2 expected_diff =
  name >:: fun _ ->
  let policy1 = prog_to_policy file1 in
  let policy2 = prog_to_policy file2 in
  let actual_diff = analyze policy1 policy2 in
  assert_equal expected_diff actual_diff ~printer:(fun d ->
      Rio_compare.Compare.to_string d)

let same =
  [
    make_compare_test "same program twice"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes.sched" Same;
    make_compare_test "merely jumbled in RR"
      "work_conserving/rr_hier_merge.sched"
      "work_conserving/rr_hier_merge_jumbled.sched" Same;
    make_compare_test "merely jumbled in WFQ"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_jumbled.sched" Same;
    make_compare_test "different variable names, same structure"
      "work_conserving/rr_hier.sched"
      "work_conserving/rr_hier_weird_var_names.sched" Same;
  ]

let armsadded =
  [
    (* SP(B,A) vs SP(C,B,A) *)
    make_compare_test "strict arm added"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 1" } ));
    (* TODO: is this index okay? *)
    (* SP(B,A) vs SP(B,C,A) *)
    make_compare_test "strict arm added in the middle"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes_BCA.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* SP(B,A) vs SP(B,C,A) *)
    make_compare_test "strict arm added in the middle"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes_BCA.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* RR(A,B) vs RR(A,B,C) *)
    make_compare_test "RR with arm added" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* RR(A,B) vs RR(C,A,B) *)
    make_compare_test "RR with arm added (CAB)"
      "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_CAB.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* WFQ(A,B) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "work_conserving/wfq_2_classes.sched"
      "work_conserving/wfq_3_classes.sched"
      (Change
         ( [],
           ArmsAdded
             {
               old_count = 2;
               new_count = 3;
               details = "added fifo[C] at 2 with weight 3";
             } ));
  ]

let armsremoved =
  (* In reality we will just give up. *)
  [
    (* Test arm removal *)
    make_compare_test "RR with arm removed" "work_conserving/rr_3_classes.sched"
      "work_conserving/rr_2_classes.sched"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
    make_compare_test "WFQ with arm removed"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_2_classes.sched"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
  ]

let verydiff =
  [
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms"
      "work_conserving/strict_2_classes.sched"
      "work_conserving/strict_3_classes_ABC.sched"
      (Change ([], VeryDifferent));
    (* WFQ(A,B,C) vs WFQ(A,B,D) *)
    make_compare_test "different WFQ" "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff.sched"
      (Change ([ 2 ], VeryDifferent));
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "work_conserving/rr_2_classes.sched"
      "work_conserving/rr_3_classes_DEF.sched"
      (Change ([], VeryDifferent));
    (* SP(C,B,A) vs SP(B,C,A) *)
    make_compare_test "Strict with arms reordered"
      "work_conserving/strict_3_classes.sched"
      "work_conserving/strict_3_classes_jumbled.sched"
      (Change ([ 0 ], VeryDifferent));
    (* WFQ weights changed *)
    make_compare_test "WFQ with weights changed"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_3_classes_diff_weights.sched"
      (Change ([], VeryDifferent));
    make_compare_test "WFQ with weights changed and arm added"
      "work_conserving/wfq_3_classes.sched"
      "work_conserving/wfq_very_diff.sched"
      (Change ([], VeryDifferent));
  ]

let superpol =
  [
    make_compare_test "B is sub-pol of large tree"
      "work_conserving/FIFO_B.sched" "work_conserving/rr_hier_superpol.sched"
      (Change ([ 1; 0 ], SuperPol));
    make_compare_test "sub-policy of rr_hier"
      "work_conserving/rr_hier_subpol.sched" "work_conserving/rr_hier.sched"
      (Change ([ 1 ], SuperPol));
    make_compare_test "rr_hier is subpol of large tree"
      "work_conserving/rr_hier_subpol.sched"
      "work_conserving/rr_hier_superpol.sched"
      (Change ([ 1; 1 ], SuperPol));
  ]

let deep =
  [
    make_compare_test "RR/Strict hierarchy with arm added deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_add_arm.sched"
      (Change
         ( [ 2; 0 ],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[CY] at 3" }
         ));
    make_compare_test "RR/Strict hierarchy with RR swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_1.sched" Same;
    make_compare_test "RR/Strict hierarchy with SP swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_2.sched"
      (Change ([ 2; 0; 0 ], VeryDifferent));
    make_compare_test "RR/Strict hierarchy with arm removed deep"
      "work_conserving/rr_strict_hier_add_arm.sched"
      "work_conserving/rr_strict_hier.sched"
      (Change ([ 2; 0 ], ArmsRemoved { old_count = 3; new_count = 2 }));
  ]

let suite =
  "compare tests"
  >::: same @ armsadded @ armsremoved @ verydiff @ superpol @ deep

let () = run_test_tt_main suite
