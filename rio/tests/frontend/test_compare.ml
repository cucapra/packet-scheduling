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
    make_compare_test "same program twice" "work_conserving/strict_ABC.sched"
      "work_conserving/strict_ABC.sched" Same;
    make_compare_test "merely jumbled in RR" "work_conserving/rr_ABC.sched"
      "work_conserving/rr_BAC.sched" Same;
    make_compare_test "merely jumbled in WFQ" "work_conserving/wfq_ABC.sched"
      "work_conserving/wfq_ABC_jumbled.sched" Same;
    make_compare_test "different variable names, same structure"
      "work_conserving/complex_tree.sched"
      "work_conserving/complex_tree_weird_var_names.sched" Same;
  ]

let armsadded =
  [
    (* SP(B,A) vs SP(C,B,A) *)
    make_compare_test "strict arm added" "work_conserving/strict_AB.sched"
      "work_conserving/strict_ABC.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 3" } ));
    (* TODO: is this index okay? *)
    (* SP(A,C) vs SP(A,B,C) *)
    make_compare_test "strict arm added in the middle"
      "work_conserving/strict_AC.sched" "work_conserving/strict_ABC.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[B] at 2" } ));
    (* SP(A,C) vs SP(A,B,C) *)
    make_compare_test "strict arm added in the middle"
      "work_conserving/strict_AC.sched" "work_conserving/strict_ABC.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[B] at 2" } ));
    (* RR(A,B) vs RR(A,B,C) *)
    make_compare_test "RR with arm added" "work_conserving/rr_AB.sched"
      "work_conserving/rr_ABC.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* RR(A,B) vs RR(C,A,B) *)
    make_compare_test "RR with arm added (BAC)" "work_conserving/rr_AB.sched"
      "work_conserving/rr_BAC.sched"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* WFQ(A,B) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "work_conserving/wfq_AB.sched"
      "work_conserving/wfq_ABC.sched"
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
    make_compare_test "RR with arm removed" "work_conserving/rr_ABC.sched"
      "work_conserving/rr_AB.sched"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
    make_compare_test "WFQ with arm removed" "work_conserving/wfq_ABC.sched"
      "work_conserving/wfq_AB.sched"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
  ]

let verydiff =
  [
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms"
      "work_conserving/strict_BA.sched" "work_conserving/strict_ABC.sched"
      (Change ([], VeryDifferent));
    (* WFQ(A,B,C) vs WFQ(A,B,D) *)
    make_compare_test "different WFQ" "work_conserving/wfq_ABC.sched"
      "work_conserving/wfq_ABC_diff.sched"
      (Change ([], VeryDifferent));
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "work_conserving/rr_AB.sched"
      "work_conserving/rr_DEF.sched"
      (Change ([], VeryDifferent));
    (* SP(C,B,A) vs SP(B,C,A) *)
    make_compare_test "Strict with arms reordered"
      "work_conserving/strict_AB.sched" "work_conserving/strict_BA.sched"
      (Change ([ 0 ], VeryDifferent));
    (* WFQ weights changed *)
    make_compare_test "WFQ with weights changed" "work_conserving/wfq_ABC.sched"
      "work_conserving/wfq_ABC_diff.sched"
      (Change ([], VeryDifferent));
    make_compare_test "WFQ with weights changed and arm added"
      "work_conserving/wfq_AB.sched" "work_conserving/wfq_ABC_diff.sched"
      (Change ([], VeryDifferent));
  ]

let superpol =
  [
    make_compare_test "fifo_A is sub-pol of complex_tree"
      "work_conserving/fifo_A.sched" "work_conserving/complex_tree.sched"
      (Change ([ 1; 0 ], SuperPol));
    make_compare_test "strict_ABC is subpol of complex_tree"
      "work_conserving/strict_ABC.sched"
      "work_conserving/complex_tree_weird_var_names.sched"
      (Change ([ 1 ], SuperPol));
  ]

let deep =
  [
    make_compare_test "complex tree add arm deep"
      "work_conserving/complex_tree.sched"
      "work_conserving/complex_tree_add_arm_deep.sched"
      (Change
         ( [ 2 ],
           ArmsAdded
             { old_count = 3; new_count = 4; details = "added fifo[NEW] at 3" }
         ));
    make_compare_test "RR/Strict hierarchy with RR swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_1.sched" Same;
    make_compare_test "RR/Strict hierarchy with SP swap deep"
      "work_conserving/rr_strict_hier.sched"
      "work_conserving/rr_strict_hier_swap_deep_2.sched"
      (Change ([ 2; 0; 0 ], VeryDifferent));
    make_compare_test "complex tree remove arm deep"
      "work_conserving/complex_tree_add_arm_deep.sched"
      "work_conserving/complex_tree.sched"
      (Change ([ 2 ], ArmsRemoved { old_count = 4; new_count = 3 }));
  ]

let suite =
  "compare tests"
  >::: same @ armsadded @ armsremoved @ verydiff @ superpol @ deep

let () = run_test_tt_main suite
