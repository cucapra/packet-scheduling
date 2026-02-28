open Frontend
open OUnit2
open Rio_compare.Compare

let prog_dir = "../../../../../progs/"

let make_compare_test name file1 file2 expected_diff =
  let prog_to_policy file =
    let filewithpath = prog_dir ^ "work_conserving/" ^ file ^ ".sched" in
    filewithpath |> Parser.parse_file |> Policy.of_program
  in
  let policy1 = prog_to_policy file1 in
  let policy2 = prog_to_policy file2 in
  let actual_diff = analyze policy1 policy2 in
  name >:: fun _ ->
  assert_equal expected_diff actual_diff ~printer:(fun d ->
      Rio_compare.Compare.to_string d)

let same =
  [
    make_compare_test "same program twice" "strict_ABC" "strict_ABC" Same;
    make_compare_test "merely jumbled in RR" "rr_ABC" "rr_BAC" Same;
    make_compare_test "merely jumbled in WFQ" "wfq_ABC" "wfq_ABC_jumbled" Same;
    make_compare_test "different variable names, same structure" "complex_tree"
      "complex_tree_weird_var_names" Same;
  ]

let armsadded =
  [
    (* SP(B,A) vs SP(C,B,A) *)
    make_compare_test "strict arm added" "strict_AB" "strict_ABC"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 3" } ));
    (* SP(A,C) vs SP(A,B,C) *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[B] at 2" } ));
    (* RR(A,B) vs RR(A,B,C) *)
    make_compare_test "RR with arm added" "rr_AB" "rr_ABC"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* RR(A,B) vs RR(B,A,C) *)
    make_compare_test "RR with arm added (BAC)" "rr_AB" "rr_BAC"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[C] at 2" } ));
    (* WFQ(A,B) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "wfq_AB" "wfq_ABC"
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
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_AB"
      (Change ([], ArmsRemoved { old_count = 3; new_count = 2 }));
  ]

let verydiff =
  [
    (* TODO: make deeper diffs *)
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC"
      (Change ([], VeryDifferent));
    (* WFQ(A,B,C) vs WFQ(A,B,D) *)
    make_compare_test "different WFQ" "wfq_ABC" "wfq_ABC_diff"
      (Change ([], VeryDifferent));
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "rr_AB" "rr_DEF"
      (Change ([], VeryDifferent));
    (* SP(C,B,A) vs SP(B,C,A) *)
    make_compare_test "Strict with arms reordered" "strict_AB" "strict_BA"
      (Change ([ 0 ], VeryDifferent));
    (* WFQ weights changed *)
    make_compare_test "WFQ with weights changed" "wfq_ABC" "wfq_ABC_diff"
      (Change ([], VeryDifferent));
    make_compare_test "WFQ with weights changed and arm added" "wfq_AB"
      "wfq_ABC_diff"
      (Change ([], VeryDifferent));
  ]

let superpol =
  [
    make_compare_test "fifo_A is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      (Change ([ 1; 0 ], SuperPol));
    make_compare_test "strict_ABC is subpol of complex_tree" "strict_ABC"
      "complex_tree_weird_var_names"
      (Change ([ 1 ], SuperPol));
  ]

let deep =
  [
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (Change
         ( [ 2 ],
           ArmsAdded
             { old_count = 3; new_count = 4; details = "added fifo[NEW] at 3" }
         ));
    make_compare_test "complex tree with an rr-reordering deep down"
      "complex_tree" "complex_tree_swap_rr_arms" Same;
    make_compare_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms"
      (Change ([ 1; 0 ], VeryDifferent));
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (Change ([ 2 ], ArmsRemoved { old_count = 4; new_count = 3 }));
  ]

let suite =
  "compare tests"
  >::: same @ armsadded @ armsremoved @ verydiff @ superpol @ deep

let () = run_test_tt_main suite
