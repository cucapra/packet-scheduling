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
    make_compare_test "complex tree with an rr-reordering deep down"
      "complex_tree" "complex_tree_swap_rr_arms" Same;
  ]

(* ArmAdded fires only on a single-arm append at a UNION/RR/SP parent
   (after [Policy.normalize] has sorted UNION/RR children). WFQ never
   reports ArmAdded itself — see [verydiff] for the cases that get
   demoted. *)
let armadded =
  [
    (* SP(A,B) vs SP(A,B,C) — append *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (Change ([], ArmAdded (Policy.FIFO "C")));
    (* RR(A,B) vs RR(A,B,C) — append after sort *)
    make_compare_test "RR with arm added at end" "rr_AB" "rr_ABC"
      (Change ([], ArmAdded (Policy.FIFO "C")));
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so still an append *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (Change ([], ArmAdded (Policy.FIFO "C")));
    (* Adding an arm deep inside a tree with WFQ at root. The root WFQ
       is a transparent passthrough (lengths and weights line up), so
       the diff surfaces at the rr child (path [1]). *)
    make_compare_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      (Change ([ 1 ], ArmAdded (Policy.FIFO "D")));
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (UNION, SP, RR), so the rr arm is at index 2. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (Change ([ 2 ], ArmAdded (Policy.FIFO "NEW")));
  ]

let armsremoved =
  (* We just give up. *)
  [
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (Change ([], VeryDifferent));
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (Change ([], VeryDifferent));
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (Change ([ 2 ], VeryDifferent));
  ]

let verydiff =
  [
    (* SP(A,C) vs SP(A,B,C) — mid-insert. SP isn't sorted by normalize, so
       [A,C] is not a one-arm append of [A,B,C]; we degrade to
       VeryDifferent. *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (Change ([], VeryDifferent));
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC"
      (Change ([], VeryDifferent));
    (* SP(A,B) vs SP(A,C) *)
    make_compare_test "strict arm changed" "strict_AB" "strict_AC"
      (Change ([ 1 ], VeryDifferent));
    (* RR(A,B) vs RR(A,D) *)
    make_compare_test "rr arm changed" "rr_AB" "rr_AD"
      (Change ([ 1 ], VeryDifferent));
    (* RR(A,B) vs RR(D,B,A,SP(C,E)) — after sort, [A,B] -> [A,B,D,SP[C,E]]
       adds two arms, so out of scope. *)
    make_compare_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE"
      (Change ([], VeryDifferent));
    (* WFQ(B,A) vs WFQ(A,B,C) — WFQ never reports ArmAdded. *)
    make_compare_test "WFQ with arm added" "wfq_BA" "wfq_ABC"
      (Change ([], VeryDifferent));
    (* complex_tree_partial vs complex_tree — a WFQ-level add at the root. *)
    make_compare_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree"
      (Change ([], VeryDifferent));
    (* WFQ(A_1,B_2,C_3) vs WFQ(A_2,B_2,C_4): classes same, weight different *)
    make_compare_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff"
      (Change ([], VeryDifferent));
    (* WFQ(A_1,B_2,C_3) vs WFQ(D_1,E_2,F_3): classes different, weights same *)
    make_compare_test "different WFQ classes" "wfq_ABC" "wfq_DEF"
      (Change ([], VeryDifferent));
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "rr_AB" "rr_DEF"
      (Change ([], VeryDifferent));
    (* SP(A,B) vs SP(B,A) *)
    make_compare_test "Strict with arms reordered" "strict_AB" "strict_BA"
      (Change ([ 0 ], VeryDifferent));
    make_compare_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff"
      (Change ([], VeryDifferent));
    make_compare_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms"
      (Change ([ 1; 0 ], VeryDifferent));
  ]

let superpol =
  [
    make_compare_test "fifo_G is sub-pol of union[G,H]" "fifo_G" "union_GH"
      (Change ([ 0 ], SuperPol));
    make_compare_test "fifo_A is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      (Change ([ 1; 0 ], SuperPol));
    make_compare_test "strict_ABC is subpol of complex_tree" "strict_ABC"
      "complex_tree"
      (Change ([ 1 ], SuperPol));
    make_compare_test "union_GH is subpol of complex_tree" "union_GH"
      "complex_tree"
      (Change ([ 0 ], SuperPol));
  ]

let suite =
  "compare tests" >::: same @ armadded @ armsremoved @ verydiff @ superpol

let () = run_test_tt_main suite
