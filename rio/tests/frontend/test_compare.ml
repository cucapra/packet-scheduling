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

(* OneArmAppended fires only on a single-arm append at a UNION/RR/SP
   parent (after [Policy.normalize] has sorted UNION/RR children). It
   binds greedily — when a change is *both* a one-arm append and a more
   general ArmsAdded, OneArmAppended wins. WFQ never
   reports OneArmAppended itself; see [armsadded] / [verydiff] for the
   WFQ cases. *)
let one_arm_appended =
  [
    (* SP(A,B) vs SP(A,B,C) — append *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (Change ([], OneArmAppended (Policy.FIFO "C")));
    (* RR(A,B) vs RR(A,B,C) — append *)
    make_compare_test "RR with arm added at end" "rr_AB" "rr_ABC"
      (Change ([], OneArmAppended (Policy.FIFO "C")));
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so still an append *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (Change ([], OneArmAppended (Policy.FIFO "C")));
    (* Adding an arm deep inside a tree with WFQ at root. The root WFQ
       is a transparent passthrough (lengths and weights line up), so
       the diff surfaces at the rr child (path [1]). *)
    make_compare_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      (Change ([ 1 ], OneArmAppended (Policy.FIFO "D")));
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (UNION, SP, RR), so the rr arm is at index 2. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (Change ([ 2 ], OneArmAppended (Policy.FIFO "NEW")));
  ]

let armsadded =
  [
    (* SP(A,C) vs SP(A,B,C) *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (Change
         ( [],
           ArmsAdded
             { old_count = 2; new_count = 3; details = "added fifo[B] at 1" } ));
    (* RR(A,B) vs RR(D,B,A,SP(C,E)) *)
    make_compare_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE"
      (Change
         ( [],
           ArmsAdded
             {
               old_count = 2;
               new_count = 4;
               details = "added fifo[D], strict[fifo[C], fifo[E]]";
             } ));
    (* WFQ(B,A) vs WFQ(A,B,C) *)
    make_compare_test "WFQ with arm added" "wfq_BA" "wfq_ABC"
      (Change
         ( [],
           ArmsAdded
             {
               old_count = 2;
               new_count = 3;
               details = "added fifo[C] with weight 3";
             } ));
    (* complex_tree_partial vs complex_tree — a WFQ-level add at the root.
       Adds an entire RR subtree as a new weighted arm. *)
    make_compare_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree"
      (Change
         ( [],
           ArmsAdded
             {
               old_count = 2;
               new_count = 3;
               details = "added rr[fifo[D], fifo[E], fifo[F]] with weight 2";
             } ));
  ]

let armsremoved =
  [
    (* RR(A,B,C) -> RR(A,B): one arm dropped from the end. *)
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (Change
         ( [],
           ArmsRemoved
             { old_count = 3; new_count = 2; details = "removed fifo[C]" } ));
    (* WFQ(A:2,B:1,C:3) -> WFQ(B:1,A:2): after normalize sorts pairs, prev =
       [(A,2);(B,1);(C,3)] and next = [(A,2);(B,1)], so (C,3) was removed. *)
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (Change
         ( [],
           ArmsRemoved
             {
               old_count = 3;
               new_count = 2;
               details = "removed fifo[C] with weight 3";
             } ));
    (* Inverse of the deep-add test: drop NEW from the inner RR (path [2]). *)
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (Change
         ( [ 2 ],
           ArmsRemoved
             { old_count = 4; new_count = 3; details = "removed fifo[NEW]" } ));
  ]

let weightchanged =
  [
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:2, C:4): same arms, two weights moved.
       After [Policy.normalize] both sort to (FIFO A, FIFO B, FIFO C). *)
    make_compare_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff"
      (Change
         ( [],
           WeightChanged { details = "fifo[B]: 1 → 2, fifo[C]: 3 → 4" } ));
  ]

let verydiff =
  [
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
  "compare tests"
  >::: same @ one_arm_appended @ armsadded @ armsremoved @ weightchanged
       @ verydiff @ superpol

let () = run_test_tt_main suite
