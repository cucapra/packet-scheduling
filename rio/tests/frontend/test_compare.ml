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

(* OneArmAdded fires on a single-arm insertion at any position of a
   UNION/RR/SP parent (after [Policy.normalize] has sorted UNION/RR
   children). WFQ-add does *not* land here, it goes to [verydiff_combos]. 
   The [path] inside [arm_diff] is the new arm's full position from the
   root of [next]. *)
let one_arm_added =
  [
    (* SP(A,B) vs SP(A,B,C) — append; new arm at root child 2. *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (OneArmAdded { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* SP(A,C) vs SP(A,B,C) — mid-insert; new arm at root child 1. *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (OneArmAdded { path = [ 1 ]; arm = Policy.FIFO "B" });
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so [path = [2]]. *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (OneArmAdded { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (UNION, SP, RR), so the rr arm is at index 2;
       the new NEW inside that RR sits at child index 3 to [2; 3]. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (OneArmAdded { path = [ 2; 3 ]; arm = Policy.FIFO "NEW" });
  ]

let armsremoved =
  [
    (* RR(A,B,C) -> RR(A,B): one arm dropped from the end. *)
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* WFQ(A:2,B:1,C:3) -> WFQ(B:1,A:2). *)
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* Inverse of the deep-add test: drop NEW from the inner RR (path [2]).
       NEW lived at index 3 inside that RR to full path [2; 3]. *)
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (OneArmRemoved { path = [ 2; 3 ]; arm = Policy.FIFO "NEW" });
  ]

let weightchanged =
  [
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:5, C:3) *)
    make_compare_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      (WeightChanged { path = [ 1 ]; new_weight = 5.0 });
  ]

let onearmreplaced =
  [
    (* SP(A,B) vs SP(A,C): exactly one arm differs (index 1). *)
    make_compare_test "strict arm changed" "strict_AB" "strict_AC"
      (OneArmReplaced { path = [ 1 ]; arm = Policy.FIFO "C" });
    (* WFQ(A:2,B:1,C:3) vs WFQ(A:2,B:1,Z:3), note the weights are unchanged. *)
    make_compare_test "WFQ arm changed in place, same weight" "wfq_ABC"
      "wfq_ABZ"
      (OneArmReplaced { path = [ 2 ]; arm = Policy.FIFO "Z" });
  ]

(* The carried path is the position in [next] where [prev] used to live. *)
let superpol =
  [
    make_compare_test "fifo[G] is sub-pol of union[G,H]" "fifo_G" "union_GH"
      (SuperPol [ 0 ]);
    make_compare_test "fifo[A] is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      (SuperPol [ 1; 0 ]);
  ]

(* The carried path is the position in [prev] where [next] used to live. *)
let subpol =
  [
    make_compare_test "union[G,H] collapsed to fifo_G" "union_GH" "fifo_G"
      (SubPol [ 0 ]);
    make_compare_test "complex_tree collapsed to fifo_A" "complex_tree" "fifo_A"
      (SubPol [ 1; 0 ]);
  ]

(* A menu of cases that come back [VeryDifferent] specifically because the
   diff is a *combination* of changes, each of which would be legal in
   isolation. Useful for nailing down where the patcher gives up even when
   the individual edits are tractable. Each entry's comment names the
   chain of legal changes. *)
let verydiff_combos =
  [
    (* WFQ(A:2,B:1,C:3) to WFQ(A:2,B:1,Z:7): one slot's arm changed
       (C to Z, an [OneArmReplaced]) and its weight changed (3 to 7, a
       [WeightChanged]). Same slot, two distinct edits. *)
    make_compare_test "WFQ slot with arm change and weight change" "wfq_ABC"
      "wfq_ABZ_diff" (VeryDifferent []);
    (* WFQ(B,A) to WFQ(A:2,B:1,C:3): adding a WFQ arm is logically
       [OneArmAdded] + [WeightChanged]. *)
    make_compare_test "WFQ with arm added" "wfq_BA" "wfq_ABC" (VeryDifferent []);
    (* RR(A,B) to RR(D,B,A,SP(C,E)): two [OneArmAdded]s. *)
    make_compare_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE" (VeryDifferent []);
    (* SP(B,A) to SP(A,B,C): swap (= two [OneArmReplaced] at indices 0/1)
       plus an [OneArmAdded] at index 2. *)
    make_compare_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" (VeryDifferent []);
    (* WFQ(A:1,B:2,C:3) to WFQ(D:1,E:2,F:3): three [OneArmReplaced]s, one
       per slot. *)
    make_compare_test "different WFQ classes" "wfq_ABC" "wfq_DEF"
      (VeryDifferent []);
    (* RR(A,B) to RR(D,E,F): two [OneArmRemoved] (A and B drop) plus three
       [OneArmAdded] (D, E, F appear). *)
    make_compare_test "RR big diff" "rr_AB" "rr_DEF" (VeryDifferent []);
    (* WFQ(B,A) to WFQ(A:2,B:2,C:4): one [OneArmAdded] (C) plus multiple
       [WeightChanged]s on the existing arms. *)
    make_compare_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff" (VeryDifferent []);
    (* SP(A,B) to SP(B,A): two [OneArmReplaced]s — both positions
       diverge. *)
    make_compare_test "Strict with arms reordered" "strict_AB" "strict_BA"
      (VeryDifferent []);
    (* Same swap one level deep inside complex_tree's SP[A;B;C]toSP[C;B;A].
       The inner SP has multi-divergence (indices 0 and 2 both differ);
       the outer compare_lists tags the SP's parent index, giving
       [VeryDifferent [1]]. Two [OneArmReplaced]s deep. *)
    make_compare_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms" (VeryDifferent [ 1 ]);
    (* WFQ(A:2,B:1,C:3) to WFQ(A:2,B:2,C:4): two [WeightChanged]s — only
       a single-weight edit lands as [WeightChanged]; multi-weight is
       this combo. *)
    make_compare_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff"
      (VeryDifferent []);
  ]

let suite =
  "compare tests"
  >::: same @ one_arm_added @ armsremoved @ weightchanged @ onearmreplaced
       @ verydiff_combos @ superpol @ subpol

let () = run_test_tt_main suite
