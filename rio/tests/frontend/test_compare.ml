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
   general ArmsAdded, OneArmAppended wins. WFQ never reports
   OneArmAppended itself; see [armsadded] / [verydiff] for the WFQ
   cases. The [path] inside [arm_diff] is the new arm's full position
   from the root of [next]. *)
let one_arm_appended =
  [
    (* SP(A,B) vs SP(A,B,C) — append; new arm at root child 2. *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (OneArmAppended { path = [ 2 ]; arm = Policy.FIFO "C"; weight = None });
    (* RR(A,B) vs RR(A,B,C) — append. *)
    make_compare_test "RR with arm added at end" "rr_AB" "rr_ABC"
      (OneArmAppended { path = [ 2 ]; arm = Policy.FIFO "C"; weight = None });
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so still an append. *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (OneArmAppended { path = [ 2 ]; arm = Policy.FIFO "C"; weight = None });
    (* Adding an arm deep inside a tree with WFQ at root. The root WFQ
       is a transparent passthrough (lengths and weights line up), so
       the diff surfaces at the rr child (path [1]); the new D inside
       that RR sits at child index 2, giving a full path of [1; 2]. *)
    make_compare_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      (OneArmAppended { path = [ 1; 2 ]; arm = Policy.FIFO "D"; weight = None });
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (UNION, SP, RR), so the rr arm is at index 2;
       the new NEW inside that RR sits at child index 3 → [2; 3]. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (OneArmAppended
         { path = [ 2; 3 ]; arm = Policy.FIFO "NEW"; weight = None });
  ]

let armsadded =
  [
    (* SP(A,C) vs SP(A,B,C): B inserted at root child 1. *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (ArmsAdded [ { path = [ 1 ]; arm = Policy.FIFO "B"; weight = None } ]);
    (* RR(A,B) vs RR(D,B,A,SP(C,E)). Post-normalize next is
       [A; B; D; SP[C;E]]; the additions are D at index 2 and SP at 3. *)
    make_compare_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE"
      (ArmsAdded
         [
           { path = [ 2 ]; arm = Policy.FIFO "D"; weight = None };
           {
             path = [ 3 ];
             arm = Policy.SP [ Policy.FIFO "C"; Policy.FIFO "E" ];
             weight = None;
           };
         ]);
    (* WFQ(B,A) vs WFQ(A,B,C): post-normalize next is [(A,2);(B,1);(C,3)],
       so (C, 3) was added at index 2. *)
    make_compare_test "WFQ with arm added" "wfq_BA" "wfq_ABC"
      (ArmsAdded [ { path = [ 2 ]; arm = Policy.FIFO "C"; weight = Some 3.0 } ]);
    (* complex_tree_partial vs complex_tree — a WFQ-level add at the root.
       Post-normalize next has children sorted to (UNION, SP, RR); RR is the
       new arm at index 2 with weight 2. *)
    make_compare_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree"
      (ArmsAdded
         [
           {
             path = [ 2 ];
             arm =
               Policy.RR [ Policy.FIFO "D"; Policy.FIFO "E"; Policy.FIFO "F" ];
             weight = Some 2.0;
           };
         ]);
  ]

let armsremoved =
  [
    (* RR(A,B,C) -> RR(A,B): one arm dropped from the end. *)
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C"; weight = None });
    (* WFQ(A:2,B:1,C:3) -> WFQ(B:1,A:2): post-normalize prev has C at
       index 2 with weight 3; that's what was removed. *)
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C"; weight = Some 3.0 });
    (* Inverse of the deep-add test: drop NEW from the inner RR (path [2]).
       NEW lived at index 3 inside that RR → full path [2; 3]. *)
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (OneArmRemoved { path = [ 2; 3 ]; arm = Policy.FIFO "NEW"; weight = None });
  ]

let weightchanged =
  [
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:5, C:3): exactly one weight moved.
       After normalize both sort to (FIFO A, FIFO B, FIFO C); the weight
       at index 1 went 1→5. *)
    make_compare_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      (WeightChanged { path = [ 1 ]; new_weight = 5.0 });
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:2, C:4): two weights moved.
       [WeightChanged] now describes only single-weight edits, so a
       multi-weight change degrades to [VeryDifferent]. *)
    make_compare_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff"
      (VeryDifferent []);
  ]

let onearmreplaced =
  [
    (* SP(A,B) vs SP(A,C): exactly one arm differs (index 1). *)
    make_compare_test "strict arm changed" "strict_AB" "strict_AC"
      (OneArmReplaced { path = [ 1 ]; arm = Policy.FIFO "C"; weight = None });
    (* RR(A,B) vs RR(A,D): exactly one arm differs (index 1). *)
    make_compare_test "rr arm changed" "rr_AB" "rr_AD"
      (OneArmReplaced { path = [ 1 ]; arm = Policy.FIFO "D"; weight = None });
  ]

let verydiff =
  [
    (* SP(B,A) vs SP(A,B,C) *)
    make_compare_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" (VeryDifferent []);
    (* WFQ(A_1,B_2,C_3) vs WFQ(D_1,E_2,F_3): classes different, weights same *)
    make_compare_test "different WFQ classes" "wfq_ABC" "wfq_DEF"
      (VeryDifferent []);
    (* RR(A,B) vs RR(D,E,F) *)
    make_compare_test "RR big diff" "rr_AB" "rr_DEF" (VeryDifferent []);
    make_compare_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff" (VeryDifferent []);
    (* SP(A,B) vs SP(B,A): two positions differ (index 0 and index 1).
       OneArmReplaced declines to fire when the change isn't pinpointable
       to a single position; falls back to VeryDifferent at this level. *)
    make_compare_test "Strict with arms reordered" "strict_AB" "strict_BA"
      (VeryDifferent []);
    (* Same shape as above, one level deeper: complex_tree's SP[A;B;C]
       becomes SP[C;B;A] at root child 1. The inner SP has multi-divergence
       (indices 0 and 2 both differ), so compare_lists returns
       VeryDifferent [] for the SP. The outer compare_lists wraps that
       with the SP's index in the WFQ root, giving VeryDifferent [1]. *)
    make_compare_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms" (VeryDifferent [ 1 ]);
  ]

let superpol =
  [
    make_compare_test "fifo_G is sub-pol of union[G,H]" "fifo_G" "union_GH"
      (SuperPol [ 0 ]);
    make_compare_test "fifo_A is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      (SuperPol [ 1; 0 ]);
    make_compare_test "strict_ABC is subpol of complex_tree" "strict_ABC"
      "complex_tree" (SuperPol [ 1 ]);
    make_compare_test "union_GH is subpol of complex_tree" "union_GH"
      "complex_tree" (SuperPol [ 0 ]);
  ]

(* Inverses of [superpol]: [next] is a sub-policy of [prev] (the user
   collapsed the tree to one of its existing subtrees). The carried path
   is the position in [prev] where [next] used to live. *)
let subpol =
  [
    make_compare_test "union[G,H] collapsed to fifo_G" "union_GH" "fifo_G"
      (SubPol [ 0 ]);
    make_compare_test "complex_tree collapsed to fifo_A" "complex_tree" "fifo_A"
      (SubPol [ 1; 0 ]);
    make_compare_test "complex_tree collapsed to strict_ABC" "complex_tree"
      "strict_ABC" (SubPol [ 1 ]);
    make_compare_test "complex_tree collapsed to union_GH" "complex_tree"
      "union_GH" (SubPol [ 0 ]);
  ]

let suite =
  "compare tests"
  >::: same @ one_arm_appended @ armsadded @ armsremoved @ weightchanged
       @ onearmreplaced @ verydiff @ superpol @ subpol

let () = run_test_tt_main suite
