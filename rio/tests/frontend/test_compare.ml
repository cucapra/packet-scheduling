open Frontend
open OUnit2
open Rio_compare.Compare

let prog_dir = "../../../../../progs/"

let prog_to_policy file =
  let filewithpath = prog_dir ^ "work_conserving/" ^ file ^ ".sched" in
  filewithpath |> Parser.parse_file |> Policy.of_program

let make_compare_test name file1 file2 expected_diff =
  let policy1 = prog_to_policy file1 in
  let policy2 = prog_to_policy file2 in
  let actual_diff = analyze policy1 policy2 in
  name >:: fun _ ->
  assert_equal expected_diff actual_diff ~printer:(fun d ->
      Rio_compare.Compare.to_string d)

(* Helper for the "give up" cases below: [Compare] couldn't break the
   diff down at depth [List.length path], so it emits
   [OneArmReplaced { path; arm = walk policy2 path }] — the IR-side
   instruction is "wholesale replace this subtree with next's." *)
let make_giveup_test name file1 file2 path =
  let policy2 = prog_to_policy file2 in
  let arm = Policy.walk policy2 path in
  make_compare_test name file1 file2 (OneArmReplaced { path; arm })

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
   children). WFQ-add does *not* land here — see [verydiff_combos] —
   because the new slot's weight can't ride along on [arm_diff], so a
   WFQ-add is logically [OneArmAdded + WeightChanged]. 
   Multi-arm insertions instead "give up" to a wholesale-replace
   [OneArmReplaced] (see [verydiff_combos]). The
   [path] inside [arm_diff] is the new arm's full position from the
   root of [next]. *)
let one_arm_added =
  [
    (* SP(A,B) vs SP(A,B,C) — append; new arm at root child 2. *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (OneArmAdded { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* SP(A,C) vs SP(A,B,C) — mid-insert; new arm at root child 1. *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (OneArmAdded { path = [ 1 ]; arm = Policy.FIFO "B" });
    (* RR(A,B) vs RR(A,B,C) — append. *)
    make_compare_test "RR with arm added at end" "rr_AB" "rr_ABC"
      (OneArmAdded { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so still an append. *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (OneArmAdded { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* Adding an arm deep inside a tree with WFQ at root. The root WFQ
       is a transparent passthrough (lengths and weights line up), so
       the diff surfaces at the rr child (path [1]); the new D inside
       that RR sits at child index 2, giving a full path of [1; 2]. *)
    make_compare_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      (OneArmAdded { path = [ 1; 2 ]; arm = Policy.FIFO "D" });
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (UNION, SP, RR), so the rr arm is at index 2;
       the new NEW inside that RR sits at child index 3 → [2; 3]. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (OneArmAdded { path = [ 2; 3 ]; arm = Policy.FIFO "NEW" });
  ]

let armsremoved =
  [
    (* RR(A,B,C) -> RR(A,B): one arm dropped from the end. *)
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* WFQ(A:2,B:1,C:3) -> WFQ(B:1,A:2): post-normalize prev has C at
       index 2; that's what was removed. The IR can recover the dropped
       weight (3) from the prev decorated tree if it cares. *)
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (OneArmRemoved { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* Inverse of the deep-add test: drop NEW from the inner RR (path [2]).
       NEW lived at index 3 inside that RR → full path [2; 3]. *)
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (OneArmRemoved { path = [ 2; 3 ]; arm = Policy.FIFO "NEW" });
  ]

let weightchanged =
  [
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:5, C:3): exactly one weight moved.
       After normalize both sort to (FIFO A, FIFO B, FIFO C); the weight
       at index 1 went 1→5. *)
    make_compare_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      (WeightChanged { path = [ 1 ]; new_weight = 5.0 });
  ]

let onearmreplaced =
  [
    (* SP(A,B) vs SP(A,C): exactly one arm differs (index 1). *)
    make_compare_test "strict arm changed" "strict_AB" "strict_AC"
      (OneArmReplaced { path = [ 1 ]; arm = Policy.FIFO "C" });
    (* RR(A,B) vs RR(A,D): exactly one arm differs (index 1). *)
    make_compare_test "rr arm changed" "rr_AB" "rr_AD"
      (OneArmReplaced { path = [ 1 ]; arm = Policy.FIFO "D" });
    (* WFQ(A:2,B:1,C:3) vs WFQ(A:2,B:1,Z:3): one slot's arm changed in
       place, weight unchanged. The IR recovers the slot's weight from
       the prev decorated tree. *)
    make_compare_test "WFQ arm changed in place, same weight" "wfq_ABC"
      "wfq_ABZ"
      (OneArmReplaced { path = [ 2 ]; arm = Policy.FIFO "Z" });
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

(* A menu of cases where the diff is a *combination* of changes each of
   which would be legal in isolation, so [Compare] gives up at the level
   of the divergence and emits [OneArmReplaced { path; arm = next_at_path }]
   — the IR will replace that subtree wholesale via [Designate]. As
   [Compare] gets smarter (e.g., learns to emit a list of edits), entries
   here will migrate to more precise variants. Each entry's comment names
   the chain of legal changes that together overwhelm the analyzer. *)
let verydiff_combos =
  [
    (* wfq_complex = WFQ([(A,1), (RR[B,C],2)]). Next changes RR[B,C]→RR[B,D]
       (a deeper [OneArmReplaced]) and bumps the weight 2→5 (a
       [WeightChanged]) at the same slot. *)
    make_giveup_test "WFQ slot with deep diff and weight change" "wfq_complex"
      "wfq_complex_deep_and_weight" [];
    (* WFQ(A:2,B:1,C:3) → WFQ(A:2,B:1,Z:7): one slot's arm changed
       (C→Z, an [OneArmReplaced]) and its weight changed (3→7, a
       [WeightChanged]). Same slot, two distinct edits. *)
    make_giveup_test "WFQ slot with arm change and weight change" "wfq_ABC"
      "wfq_ABZ_diff" [];
    (* WFQ(B,A) → WFQ(A:2,B:1,C:3): adding a WFQ arm is logically
       [OneArmAdded] (the arm) + [WeightChanged] (the new slot's
       weight). [arm_diff] no longer carries a weight, so this combo
       can't fold into a single variant. *)
    make_giveup_test "WFQ with arm added" "wfq_BA" "wfq_ABC" [];
    (* complex_tree_partial → complex_tree: a WFQ-level arm-add at the
       root (the RR subtree, weight 2). Same combo as above. *)
    make_giveup_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree" [];
    (* RR(A,B) → RR(D,B,A,SP(C,E)): two new arms (D and SP[C,E]) — a
       multi-arm add, hence two [OneArmAdded]s. *)
    make_giveup_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE" [];
    (* SP(B,A) → SP(A,B,C): swap (= two [OneArmReplaced] at indices 0/1)
       plus an [OneArmAdded] at index 2. *)
    make_giveup_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" [];
    (* WFQ(A:1,B:2,C:3) → WFQ(D:1,E:2,F:3): three [OneArmReplaced]s, one
       per slot. *)
    make_giveup_test "different WFQ classes" "wfq_ABC" "wfq_DEF" [];
    (* RR(A,B) → RR(D,E,F): two [OneArmRemoved] (A and B drop) plus three
       [OneArmAdded] (D, E, F appear). *)
    make_giveup_test "RR big diff" "rr_AB" "rr_DEF" [];
    (* WFQ(B,A) → WFQ(A:2,B:2,C:4): one [OneArmAdded] (C) plus multiple
       [WeightChanged]s on the existing arms. *)
    make_giveup_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff" [];
    (* SP(A,B) → SP(B,A): two [OneArmReplaced]s — both positions
       diverge. *)
    make_giveup_test "Strict with arms reordered" "strict_AB" "strict_BA" [];
    (* Same swap one level deep inside complex_tree's SP[A;B;C]→SP[C;B;A].
       The inner SP has multi-divergence (indices 0 and 2 both differ);
       the outer compare_lists tags the SP's parent index, giving a
       deep give-up at path [1]. *)
    make_giveup_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms" [ 1 ];
    (* WFQ(A:2,B:1,C:3) → WFQ(A:2,B:2,C:4): two [WeightChanged]s — only
       a single-weight edit lands as [WeightChanged]; multi-weight is
       this combo. *)
    make_giveup_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff" [];
  ]

let suite =
  "compare tests"
  >::: same @ one_arm_added @ armsremoved @ weightchanged @ onearmreplaced
       @ verydiff_combos @ superpol @ subpol

let () = run_test_tt_main suite
