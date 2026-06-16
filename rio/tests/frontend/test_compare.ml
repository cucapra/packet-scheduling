open Rio_core
open Frontend
open OUnit2
open Rio_compare.Compare

let prog_dir = "../progs/"

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
   [Replace { path; arm = walk policy2 path; meta = None }] — the
   IR-side instruction is "wholesale replace this subtree with next's." *)
let make_giveup_test name file1 file2 path =
  let policy2 = prog_to_policy file2 in
  let arm = Policy.walk policy2 path in
  make_compare_test name file1 file2 (Replace { path; arm; meta = None })

let same =
  [
    make_compare_test "same program twice" "strict_ABC" "strict_ABC" Same;
    make_compare_test "merely jumbled in RR" "rr_ABC" "rr_BAC" Same;
    make_compare_test "merely jumbled in WFQ" "wfq_ABC" "wfq_ABC_jumbled" Same;
  ]

(* Add fires on a single-arm insertion at any position of an RR/SP parent
   (after [Policy.normalize] has sorted RR children) or at any position of
   a WFQ parent (where the [weight] field is [Some w]). Multi-arm insertions
   instead "give up" to a wholesale-replace [Replace] (see [verydiff_combos]).
   The [path] inside the payload is the new arm's full position from the root
   of [next]. *)
let one_arm_added =
  [
    (* SP(A,B) vs SP(A,B,C) — append. Positional sugar gives the new arm
       rank 3 (its 1-indexed source position). *)
    make_compare_test "strict arm added at end" "strict_AB" "strict_ABC"
      (Add { path = [ 2 ]; arm = Policy.FIFO "C"; meta = Some 3.0 });
    (* SP(A,C) vs SP(A,B,C) — mid-insert. Positional sugar gives B (now
       written at source index 1) rank 2. Note: this collides with prev's
       C (which kept its parsed rank 2.0); this is a known sharp edge of
       positional sugar across mid-mutations. Explicit ranks avoid it. *)
    make_compare_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      (Add { path = [ 1 ]; arm = Policy.FIFO "B"; meta = Some 2.0 });
    (* RR(A,B) vs RR(A,B,C) — append. *)
    make_compare_test "RR with arm added at end" "rr_AB" "rr_ABC"
      (Add { path = [ 2 ]; arm = Policy.FIFO "C"; meta = None });
    (* RR(A,B) vs RR(B,A,C) — both sort to [A,B,...], so still an append. *)
    make_compare_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      (Add { path = [ 2 ]; arm = Policy.FIFO "C"; meta = None });
    (* Adding an arm deep inside a tree with WFQ at root. The root WFQ
       is a transparent passthrough (lengths and weights line up), so
       the diff surfaces at the rr child (path [1]); the new D inside
       that RR sits at child index 2, giving a full path of [1; 2]. *)
    make_compare_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      (Add { path = [ 1; 2 ]; arm = Policy.FIFO "D"; meta = None });
    (* Adding an arm deep inside the complex tree. After normalize, the
       WFQ pairs sort to (SP, RR[D,E,F], RR[G,H]); the mid rr is at index 1
       and the new NEW inside that RR sits at child index 3, so [1; 3]. *)
    make_compare_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      (Add { path = [ 1; 3 ]; arm = Policy.FIFO "NEW"; meta = None });
  ]

(* WFQ-parent additions: detected when the policy list and the weight list
   each show a single insertion at the same index. The [weight] field
   carries the new slot's weight from [next]. *)
let one_arm_added_wfq =
  [
    (* WFQ(B,A) → WFQ(A:2,B:1,C:3): after normalize the prev pairs sort to
       [(A,2),(B,1)] and next to [(A,2),(B,1),(C,3)]. The C slot is the
       lone insertion (index 2, weight 3). *)
    make_compare_test "WFQ with arm added at end" "wfq_BA" "wfq_ABC"
      (Add { path = [ 2 ]; arm = Policy.FIFO "C"; meta = Some 3.0 });
    (* complex_tree_partial -> complex_tree: at the root WFQ a new
       (rr[D,E,F], 2) slot appears. WFQ pairs sort by arm: SP < RR by
       constructor order, and rr[D,E,F] < rr[G,H] by element compare. So
       prev pairs are [(SP,1),(RR[G,H],3)] and next pairs are
       [(SP,1),(RR[D,E,F],2),(RR[G,H],3)]: insertion at index 1,
       weight 2. *)
    make_compare_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree"
      (Add
         {
           path = [ 1 ];
           arm = Policy.RR [ Policy.FIFO "D"; Policy.FIFO "E"; Policy.FIFO "F" ];
           meta = Some 2.0;
         });
  ]

let armsremoved =
  [
    (* RR(A,B,C) -> RR(A,B): one arm dropped from the end. *)
    make_compare_test "RR with arm removed" "rr_ABC" "rr_AB"
      (Remove { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* WFQ(A:2,B:1,C:3) -> WFQ(B:1,A:2): post-normalize prev has C at
       index 2; that's what was removed. The IR can recover the dropped
       weight (3) from the prev decorated tree if it cares. *)
    make_compare_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (Remove { path = [ 2 ]; arm = Policy.FIFO "C" });
    (* Inverse of the deep-add test: drop NEW from the mid RR (path [1]).
       NEW lived at index 3 inside that RR, so full path [1; 3]. *)
    make_compare_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (Remove { path = [ 1; 3 ]; arm = Policy.FIFO "NEW" });
  ]

let weightchanged =
  [
    (* WFQ(A:2, B:1, C:3) vs WFQ(A:2, B:5, C:3): exactly one weight moved.
       After normalize both sort to (FIFO A, FIFO B, FIFO C); the weight
       at index 1 went 1→5. *)
    make_compare_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      (ChangeWeight { path = [ 1 ]; new_weight = 5.0 });
  ]

let onearmreplaced =
  [
    (* SP(A,B) vs SP(A,C): exactly one arm differs (index 1). *)
    make_compare_test "strict arm changed" "strict_AB" "strict_AC"
      (Replace { path = [ 1 ]; arm = Policy.FIFO "C"; meta = None });
    (* RR(A,B) vs RR(A,D): exactly one arm differs (index 1). *)
    make_compare_test "rr arm changed" "rr_AB" "rr_AD"
      (Replace { path = [ 1 ]; arm = Policy.FIFO "D"; meta = None });
    (* WFQ(A:2,B:1,C:3) vs WFQ(A:2,B:1,Z:3): one slot's arm changed in
       place, weight unchanged. The IR recovers the slot's weight from
       the prev decorated tree. *)
    make_compare_test "WFQ arm changed in place, same weight" "wfq_ABC"
      "wfq_ABZ"
      (Replace { path = [ 2 ]; arm = Policy.FIFO "Z"; meta = None });
  ]

(* WFQ-slot replace with weight change: a single WFQ slot's arm and weight
   both changed. Detected when policy and weight lists each have exactly one
   in-place divergence at the same slot, and the slot's arm-vs-arm diff is
   itself a leaf-level [Replace]. *)
let one_arm_replaced_wfq =
  [
    (* WFQ(A:2,B:1,C:3) → WFQ(A:2,B:1,Z:7): slot 2's arm flipped C→Z and
       weight 3→7 in the same edit. *)
    make_compare_test "WFQ slot with arm change and weight change" "wfq_ABC"
      "wfq_ABZ_diff"
      (Replace { path = [ 2 ]; arm = Policy.FIFO "Z"; meta = Some 7.0 });
  ]

let nested_giveup_demotion =
  [
    make_compare_test "nested Graft demotes to Replace" "strict_AB"
      "strict_A_rrBC"
      (Replace
         {
           path = [ 1 ];
           arm = Policy.RR [ Policy.FIFO "B"; Policy.FIFO "C" ];
           meta = None;
         });
    make_compare_test "nested ChangeRoot demotes to Replace" "strict_A_rrBC"
      "strict_AB"
      (Replace { path = [ 1 ]; arm = Policy.FIFO "B"; meta = None });
  ]

let graft =
  [
    (* After normalize, complex_tree's WFQ pairs are
       (SP[A,B,C],1), (RR[D,E,F],2), (RR[G,H],3) at indices 0, 1, 2.
       FIFO A sits inside the SP at SP's child 0, so path [0; 0]. *)
    make_compare_test "fifo_A is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      (Graft [ 0; 0 ]);
    make_compare_test "strict_ABC is subpol of complex_tree" "strict_ABC"
      "complex_tree" (Graft [ 0 ]);
  ]

(* Inverses of [graft]: [next] is a sub-policy of [prev] (the user
   collapsed the tree to one of its existing subtrees). The carried path
   is the position in [prev] where [next] used to live. *)
let change_root =
  [
    make_compare_test "complex_tree collapsed to fifo_A" "complex_tree" "fifo_A"
      (ChangeRoot [ 0; 0 ]);
    make_compare_test "complex_tree collapsed to strict_ABC" "complex_tree"
      "strict_ABC" (ChangeRoot [ 0 ]);
  ]

(* A menu of cases where the diff is a *combination* of changes each of
   which would be legal in isolation, so [Compare] gives up at the level
   of the divergence and emits [Replace { path; arm = next_at_path; meta = None }]
   — the IR will replace that subtree wholesale via [Designate]. As
   [Compare] gets smarter (e.g., learns to emit a list of edits), entries
   here will migrate to more precise variants. Each entry's comment names
   the chain of legal changes that together overwhelm the analyzer. *)
let verydiff_combos =
  [
    (* wfq_complex = WFQ([(A,1), (RR[B,C],2)]). Next changes RR[B,C]→RR[B,D]
       (a deeper [Replace]) and bumps the weight 2→5 (a [ChangeWeight]) at the
       same slot. *)
    make_giveup_test "WFQ slot with deep diff and weight change" "wfq_complex"
      "wfq_complex_deep_and_weight" [];
    (* RR(A,B) → RR(D,B,A,SP(C,E)): two new arms (D and SP[C,E]) — a
       multi-arm add, hence two [Add]s. *)
    make_giveup_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE" [];
    (* SP(B,A) → SP(A,B,C): swap (= two [Replace] at indices 0/1)
       plus an [Add] at index 2. *)
    make_giveup_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" [];
    (* WFQ(A:1,B:2,C:3) → WFQ(D:1,E:2,F:3): three [Replace]s, one
       per slot. *)
    make_giveup_test "different WFQ classes" "wfq_ABC" "wfq_DEF" [];
    (* RR(A,B) → RR(D,E,F): two [Remove] (A and B drop) plus three
       [Add] (D, E, F appear). *)
    make_giveup_test "RR big diff" "rr_AB" "rr_DEF" [];
    (* WFQ(B,A) → WFQ(A:2,B:2,C:4): one [Add] (C) plus multiple
       [ChangeWeight]s on the existing arms. *)
    make_giveup_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff" [];
    (* SP(A,B) → SP(B,A): two [Replace]s — both positions
       diverge. *)
    make_giveup_test "Strict with arms reordered" "strict_AB" "strict_BA" [];
    (* Same swap one level deep inside complex_tree's SP[A;B;C]->SP[C;B;A].
       The inner SP has multi-divergence (indices 0 and 2 both differ);
       the outer compare_lists tags the SP's parent index. SP is at WFQ
       child index 0 in the normalized tree, so the deep give-up sits at
       path [0]. *)
    make_giveup_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms" [ 0 ];
    (* WFQ(A:2,B:1,C:3) → WFQ(A:2,B:2,C:4): two [ChangeWeight]s — only
       a single-weight edit lands as [ChangeWeight]; multi-weight is
       this combo. *)
    make_giveup_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff" [];
  ]

let suite =
  "compare tests"
  >::: same @ one_arm_added @ one_arm_added_wfq @ armsremoved @ weightchanged
       @ onearmreplaced @ one_arm_replaced_wfq @ verydiff_combos @ graft
       @ change_root @ nested_giveup_demotion

let () = run_test_tt_main suite
