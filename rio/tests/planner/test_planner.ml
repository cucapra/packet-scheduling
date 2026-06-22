open Rio_core
open Frontend
open OUnit2
module Delta = Rio_delta.Delta
module Planner = Rio_planner.Planner

let prog_dir = "../progs/"

let prog_to_policy file =
  let filewithpath = prog_dir ^ "work_conserving/" ^ file ^ ".sched" in
  filewithpath |> Parser.parse_file |> Pol.of_program

let make_planner_test name file1 file2 expected_seq =
  let policy1 = prog_to_policy file1 in
  let policy2 = prog_to_policy file2 in
  let actual_seq = Planner.analyze policy1 policy2 in
  name >:: fun _ ->
  assert_equal expected_seq actual_seq ~printer:Planner.to_string

(* The planner's [Replace] idiom: at slot [path],
   [Designate(path) ; Quiesce(path ++ [0]) ; (Empty (path ++ [0])) Undesignate(path)].
   After [Designate] fires, the slot is a designated SP* with the loser at
   child index 0; [Quiesce] tears down the loser's class routing; [Undesignate]
   waits for the loser to drain before collapsing the SP* down to the
   survivor. When the replace also rebinds the slot's per-arm meta, a trailing
   [ChangeMeta] step under [True] fires immediately after [Undesignate]
   (sequential dependency carries the drain). *)
let replace_seq ?meta path arm =
  let loser_path = path @ [ 0 ] in
  let base =
    [
      (Planner.True, Delta.Designate { path; survivor = arm });
      (Planner.True, Delta.Quiesce loser_path);
      (Planner.Empty loser_path, Delta.Undesignate path);
    ]
  in
  match meta with
  | None -> base
  | Some m -> base @ [ (Planner.True, Delta.ChangeMeta { path; new_meta = m }) ]

(* The planner's [Retire] idiom: drain the subtree at [path], then structurally
   remove the slot once the subtree is empty. *)
let retire_seq path =
  [
    (Planner.True, Delta.Quiesce path); (Planner.Empty path, Delta.Remove path);
  ]

(* Sniffer-emitted give-up: walks [next] at [path] to recover the wholesale
   replacement arm. *)
let make_giveup_test name file1 file2 path =
  let policy2 = prog_to_policy file2 in
  let arm = Pol.walk policy2 path in
  make_planner_test name file1 file2 (replace_seq path arm)

let same =
  [
    make_planner_test "same program twice" "strict_ABC" "strict_ABC" [];
    make_planner_test "merely jumbled in RR" "rr_ABC" "rr_BAC" [];
    make_planner_test "merely jumbled in WFQ" "wfq_ABC" "wfq_ABC_jumbled" [];
    (* strict_AB_swapped has the same (arm, rank) pairs as strict_AB but
       lists them in swapped source order; [Pol.normalize]'s rank sort
       canonicalizes the two to the same shape. *)
    make_planner_test "merely source-swapped in SP" "strict_AB"
      "strict_AB_swapped" [];
  ]

(* [Add] fires on a single-arm insertion at any position of an RR/SP parent
   (after [Pol.normalize] has sorted RR children) or at any position of a
   WFQ parent. Multi-arm insertions instead give up to the wholesale-replace
   idiom (see [verydiff_combos]). *)
let one_arm_added =
  [
    make_planner_test "strict arm added at end" "strict_AB" "strict_ABC"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = Some 3.0 } );
      ];
    make_planner_test "strict arm added in the middle" "strict_AC" "strict_ABC"
      [
        ( Planner.True,
          Delta.Add { path = [ 1 ]; arm = Pol.FIFO "B"; meta = Some 2.0 } );
      ];
    make_planner_test "RR with arm added at end" "rr_AB" "rr_ABC"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = None } );
      ];
    make_planner_test "RR with arm added whilst reordering" "rr_AB" "rr_BAC"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = None } );
      ];
    make_planner_test "WFQ with arm added deep" "wfq_complex"
      "wfq_complex_add_arm_deep"
      [
        ( Planner.True,
          Delta.Add { path = [ 1; 2 ]; arm = Pol.FIFO "D"; meta = None } );
      ];
    make_planner_test "complex tree add arm deep" "complex_tree"
      "complex_tree_add_arm_deep"
      [
        ( Planner.True,
          Delta.Add { path = [ 1; 3 ]; arm = Pol.FIFO "NEW"; meta = None } );
      ];
  ]

let one_arm_added_wfq =
  [
    make_planner_test "WFQ with arm added at end" "wfq_BA" "wfq_ABC"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = Some 3.0 } );
      ];
    make_planner_test "complex tree fill in missing arm" "complex_tree_partial"
      "complex_tree"
      [
        ( Planner.True,
          Delta.Add
            {
              path = [ 1 ];
              arm = Pol.RR [ Pol.FIFO "D"; Pol.FIFO "E"; Pol.FIFO "F" ];
              meta = Some 2.0;
            } );
      ];
  ]

let armsremoved =
  [
    make_planner_test "RR with arm removed" "rr_ABC" "rr_AB" (retire_seq [ 2 ]);
    make_planner_test "WFQ with arm removed" "wfq_ABC" "wfq_BA"
      (retire_seq [ 2 ]);
    make_planner_test "complex tree remove arm deep" "complex_tree_add_arm_deep"
      "complex_tree"
      (retire_seq [ 1; 3 ]);
  ]

(* Multi-arm pure additions (no metas): every surplus arm in [next] becomes
   its own [Add] step, ascending. The arms of [prev] must appear in-order as
   a subsequence of [next]; surplus arms may cluster or skip apart. *)
let multi_arms_added =
  [
    make_planner_test "RR with two arms added at end" "rr_AB" "rr_ABCD"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = None } );
        ( Planner.True,
          Delta.Add { path = [ 3 ]; arm = Pol.FIFO "D"; meta = None } );
      ];
    make_planner_test "RR with two arms added non-consecutively" "rr_AC"
      "rr_ABCD"
      [
        ( Planner.True,
          Delta.Add { path = [ 1 ]; arm = Pol.FIFO "B"; meta = None } );
        ( Planner.True,
          Delta.Add { path = [ 3 ]; arm = Pol.FIFO "D"; meta = None } );
      ];
    (* rr_DBA_SP_CE normalizes to RR[A; B; D; SP[(C,1);(E,2)]]; against
       rr_AB = RR[A; B] this is two surplus arms at indices 2 and 3. *)
    make_planner_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "D"; meta = None } );
        ( Planner.True,
          Delta.Add
            {
              path = [ 3 ];
              arm = Pol.SP ([ (Pol.FIFO "C", 1.0); (Pol.FIFO "E", 2.0) ], false);
              meta = None;
            } );
      ];
  ]

(* Symmetric to [multi_arms_added]: each surplus arm in [prev] becomes its
   own retire, fired in descending index order so higher-index drains don't
   shift the lower paths still waiting to fire. *)
let multi_arms_removed =
  [
    make_planner_test "RR with two arms removed at end" "rr_ABCD" "rr_AB"
      (retire_seq [ 3 ] @ retire_seq [ 2 ]);
    make_planner_test "RR with two arms removed non-consecutively" "rr_ABCD"
      "rr_AC"
      (retire_seq [ 3 ] @ retire_seq [ 1 ]);
  ]

(* Multi-arm additions inside a metaed parent (WFQ/SP). The arm projection
   must witness [ps1]'s arms as a subsequence of [ps2]'s; each surplus arm
   becomes an [Add] carrying its meta. When every shared arm's meta also
   agrees the sequence is just the [Add]s; for the mixed case where a
   shared arm's meta differs, see [add_with_shared_meta_change]. *)
let multi_arms_added_metaed =
  [
    make_planner_test "WFQ with two arms added at end" "wfq_BA" "wfq_BACD"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = Some 3.0 } );
        ( Planner.True,
          Delta.Add { path = [ 3 ]; arm = Pol.FIFO "D"; meta = Some 4.0 } );
      ];
  ]

let multi_arms_removed_metaed =
  [
    make_planner_test "WFQ with two arms removed at end" "wfq_BACD" "wfq_BA"
      (retire_seq [ 3 ] @ retire_seq [ 2 ]);
  ]

let metachanged =
  [
    make_planner_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      [ (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 5.0 }) ];
    make_planner_test "two WFQ weights changed" "wfq_ABC" "wfq_ABC_two_weights"
      [
        (Planner.True, Delta.ChangeMeta { path = [ 0 ]; new_meta = 8.0 });
        (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 5.0 });
      ];
    (* Arms agree, every meta differs. wfq_ABC = (A,2),(B,1),(C,3);
       wfq_ABC_diff = (A,2),(B,2),(C,4) -- A unchanged, B and C bumped. *)
    make_planner_test "WFQ weights changed at two slots" "wfq_ABC"
      "wfq_ABC_diff"
      [
        (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 2.0 });
        (Planner.True, Delta.ChangeMeta { path = [ 2 ]; new_meta = 4.0 });
      ];
  ]

(* Slot-level replace (single-arm in-place divergence) expands into the
   give-up idiom at the affected slot. *)
let one_arm_replaced =
  [
    (* SP(A,B) vs SP(A,C): slot 1 differs in arm AND in rank (2.0 -> 3.0). *)
    make_planner_test "strict arm changed" "strict_AB" "strict_AC"
      (replace_seq ~meta:3.0 [ 1 ] (Pol.FIFO "C"));
    (* RR(A,B) vs RR(A,D): slot 1 differs in arm only. *)
    make_planner_test "rr arm changed" "rr_AB" "rr_AD"
      (replace_seq [ 1 ] (Pol.FIFO "D"));
    (* WFQ slot's arm changed in place, weight unchanged. *)
    make_planner_test "WFQ arm changed in place, same weight" "wfq_ABC"
      "wfq_ABZ"
      (replace_seq [ 2 ] (Pol.FIFO "Z"));
  ]

let one_arm_replaced_wfq =
  [
    (* WFQ slot's arm AND weight both flip in one edit. *)
    make_planner_test "WFQ slot with arm change and weight change" "wfq_ABC"
      "wfq_ABZ_diff"
      (replace_seq ~meta:7.0 [ 2 ] (Pol.FIFO "Z"));
  ]

(* Same-length metaed parent (WFQ/SP) where one or more slots have an arm
   change (with or without a meta change at the same slot). Each diverging
   slot emits its own slot-level edit independently; the per-slot emissions
   target distinct indices and concatenate in ascending order with no
   interference. Sub-cases by inner-recursion shape: a [Replace]-root inner
   takes any meta change as a trailing [ChangeMeta]; a non-bubbleable inner
   ([Graft]/[PruneDownTo]) demotes to a slot-level [Replace] (with meta if
   applicable); any clean smaller inner edit bubbles via [prepend_seq] with
   a separate [ChangeMeta] for the meta change. *)
let multi_arms_replaced_metaed =
  [
    make_planner_test "WFQ two slots with arm and weight changes" "wfq_ABC"
      "wfq_ADE"
      (replace_seq ~meta:5.0 [ 1 ] (Pol.FIFO "D")
      @ replace_seq ~meta:7.0 [ 2 ] (Pol.FIFO "E"));
    (* wfq_ABC = (A,2),(B,1),(C,3); wfq_DEF = (D,1),(E,2),(F,3). Slot 2's
       meta agrees, the other two slots flip both arm and meta. *)
    make_planner_test "different WFQ classes" "wfq_ABC" "wfq_DEF"
      (replace_seq ~meta:1.0 [ 0 ] (Pol.FIFO "D")
      @ replace_seq ~meta:2.0 [ 1 ] (Pol.FIFO "E")
      @ replace_seq [ 2 ] (Pol.FIFO "F"));
    (* SP rank swap: strict_AB = (A,1),(B,2); strict_BA = (B,1),(A,2).
       After [Pol.normalize]'s rank sort the two arms appear in swapped
       slots with the original ranks unchanged. *)
    make_planner_test "Strict with arms reordered" "strict_AB" "strict_BA"
      (replace_seq [ 0 ] (Pol.FIFO "B") @ replace_seq [ 1 ] (Pol.FIFO "A"));
    (* complex_tree's outer slot 0 differs (strict[A,B,C] vs strict[C,B,A])
       and the inner recursion itself returns a multi-slot Replace; the
       outer edit bubbles that inner sequence via [prepend_seq 0]. *)
    make_planner_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms"
      (replace_seq [ 0; 0 ] (Pol.FIFO "C") @ replace_seq [ 0; 2 ] (Pol.FIFO "A"));
    (* wfq_complex's slot 1 has both a deeper arm change (inner RR replaces
       a leaf) and a weight change. The inner sequence is itself a bubbled
       Replace, so the slot's edit becomes (bubbled inner) ++ ChangeMeta. *)
    make_planner_test "WFQ slot with deep diff and weight change" "wfq_complex"
      "wfq_complex_deep_and_weight"
      (replace_seq [ 1; 1 ] (Pol.FIFO "D")
      @ [ (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 5.0 }) ]);
  ]

(* Length-changing metaed parent where the arm projection embeds cleanly but
   a shared arm's meta differs. Emits [Add]s ascending (carrying the surplus
   arms' metas) followed by one [ChangeMeta] per shared arm whose meta
   differs (in [ps2]'s post-Add frame). *)
let add_with_shared_meta_change =
  [
    (* wfq_BA = (A,2),(B,1); wfq_ABC_diff = (A,2),(B,2),(C,4). Arm C is new
       (Add at index 2 with weight 4); the shared arm B's weight changes
       from 1 to 2 (ChangeMeta at index 1 in ps2's frame). *)
    make_planner_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff"
      [
        ( Planner.True,
          Delta.Add { path = [ 2 ]; arm = Pol.FIFO "C"; meta = Some 4.0 } );
        (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 2.0 });
      ];
  ]

let nested_giveup_demotion =
  [
    make_planner_test "nested Graft demotes to give-up" "strict_AB"
      "strict_A_rrBC"
      (replace_seq [ 1 ] (Pol.RR [ Pol.FIFO "B"; Pol.FIFO "C" ]));
    make_planner_test "nested ChangeRoot demotes to give-up" "strict_A_rrBC"
      "strict_AB"
      (replace_seq [ 1 ] (Pol.FIFO "B"));
  ]

let graft =
  [
    make_planner_test "fifo_A is sub-pol of complex_tree" "fifo_A"
      "complex_tree"
      [ (Planner.True, Delta.Graft [ 0; 0 ]) ];
    make_planner_test "strict_ABC is subpol of complex_tree" "strict_ABC"
      "complex_tree"
      [ (Planner.True, Delta.Graft [ 0 ]) ];
  ]

(* The planner's [PruneDownTo] idiom: retire each off-path sibling along the
   route to the surviving subtree (highest-index-within-level first,
   outer-level first), then re-root via [(True, ChangeRoot [0;...;0])]. *)
let change_root =
  [
    make_planner_test "complex_tree collapsed to fifo_A" "complex_tree" "fifo_A"
      (retire_seq [ 2 ] @ retire_seq [ 1 ]
      @ retire_seq [ 0; 2 ]
      @ retire_seq [ 0; 1 ]
      @ [ (Planner.True, Delta.ChangeRoot [ 0; 0 ]) ]);
    make_planner_test "complex_tree collapsed to strict_ABC" "complex_tree"
      "strict_ABC"
      (retire_seq [ 2 ] @ retire_seq [ 1 ]
      @ [ (Planner.True, Delta.ChangeRoot [ 0 ]) ]);
  ]

(* Combination cases where the diff is multiple legal edits at once; the
   planner gives up at the level of the divergence and expands the give-up
   idiom there. As the planner gets smarter (more idioms, longer sequences),
   entries here will migrate to more precise productions. *)
let verydiff_combos =
  [
    make_giveup_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" [];
    make_giveup_test "RR big diff" "rr_AB" "rr_DEF" [];
  ]

let suite =
  "planner tests"
  >::: same @ one_arm_added @ one_arm_added_wfq @ armsremoved @ multi_arms_added
       @ multi_arms_removed @ multi_arms_added_metaed
       @ multi_arms_removed_metaed @ metachanged @ one_arm_replaced
       @ one_arm_replaced_wfq @ multi_arms_replaced_metaed
       @ add_with_shared_meta_change @ verydiff_combos @ graft @ change_root
       @ nested_giveup_demotion

let () = run_test_tt_main suite
