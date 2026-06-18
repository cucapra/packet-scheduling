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

let metachanged =
  [
    make_planner_test "one WFQ weight changed" "wfq_ABC" "wfq_ABC_one_weight"
      [ (Planner.True, Delta.ChangeMeta { path = [ 1 ]; new_meta = 5.0 }) ];
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
    make_giveup_test "WFQ slot with deep diff and weight change" "wfq_complex"
      "wfq_complex_deep_and_weight" [];
    make_giveup_test "RR with two arms added whilst reordering" "rr_AB"
      "rr_DBA_SP_CE" [];
    make_giveup_test "strict arm added whilst reordering arms" "strict_BA"
      "strict_ABC" [];
    make_giveup_test "different WFQ classes" "wfq_ABC" "wfq_DEF" [];
    make_giveup_test "RR big diff" "rr_AB" "rr_DEF" [];
    make_giveup_test "WFQ with weights changed and arm added" "wfq_BA"
      "wfq_ABC_diff" [];
    make_giveup_test "Strict with arms reordered" "strict_AB" "strict_BA" [];
    make_giveup_test "complex tree with an SP reordering deep down"
      "complex_tree" "complex_tree_swap_sp_arms" [ 0 ];
    make_giveup_test "different WFQ weights" "wfq_ABC" "wfq_ABC_diff" [];
  ]

let suite =
  "planner tests"
  >::: same @ one_arm_added @ one_arm_added_wfq @ armsremoved @ metachanged
       @ one_arm_replaced @ one_arm_replaced_wfq @ verydiff_combos @ graft
       @ change_root @ nested_giveup_demotion

let () = run_test_tt_main suite
