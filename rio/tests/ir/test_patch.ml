open Rio_core
open Frontend
open OUnit2
open Ir
module Planner = Rio_planner.Planner

let prog_dir = "../progs/work_conserving/"

let policy_of file =
  prog_dir ^ file ^ ".sched" |> Parser.parse_file |> Pol.of_program

let compile file = Ir.of_policy (policy_of file)

(* Patch [prev_file] toward [next_file]'s policy and return the [compiled]
   delta. Fails the test if [patch] gives up. *)
let patch_files prev_file next_file =
  let prev = compile prev_file in
  let next = policy_of next_file in
  match Ir.patch ~prev ~next with
  | Some c -> c
  | None ->
      assert_failure
        (Printf.sprintf "patch %s -> %s returned None" prev_file next_file)

let make_delta_test name prev_file next_file
    (expected : (Planner.guard * commit) list) =
  name >:: fun _ ->
  let c = patch_files prev_file next_file in
  assert_equal ~printer:Ir.string_of_steps expected c.steps

(* Brief constructors for guarded-step literals, just to keep fixtures readable. *)
let t_ c = (Planner.True, c)
let e_ p c = (Planner.Empty p, c)

(* Extract the commit from a single-step result. Used by the per-production
   helper tests ([patch_designate], [patch_quiesce], [patch_undesignate])
   that exercise a single lowering directly. *)
let single_commit c =
  match c.steps with
  | [ (_, commit) ] -> commit
  | _ -> assert_failure "single_commit: expected exactly one step"

(* Add *)

(* There's a walkthrough for this case in the topmatter of the PR. *)
let strict_ab_to_abc_expected =
  [
    t_
      [
        Spawn (103, 1);
        Adopt (1002, 100, 103);
        Assoc (99, "C");
        Assoc (100, "C");
        Assoc (103, "C");
        Map (99, "C", 999);
        Map (100, "C", 1002);
        Change_arity (100, 3);
        Set_arm_meta (100, 1002, 3.0);
      ];
  ]

(* Mid-insert in SP: prev SP(A,C) parsed with positional ranks A:1, C:2.
   After inserting B at index 1, prev's existing ranks are preserved (no
   positional cascade); only the new arm B gets a Set_arm_meta. B's rank
   comes from [next]'s positional sugar (1-indexed source position 2),
   which here collides with C's preserved rank — a known sharp edge of
   bare positional sugar across mid-mutations. *)
let strict_ac_to_abc_expected =
  [
    t_
      [
        Spawn (103, 1);
        Adopt (1002, 100, 103);
        Assoc (99, "B");
        Assoc (100, "B");
        Assoc (103, "B");
        Map (99, "B", 999);
        Map (100, "B", 1002);
        Change_arity (100, 3);
        Set_arm_meta (100, 1002, 2.0);
      ];
  ]

(* RR arm appended at the root. Same shape as SP but no Set_arm_meta. *)
let rr_ab_to_abc_expected =
  [
    t_
      [
        Spawn (103, 1);
        Adopt (1002, 100, 103);
        Assoc (99, "C");
        Assoc (100, "C");
        Assoc (103, "C");
        Map (99, "C", 999);
        Map (100, "C", 1002);
        Change_arity (100, 3);
      ];
  ]

(* Deep arm add. complex_tree's normalized root is WFQ with children sorted
   to (SP, RR[D,E,F], RR[G,H]) at indices 0, 1, 2. The mid RR (at path [1])
   has parent pifo 105 and arity 3; complex_tree leaves the counters at
   next_pifo=112, next_step=1011. New FIFO NEW lives one level below the
   RR, so PE 2. *)
let complex_tree_add_deep_expected =
  [
    t_
      [
        Spawn (112, 2);
        Adopt (1011, 105, 112);
        Assoc (99, "NEW");
        Assoc (100, "NEW");
        Assoc (105, "NEW");
        Assoc (112, "NEW");
        Map (99, "NEW", 999);
        Map (100, "NEW", 1009);
        Map (105, "NEW", 1011);
        Change_arity (105, 4);
      ];
  ]

let one_arm_added_tests =
  [
    make_delta_test "strict[A,B] -> strict[A,B,C]" "strict_AB" "strict_ABC"
      strict_ab_to_abc_expected;
    make_delta_test "strict[A,C] -> strict[A,B,C]" "strict_AC" "strict_ABC"
      strict_ac_to_abc_expected;
    make_delta_test "rr[A,B] -> rr[A,B,C]" "rr_AB" "rr_ABC"
      rr_ab_to_abc_expected;
    make_delta_test "complex_tree -> complex_tree_add_arm_deep" "complex_tree"
      "complex_tree_add_arm_deep" complex_tree_add_deep_expected;
  ]

(* Add (WFQ parent) *)

(* wfq_BA compiles with pifos 100 (root WFQ), 101 (A), 102 (B); steps 1000
   (A), 1001 (B). Adding FIFO C at slot 2 with weight 3 mints v=103 (PE 1)
   and step 1002, plus a single Set_arm_meta on the new step (no SP-style
   shifts: WFQ slots are independent). *)
let wfq_ba_to_abc_expected =
  [
    t_
      [
        Spawn (103, 1);
        Adopt (1002, 100, 103);
        Assoc (99, "C");
        Assoc (100, "C");
        Assoc (103, "C");
        Map (99, "C", 999);
        Map (100, "C", 1002);
        Change_arity (100, 3);
        Set_arm_meta (100, 1002, 3.0);
      ];
  ]

(* complex_tree_partial: WFQ at root with two children, SP[A,B,C] (weight 1)
   and RR[G,H] (weight 3). After normalize: pairs sort by arm; SP < RR by
   constructor tag, giving children at indices 0, 1.
     v=100 root WFQ; v=101 SP; v=102/103/104 A/B/C; v=105 RR[G,H];
     v=106/107 G/H. Steps: 1000/1001/1002 (SP -> A/B/C), 1003/1004
     (RR-GH -> G/H), 1005 (root -> SP), 1006 (root -> RR-GH). pes=[0;1;2].
   Patch adds the RR[D,E,F] subtree with weight 2 at slot 1 (between SP and
   RR-GH after normalize): arm internals land on v=108 (RR, PE 1) plus
   109/110/111 (D/E/F, PE 2); arm-internal adopts on steps 1007/1008/1009.
   The new parent-to-child step is 1010, with Set_arm_meta(100, 1010, 2.0). *)
let complex_tree_partial_to_full_expected =
  [
    t_
      [
        Spawn (108, 1);
        Spawn (109, 2);
        Spawn (110, 2);
        Spawn (111, 2);
        Adopt (1010, 100, 108);
        Adopt (1007, 108, 109);
        Adopt (1008, 108, 110);
        Adopt (1009, 108, 111);
        Assoc (99, "D");
        Assoc (99, "E");
        Assoc (99, "F");
        Assoc (100, "D");
        Assoc (100, "E");
        Assoc (100, "F");
        Assoc (108, "D");
        Assoc (108, "E");
        Assoc (108, "F");
        Assoc (109, "D");
        Assoc (110, "E");
        Assoc (111, "F");
        Map (99, "D", 999);
        Map (99, "E", 999);
        Map (99, "F", 999);
        Map (100, "D", 1010);
        Map (100, "E", 1010);
        Map (100, "F", 1010);
        Map (108, "D", 1007);
        Map (108, "E", 1008);
        Map (108, "F", 1009);
        Set_policy (108, RR, 3);
        Change_arity (100, 3);
        Set_arm_meta (100, 1010, 2.0);
      ];
  ]

let one_arm_added_wfq_tests =
  [
    make_delta_test "wfq[B,A] -> wfq[A,B,C]" "wfq_BA" "wfq_ABC"
      wfq_ba_to_abc_expected;
    make_delta_test "complex_tree_partial -> complex_tree"
      "complex_tree_partial" "complex_tree"
      complex_tree_partial_to_full_expected;
  ]

(* ChangeMeta *)

(* WFQ root with three FIFO arms: pifo IDs 100 (root), 101/102/103 (A/B/C);
   adopt steps 1000/1001/1002. Bumping B's weight 1 -> 5 should emit a single
   Set_arm_meta on the root for B's step. *)
let meta_changed_tests =
  [
    make_delta_test "wfq[A,B,C] -> wfq[A,B(5),C]" "wfq_ABC" "wfq_ABC_one_weight"
      [ t_ [ Set_arm_meta (100, 1001, 5.0) ] ];
  ]

(* Remove. The planner expands as the [Retire] idiom
   [Quiesce ; (Empty) Remove]; [Ir.patch]'s fold lowers them in order.
   [Quiesce] tears down class routing for the dropped class along the full
   chain (port root + every internal ancestor); [Remove] then emits the
   structural teardown ([Change_arity] shrink, parent-side [Emancipate], and
   [GC] of the removed subtree). *)

(* SP[A,B,C] -> SP[A,B]: drop C (last, index 2). No SP weight shifts.
   Two guarded steps: [Quiesce] under [True] tears down class C's routing on
   the chain; [Remove] under [Empty [2]] waits for the subtree to drain
   before emitting the structural teardown. *)
let strict_abc_to_ab_expected =
  [
    t_ [ Deassoc (99, "C"); Deassoc (100, "C"); Deassoc (103, "C") ];
    e_ [ 2 ]
      [
        Change_arity (100, 2);
        Emancipate (1002, 100);
        Unmap (99, "C");
        Unmap (100, "C");
        GC 103;
      ];
  ]

(* SP[A,B,C] -> SP[A,C]: drop B (mid, index 1). Existing SP arms keep their
   ranks (per paper's [init_slot_Strict(_, p) = p]); no Set_arm_meta is
   emitted for surviving siblings. *)
let strict_abc_to_ac_expected =
  [
    t_ [ Deassoc (99, "B"); Deassoc (100, "B"); Deassoc (102, "B") ];
    e_ [ 1 ]
      [
        Change_arity (100, 2);
        Emancipate (1001, 100);
        Unmap (99, "B");
        Unmap (100, "B");
        GC 102;
      ];
  ]

(* RR[A,B,C] -> RR[A,B]: drop C from RR. No weight shifts (RR is unweighted). *)
let rr_abc_to_ab_expected =
  [
    t_ [ Deassoc (99, "C"); Deassoc (100, "C"); Deassoc (103, "C") ];
    e_ [ 2 ]
      [
        Change_arity (100, 2);
        Emancipate (1002, 100);
        Unmap (99, "C");
        Unmap (100, "C");
        GC 103;
      ];
  ]

let one_arm_removed_tests =
  [
    make_delta_test "strict[A,B,C] -> strict[A,B]" "strict_ABC" "strict_AB"
      strict_abc_to_ab_expected;
    make_delta_test "strict[A,B,C] -> strict[A,C]" "strict_ABC" "strict_AC"
      strict_abc_to_ac_expected;
    make_delta_test "rr[A,B,C] -> rr[A,B]" "rr_ABC" "rr_AB"
      rr_abc_to_ab_expected;
  ]

(* Replace. The planner expands as the [Replace] idiom
   [Designate(p) ; Quiesce(p ++ [0]) ; (Empty (p ++ [0])) Undesignate(p)]:
   - [Designate] mints a fresh [SP_star] super-node ([sp_v]) on the loser's
     PE; the loser becomes child 0 of [sp_v], the freshly compiled survivor
     becomes child 1. The parent edge rewires from loser to [sp_v]; [sp_v]
     routes loser-side classes via [loser_step] (favored) and survivor-only
     classes via [surv_step].
   - [Quiesce] tears down loser-only routing along the chain above [sp_v]
     and at [sp_v] itself; in-flight packets continue to dequeue.
   - [Undesignate] (gated on the loser being empty) emits [Undesignate loser_v]
     as the §6 ISA marker and GCs [sp_v] + the loser subtree. The parent
     rewire from [sp_v] back to the survivor is implicit in the ISA marker
     (paper §6.1), so no explicit [Emancipate]/[Adopt] is emitted.
   When the same edit also rebinds the slot's per-arm meta, a trailing
   [ChangeMeta]-only step under [True] fires immediately after. *)

(* RR[A,B] -> RR[A,D]: replace B (pifo 102, step 1001) with FIFO D. New IDs
   mint as: v=103 for the survivor (FIFO D), v=104 for sp_v, step_1002 for
   loser_step (sp_v -> loser), step_1003 for surv_step (sp_v -> survivor). All
   three nodes (v102, v103, v104) live on PE 1. The parent edge step_1001
   rewires to point at v104; chain above gets [Assoc]/[Map] entries for the
   new class D. Loser-only class B drains: chain above unmaps B (v99 / v100)
   and v104 unmaps + deassocs B internally. Once empty, [Undesignate] emits
   [Undesignate v102] as the §6 marker and GCs v104 (sp_v) and v102 (loser);
   the parent rewire of step_1001 from v104 to v103 is implicit in the marker
   (paper §6.1). *)
let rr_ab_to_ad_expected =
  [
    t_
      [
        Spawn (103, 1);
        Assoc (103, "D");
        Spawn (104, 1);
        Adopt (1002, 104, 102);
        Adopt (1003, 104, 103);
        Designate (102, 103);
        Emancipate (1001, 100);
        Adopt (1001, 100, 104);
        Assoc (104, "B");
        Assoc (104, "D");
        Map (104, "B", 1002);
        Map (104, "D", 1003);
        Assoc (99, "D");
        Assoc (100, "D");
        Map (99, "D", 999);
        Map (100, "D", 1001);
        Set_policy (104, SP_star, 2);
        Set_arm_meta (104, 1002, 1.0);
        Set_arm_meta (104, 1003, 2.0);
      ];
    t_
      [
        Deassoc (99, "B");
        Deassoc (100, "B");
        Deassoc (104, "B");
        Deassoc (102, "B");
      ];
    e_ [ 1; 0 ] [ Undesignate 102; GC 104; GC 102 ];
  ]

(* SP[A,B] -> SP[A,C]: arm-swap at slot 1, AND the slot's rank changes
   (B was rank 2 in strict_AB; C is rank 3 in strict_AC). A trailing
   [ChangeMeta] step under [True] fires immediately after [Undesignate]. *)
let strict_ab_to_ac_expected =
  [
    t_
      [
        Spawn (103, 1);
        Assoc (103, "C");
        Spawn (104, 1);
        Adopt (1002, 104, 102);
        Adopt (1003, 104, 103);
        Designate (102, 103);
        Emancipate (1001, 100);
        Adopt (1001, 100, 104);
        Assoc (104, "B");
        Assoc (104, "C");
        Map (104, "B", 1002);
        Map (104, "C", 1003);
        Assoc (99, "C");
        Assoc (100, "C");
        Map (99, "C", 999);
        Map (100, "C", 1001);
        Set_policy (104, SP_star, 2);
        Set_arm_meta (104, 1002, 1.0);
        Set_arm_meta (104, 1003, 2.0);
      ];
    t_
      [
        Deassoc (99, "B");
        Deassoc (100, "B");
        Deassoc (104, "B");
        Deassoc (102, "B");
      ];
    e_ [ 1; 0 ] [ Undesignate 102; GC 104; GC 102 ];
    t_ [ Set_arm_meta (100, 1001, 3.0) ];
  ]

let one_arm_replaced_tests =
  [
    make_delta_test "rr[A,B] -> rr[A,D]" "rr_AB" "rr_AD" rr_ab_to_ad_expected;
    make_delta_test "strict[A,B] -> strict[A,C]" "strict_AB" "strict_AC"
      strict_ab_to_ac_expected;
  ]

(* Replace (WFQ parent). wfq_ABC has root WFQ v=100 with leaves v=101/102/103 for
   A/B/C on steps 1000/1001/1002. Replacing slot 2's arm (C -> FIFO Z) *and*
   its weight (3 -> 7) at path [2]: the survivor FIFO Z spawns on v=104, sp_v
   is v=105, loser_step is step_1003, surv_step is step_1004. All three of
   v103/v104/v105 live on PE 1. After [Quiesce] drains C, [Undesignate] emits
   its §6 marker and GCs v105 + the loser subtree; step_1002's rewire from
   v105 to v104 is implicit in the marker. The slot's new weight is set in a
   trailing [True]-guarded [ChangeMeta] step. *)
let wfq_abc_to_abz_diff_expected =
  [
    t_
      [
        Spawn (104, 1);
        Assoc (104, "Z");
        Spawn (105, 1);
        Adopt (1003, 105, 103);
        Adopt (1004, 105, 104);
        Designate (103, 104);
        Emancipate (1002, 100);
        Adopt (1002, 100, 105);
        Assoc (105, "C");
        Assoc (105, "Z");
        Map (105, "C", 1003);
        Map (105, "Z", 1004);
        Assoc (99, "Z");
        Assoc (100, "Z");
        Map (99, "Z", 999);
        Map (100, "Z", 1002);
        Set_policy (105, SP_star, 2);
        Set_arm_meta (105, 1003, 1.0);
        Set_arm_meta (105, 1004, 2.0);
      ];
    t_
      [
        Deassoc (99, "C");
        Deassoc (100, "C");
        Deassoc (105, "C");
        Deassoc (103, "C");
      ];
    e_ [ 2; 0 ] [ Undesignate 103; GC 105; GC 103 ];
    t_ [ Set_arm_meta (100, 1002, 7.0) ];
  ]

let one_arm_replaced_wfq_tests =
  [
    make_delta_test "wfq[A,B,C] -> wfq[A,B,Z(7)]" "wfq_ABC" "wfq_ABZ_diff"
      wfq_abc_to_abz_diff_expected;
  ]

(* Whole-tree replacement (planner expands as the Replace idiom at
   path []). prev = rr[A,B] (pifos 100/101/102, steps 1000/1001);
   next = rr[D,E,F] which shares no arms. The patch emits three steps:
   - Designate: builds rr[D,E,F] off fresh ids (root v=103 on PE 0,
     leaves 104/105/106 on PE 1; steps 1002/1003/1004), spawns
     sp_v=107 on PE 0, adopts loser (v=100) and survivor (v=103)
     under it, rewires the port root edge from v=100 to sp_v, and
     installs routing at sp_v plus chain-above for the survivor's
     new classes.
   - Quiesce ([0]): tears down loser-only routing above and at sp_v.
     No shared classes, so no remap at sp_v.
   - Empty[0] Undesignate: emits the §6 marker and GCs sp_v + the loser
     subtree; the port root's swing from sp_v to the survivor is implicit
     in the marker. *)
let rr_ab_to_rr_def_expected =
  [
    t_
      [
        Spawn (103, 0);
        Spawn (104, 1);
        Spawn (105, 1);
        Spawn (106, 1);
        Adopt (1002, 103, 104);
        Adopt (1003, 103, 105);
        Adopt (1004, 103, 106);
        Assoc (103, "D");
        Assoc (103, "E");
        Assoc (103, "F");
        Assoc (104, "D");
        Assoc (105, "E");
        Assoc (106, "F");
        Map (103, "D", 1002);
        Map (103, "E", 1003);
        Map (103, "F", 1004);
        Set_policy (103, RR, 3);
        Spawn (107, 0);
        Adopt (1005, 107, 100);
        Adopt (1006, 107, 103);
        Designate (100, 103);
        Emancipate (999, 99);
        Adopt (999, 99, 107);
        Assoc (107, "A");
        Assoc (107, "B");
        Assoc (107, "D");
        Assoc (107, "E");
        Assoc (107, "F");
        Map (107, "A", 1005);
        Map (107, "B", 1005);
        Map (107, "D", 1006);
        Map (107, "E", 1006);
        Map (107, "F", 1006);
        Assoc (99, "D");
        Assoc (99, "E");
        Assoc (99, "F");
        Map (99, "D", 999);
        Map (99, "E", 999);
        Map (99, "F", 999);
        Set_policy (107, SP_star, 2);
        Set_arm_meta (107, 1005, 1.0);
        Set_arm_meta (107, 1006, 2.0);
      ];
    t_
      [
        Deassoc (99, "A");
        Deassoc (99, "B");
        Deassoc (107, "A");
        Deassoc (107, "B");
        Deassoc (100, "A");
        Deassoc (100, "B");
        Deassoc (101, "A");
        Deassoc (102, "B");
      ];
    e_ [ 0 ] [ Undesignate 100; GC 107; GC 100; GC 101; GC 102 ];
  ]

(* Whole-tree replacement via constructor mismatch at the root: prev =
   sp[A,B] (pifos 100/101/102), next = rr[A,B]: same children,
   different root policy. The class sets coincide ({A,B}={A,B}) so the
   port root's existing routing is preserved (no chain-above edits).
   Designate routes both shared classes through surv_step at sp_v=106
   (paper §3.4.5: shared classes belong to the survivor from Designate's
   firing onward), and Quiesce's SP*-aware branch has no loser-only work
   to do at sp_v: only the loser interior drains. *)
let strict_ab_to_rr_ab_expected =
  [
    t_
      [
        Spawn (103, 0);
        Spawn (104, 1);
        Spawn (105, 1);
        Adopt (1002, 103, 104);
        Adopt (1003, 103, 105);
        Assoc (103, "A");
        Assoc (103, "B");
        Assoc (104, "A");
        Assoc (105, "B");
        Map (103, "A", 1002);
        Map (103, "B", 1003);
        Set_policy (103, RR, 2);
        Spawn (106, 0);
        Adopt (1004, 106, 100);
        Adopt (1005, 106, 103);
        Designate (100, 103);
        Emancipate (999, 99);
        Adopt (999, 99, 106);
        Assoc (106, "A");
        Assoc (106, "B");
        Map (106, "A", 1005);
        Map (106, "B", 1005);
        Set_policy (106, SP_star, 2);
        Set_arm_meta (106, 1004, 1.0);
        Set_arm_meta (106, 1005, 2.0);
      ];
    t_
      [
        Deassoc (100, "A");
        Deassoc (100, "B");
        Deassoc (101, "A");
        Deassoc (102, "B");
      ];
    e_ [ 0 ] [ Undesignate 100; GC 106; GC 100; GC 101; GC 102 ];
  ]

let whole_tree_replace_tests =
  [
    make_delta_test "rr[A,B] -> rr[D,E,F] (whole-tree replace)" "rr_AB" "rr_DEF"
      rr_ab_to_rr_def_expected;
    make_delta_test "strict[A,B] -> rr[A,B] (root constructor change)"
      "strict_AB" "rr_AB" strict_ab_to_rr_ab_expected;
  ]

(* ChangeRoot. The planner expands re-rooting as the [PruneDownTo] idiom: a
   sequence of [Retire]s on off-path siblings along the route, followed by a
   final [ChangeRoot] that GC's whatever ancestors remain. *)

(* SP[A,B,C] -> FIFO A (target path [0]). Two off-path Retires fire first
   (highest-index-within-level first): [Retire([2]) C] then [Retire([1]) B],
   each emitting [Quiesce]'s class-routing teardown across the chain and
   [Remove]'s structural teardown. Then [ChangeRoot([0])] swings the port
   root's single step from v100 to v101 (the FIFO A leaf, sole remaining
   child) and GC's the now-empty SP root v100. The dropped-class block on
   the port root is empty by this point because [Quiesce] already drained B
   and C; the only [GC] left for [ChangeRoot] is v100 itself. *)
let strict_abc_to_fifo_a_expected =
  [
    (* Retire([2]) C: Quiesce then Remove. *)
    t_ [ Deassoc (99, "C"); Deassoc (100, "C"); Deassoc (103, "C") ];
    e_ [ 2 ]
      [
        Change_arity (100, 2);
        Emancipate (1002, 100);
        Unmap (99, "C");
        Unmap (100, "C");
        GC 103;
      ];
    (* Retire([1]) B: Quiesce then Remove. *)
    t_ [ Deassoc (99, "B"); Deassoc (100, "B"); Deassoc (102, "B") ];
    e_ [ 1 ]
      [
        Change_arity (100, 1);
        Emancipate (1001, 100);
        Unmap (99, "B");
        Unmap (100, "B");
        GC 102;
      ];
    (* ChangeRoot([0]): swing the port root from v100 to v101 and GC the SP
       root. Dropped-class block is empty (B and C already drained). *)
    t_ [ Emancipate (999, 99); Adopt (999, 99, 101); GC 100 ];
  ]

let change_root_tests =
  [
    make_delta_test "strict[A,B,C] -> fifo[A]" "strict_ABC" "fifo_A"
      strict_abc_to_fifo_a_expected;
  ]

(* prev embedded in next (root-level Replace via Designate) *)

(* prev = strict[A,B,C] compiles to pifos 100 (root)/101/102/103 with
   adopt steps 1000/1001/1002 and pes [0; 1]; counters end at next_v=104,
   next_s=1003. complex_tree normalizes to
   wfq[(sp[A,B,C], 1); (rr[D,E,F], 2); (rr[G,H], 3)] (children sort by
   constructor tag SP < RR, and rr[D,E,F] < rr[G,H] by element compare),
   so prev sits at path [0] inside next. Before Graft was removed this
   was the canonical Graft case; now the planner falls back to the
   root-level [Replace] idiom via [Designate].

   The delta compiles the new tree freshly: WFQ root v=104 on PE 0, then
   the three children (sp[A,B,C], rr[D,E,F], rr[G,H]) and their leaves
   on PEs 1 and 2 in normalized preorder. Once the new tree is wired,
   the [Designate] wrapper v=116 (SP_star) is spawned on PE 0, adopts
   loser v100 and survivor v104 under it, [Designate]s v100 -> v104, and
   the port root emancipates from v100 and re-adopts onto v116. The
   wrapper carries every class (A..H) mapped to step 1015 (the survivor
   side), since all loser classes A,B,C are shared with the survivor.
   The port root gains [Assoc]/[Map] entries for the five classes that
   [complex_tree] adds beyond [strict_ABC] (D, E, F, G, H).

   The second [True] frame tears down the loser's internal routing:
   [Deassoc] for A, B, C at v100 and at the corresponding leaves
   v101/v102/v103. No port-root or wrapper deassocs fire because the
   loser class set is a subset of the survivor's.

   The final [Empty [0]] frame fires once the wrapper's loser child has
   drained: [Undesignate v100] swings the port root from the wrapper to
   the survivor, then GCs the wrapper and the loser subtree. *)
let strict_abc_to_complex_tree_expected =
  [
    t_
      [
        Spawn (104, 0);
        Spawn (105, 1);
        Spawn (106, 2);
        Spawn (107, 2);
        Spawn (108, 2);
        Spawn (109, 1);
        Spawn (110, 2);
        Spawn (111, 2);
        Spawn (112, 2);
        Spawn (113, 1);
        Spawn (114, 2);
        Spawn (115, 2);
        Adopt (1011, 104, 105);
        Adopt (1012, 104, 109);
        Adopt (1013, 104, 113);
        Adopt (1003, 105, 106);
        Adopt (1004, 105, 107);
        Adopt (1005, 105, 108);
        Adopt (1006, 109, 110);
        Adopt (1007, 109, 111);
        Adopt (1008, 109, 112);
        Adopt (1009, 113, 114);
        Adopt (1010, 113, 115);
        Assoc (104, "A");
        Assoc (104, "B");
        Assoc (104, "C");
        Assoc (104, "D");
        Assoc (104, "E");
        Assoc (104, "F");
        Assoc (104, "G");
        Assoc (104, "H");
        Assoc (105, "A");
        Assoc (105, "B");
        Assoc (105, "C");
        Assoc (106, "A");
        Assoc (107, "B");
        Assoc (108, "C");
        Assoc (109, "D");
        Assoc (109, "E");
        Assoc (109, "F");
        Assoc (110, "D");
        Assoc (111, "E");
        Assoc (112, "F");
        Assoc (113, "G");
        Assoc (113, "H");
        Assoc (114, "G");
        Assoc (115, "H");
        Map (104, "A", 1011);
        Map (104, "B", 1011);
        Map (104, "C", 1011);
        Map (104, "D", 1012);
        Map (104, "E", 1012);
        Map (104, "F", 1012);
        Map (104, "G", 1013);
        Map (104, "H", 1013);
        Map (105, "A", 1003);
        Map (105, "B", 1004);
        Map (105, "C", 1005);
        Map (109, "D", 1006);
        Map (109, "E", 1007);
        Map (109, "F", 1008);
        Map (113, "G", 1009);
        Map (113, "H", 1010);
        Set_policy (104, WFQ, 3);
        Set_policy (105, SP, 3);
        Set_policy (109, RR, 3);
        Set_policy (113, RR, 2);
        Set_arm_meta (104, 1011, 1.0);
        Set_arm_meta (104, 1012, 2.0);
        Set_arm_meta (104, 1013, 3.0);
        Set_arm_meta (105, 1003, 1.0);
        Set_arm_meta (105, 1004, 2.0);
        Set_arm_meta (105, 1005, 3.0);
        Spawn (116, 0);
        Adopt (1014, 116, 100);
        Adopt (1015, 116, 104);
        Designate (100, 104);
        Emancipate (999, 99);
        Adopt (999, 99, 116);
        Assoc (116, "A");
        Assoc (116, "B");
        Assoc (116, "C");
        Assoc (116, "D");
        Assoc (116, "E");
        Assoc (116, "F");
        Assoc (116, "G");
        Assoc (116, "H");
        Map (116, "A", 1015);
        Map (116, "B", 1015);
        Map (116, "C", 1015);
        Map (116, "D", 1015);
        Map (116, "E", 1015);
        Map (116, "F", 1015);
        Map (116, "G", 1015);
        Map (116, "H", 1015);
        Assoc (99, "D");
        Assoc (99, "E");
        Assoc (99, "F");
        Assoc (99, "G");
        Assoc (99, "H");
        Map (99, "D", 999);
        Map (99, "E", 999);
        Map (99, "F", 999);
        Map (99, "G", 999);
        Map (99, "H", 999);
        Set_policy (116, SP_star, 2);
        Set_arm_meta (116, 1014, 1.0);
        Set_arm_meta (116, 1015, 2.0);
      ];
    t_
      [
        Deassoc (100, "A");
        Deassoc (100, "B");
        Deassoc (100, "C");
        Deassoc (101, "A");
        Deassoc (102, "B");
        Deassoc (103, "C");
      ];
    e_ [ 0 ] [ Undesignate 100; GC 116; GC 100; GC 101; GC 102; GC 103 ];
  ]

let prev_embedded_in_next_tests =
  [
    ( "strict[A,B,C] -> complex_tree (pes invariant)" >:: fun _ ->
      let c = patch_files "strict_ABC" "complex_tree" in
      assert_equal
        ~printer:(fun pes ->
          "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
        [ 0; 1; 2 ] c.pes );
    make_delta_test "strict[A,B,C] -> complex_tree" "strict_ABC" "complex_tree"
      strict_abc_to_complex_tree_expected;
  ]

(* pes-extension regressions: when an [Add] or [Replace] inserts
   an arm whose internal depth pushes the tree deeper than [prev.pes]
   covered, [pes_extended_to_depth] should grow [pes] with fresh PEs above
   the existing max — keeping the "same depth ⇒ same PE" invariant for
   the new layers. No fixture pair fits this shape, so we build the
   policies inline. *)

(* prev = sp[A] (depth 1, pes [0; 1]); next = sp[A, rr[B, C]] (depth 2).
   The new arm at index 1 is rr[B, C]. arm_depth = 1, internal depth = 1,
   so the tree reaches depth 2 — one below prev's max. New layer takes
   PE 2 (fresh, above prev's max PE 1). *)
let one_arm_added_extends_pes_test =
  "sp[A] -> sp[A, rr[B,C]] (extends pes)" >:: fun _ ->
  let prev = Ir.of_policy (Pol.SP ([ (Pol.FIFO "A", 1.0) ], false)) in
  let next =
    Pol.SP
      ( [ (Pol.FIFO "A", 1.0); (Pol.RR [ Pol.FIFO "B"; Pol.FIFO "C" ], 2.0) ],
        false )
  in
  let c =
    match Ir.patch ~prev ~next with
    | Some c -> c
    | None -> assert_failure "patch returned None"
  in
  assert_equal
    ~printer:(fun pes ->
      "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
    [ 0; 1; 2 ] c.pes;
  let expected =
    [
      t_
        [
          Spawn (102, 1);
          Spawn (103, 2);
          Spawn (104, 2);
          Adopt (1003, 100, 102);
          Adopt (1001, 102, 103);
          Adopt (1002, 102, 104);
          Assoc (99, "B");
          Assoc (99, "C");
          Assoc (100, "B");
          Assoc (100, "C");
          Assoc (102, "B");
          Assoc (102, "C");
          Assoc (103, "B");
          Assoc (104, "C");
          Map (99, "B", 999);
          Map (99, "C", 999);
          Map (100, "B", 1003);
          Map (100, "C", 1003);
          Map (102, "B", 1001);
          Map (102, "C", 1002);
          Set_policy (102, RR, 2);
          Change_arity (100, 2);
          Set_arm_meta (100, 1003, 2.0);
        ];
    ]
  in
  assert_equal ~printer:Ir.string_of_steps expected c.steps

let pes_extension_tests = [ one_arm_added_extends_pes_test ]

(* Deep give-up: complex_tree -> complex_tree_swap_sp_arms differs only
   inside the SP subtree at root child 1 (multi-arm reorder). Delta
   gives up at that level and emits the [Replace] idiom at [path = [1]];
   IR's existing [Replace] handler routes through [Designate] on the
   parent's existing step. Smoke-test that patch returns [Some] — the
   exact instruction sequence is exercised by the precise [Replace]
   tests at non-empty paths above. *)
let deep_giveup_test =
  "complex_tree -> swap_sp_arms (deep give-up)" >:: fun _ ->
  let prev = compile "complex_tree" in
  let next = policy_of "complex_tree_swap_sp_arms" in
  match Ir.patch ~prev ~next with
  | Some _ -> ()
  | None -> assert_failure "patch returned None for deep give-up"

let deep_giveup_tests = [ deep_giveup_test ]

(* ------------------------------------------------------------------ *)
(* Planner-only Delta productions: Designate / Quiesce / Undesignate.
   [analyze] never emits these; the lowering helpers are called
   directly. Each test pins down the ISA shape so a future planner has a
   stable target.                                                     *)
(* ------------------------------------------------------------------ *)

(* Designate at a leaf slot of rr[A,B]: introduce a new FIFO D as the
   survivor against the existing FIFO B (loser v=102, parent edge
   step_1001 from v=100). A fresh sp_v=104 is minted (same PE as the
   loser), and adopts the loser at step_1002 and survivor at step_1003.
   The parent edge is rewired from loser to sp_v. Routing entries at
   sp_v send loser classes via step_1002 and survivor-only classes via
   step_1003. Class D, new to the tree, also gets routed down the chain
   from port root to the slot's parent. *)
let designate_arm_test =
  "Designate: rr[A,B] gets D introduced at slot 1" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let c = Ir.patch_designate ~prev ~path:[ 1 ] ~arm in
  let expected : commit =
    [
      Spawn (103, 1);
      Assoc (103, "D");
      Spawn (104, 1);
      Adopt (1002, 104, 102);
      Adopt (1003, 104, 103);
      Designate (102, 103);
      Emancipate (1001, 100);
      Adopt (1001, 100, 104);
      Assoc (104, "B");
      Assoc (104, "D");
      Map (104, "B", 1002);
      Map (104, "D", 1003);
      Assoc (99, "D");
      Assoc (100, "D");
      Map (99, "D", 999);
      Map (100, "D", 1001);
      Set_policy (104, SP_star, 2);
      Set_arm_meta (104, 1002, 1.0);
      Set_arm_meta (104, 1003, 2.0);
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* Designate at the root of rr[A,B]: the whole tree gets designated
   against a new FIFO D. Only the port root sits above; the chain
   shortens to just [(99, 999)]. sp_v=104 spawns on PE 0 (same as the
   old root v=100), adopts loser at step_1002 and survivor at step_1003,
   and the port root edge step_999 is repointed from v=100 to v=104. *)
let designate_root_test =
  "Designate: rr[A,B] whole-tree against FIFO D" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let c = Ir.patch_designate ~prev ~path:[] ~arm in
  let expected : commit =
    [
      Spawn (103, 0);
      Assoc (103, "D");
      Spawn (104, 0);
      Adopt (1002, 104, 100);
      Adopt (1003, 104, 103);
      Designate (100, 103);
      Emancipate (999, 99);
      Adopt (999, 99, 104);
      Assoc (104, "A");
      Assoc (104, "B");
      Assoc (104, "D");
      Map (104, "A", 1002);
      Map (104, "B", 1002);
      Map (104, "D", 1003);
      Assoc (99, "D");
      Map (99, "D", 999);
      Set_policy (104, SP_star, 2);
      Set_arm_meta (104, 1002, 1.0);
      Set_arm_meta (104, 1003, 2.0);
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* Quiesce slot 1 of rr[A,B]: tear down routing for class B along the
   chain from the port root down to the slot's parent, then Deassoc the
   FIFO B leaf itself. den(Quiesce) = id, so no shape change to the
   tree. *)
let quiesce_arm_test =
  "Quiesce: rr[A,B] at slot 1 (drains B)" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_quiesce ~prev ~path:[ 1 ] in
  let expected : commit =
    [ Deassoc (99, "B"); Deassoc (100, "B"); Deassoc (102, "B") ]
  in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* Quiesce at root: Deassocs every leaf class at every node from the port
   root down through the rr root to its leaves. The whole tree is being
   drained. *)
let quiesce_root_test =
  "Quiesce: rr[A,B] whole-tree" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_quiesce ~prev ~path:[] in
  let expected : commit =
    [
      Deassoc (99, "A");
      Deassoc (99, "B");
      Deassoc (100, "A");
      Deassoc (100, "B");
      Deassoc (101, "A");
      Deassoc (102, "B");
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* Undesignate at slot 1 of a previously-designated rr[A,B]: the slot holds
   sp_v=104 with loser v=102 (FIFO B) at child 0 and survivor v=103 (FIFO D) at
   child 1. Collapsing emits [Undesignate 102] as the ISA marker and GCs sp_v
   plus the loser subtree; the parent rewire from sp_v to v=103 is implicit in
   [Isa_undesignate] (paper §6.1). *)
let undesignate_arm_test =
  "Undesignate: rr[A,B] at slot 1" >:: fun _ ->
  let prev = compile "rr_AB" in
  let designated = Ir.patch_designate ~prev ~path:[ 1 ] ~arm:(Pol.FIFO "D") in
  let c = Ir.patch_undesignate ~prev:designated ~path:[ 1 ] in
  let expected : commit = [ Undesignate 102; GC 104; GC 102 ] in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* Undesignate at the root of a previously-designated rr[A,B]: parent is the
   port root v=99 (step 999). loser_v=100 is the old RR root, with FIFO A
   (v=101) and FIFO B (v=102) underneath. GC walks the whole loser subtree;
   the port root's rewire from sp_v to the survivor v=103 is implicit in
   [Isa_undesignate]. *)
let undesignate_root_test =
  "Undesignate: rr[A,B] at root" >:: fun _ ->
  let prev = compile "rr_AB" in
  let designated = Ir.patch_designate ~prev ~path:[] ~arm:(Pol.FIFO "D") in
  let c = Ir.patch_undesignate ~prev:designated ~path:[] in
  let expected : commit = [ Undesignate 100; GC 104; GC 100; GC 101; GC 102 ] in
  assert_equal ~printer:Ir.string_of_commit expected (single_commit c)

(* End-to-end give-up idiom: Replace([], FIFO D) expands as
   Designate([]) ; Quiesce([0]) ; Undesignate([]). The Quiesce path is
   [0] because, post-Designate, the loser sits at child 0 inside the
   freshly minted sp_v=104. Each lowering threads the post-mutation
   [compiled] forward so Quiesce sees the SP* tree. *)
let giveup_idiom_test =
  "give-up idiom: Designate ; Quiesce ; Undesignate at root" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let d = Ir.patch_designate ~prev ~path:[] ~arm in
  let q = Ir.patch_quiesce ~prev:d ~path:[ 0 ] in
  let u = Ir.patch_undesignate ~prev:q ~path:[] in
  let combined = single_commit d @ single_commit q @ single_commit u in
  let expected : commit =
    [
      Spawn (103, 0);
      Assoc (103, "D");
      Spawn (104, 0);
      Adopt (1002, 104, 100);
      Adopt (1003, 104, 103);
      Designate (100, 103);
      Emancipate (999, 99);
      Adopt (999, 99, 104);
      Assoc (104, "A");
      Assoc (104, "B");
      Assoc (104, "D");
      Map (104, "A", 1002);
      Map (104, "B", 1002);
      Map (104, "D", 1003);
      Assoc (99, "D");
      Map (99, "D", 999);
      Set_policy (104, SP_star, 2);
      Set_arm_meta (104, 1002, 1.0);
      Set_arm_meta (104, 1003, 2.0);
      Deassoc (99, "A");
      Deassoc (99, "B");
      Deassoc (104, "A");
      Deassoc (104, "B");
      Deassoc (100, "A");
      Deassoc (100, "B");
      Deassoc (101, "A");
      Deassoc (102, "B");
      Undesignate 100;
      GC 104;
      GC 100;
      GC 101;
      GC 102;
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected combined

let planner_production_tests =
  [
    designate_arm_test;
    designate_root_test;
    quiesce_arm_test;
    quiesce_root_test;
    undesignate_arm_test;
    undesignate_root_test;
    giveup_idiom_test;
  ]

let suite =
  "patch tests"
  >::: one_arm_added_tests @ one_arm_added_wfq_tests @ meta_changed_tests
       @ one_arm_removed_tests @ one_arm_replaced_tests
       @ one_arm_replaced_wfq_tests @ whole_tree_replace_tests
       @ change_root_tests @ prev_embedded_in_next_tests @ pes_extension_tests
       @ deep_giveup_tests @ planner_production_tests

let () = run_test_tt_main suite
