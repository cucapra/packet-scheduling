open Rio_core
open Frontend
open OUnit2
open Ir

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

let make_delta_test name prev_file next_file (expected : commit) =
  name >:: fun _ ->
  let c = patch_files prev_file next_file in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

(* ArmAdded *)

(* There's a walkthrough for this case in the topmatter of the PR. *)
let strict_ab_to_abc_expected : commit =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "C");
    Assoc (103, "C");
    Map (100, "C", 1002);
    Change_arity (100, 3);
    Set_arm_meta (100, 1002, 3.0);
  ]

(* Mid-insert in SP: prev SP(A,C) parsed with positional ranks A:1, C:2.
   After inserting B at index 1, prev's existing ranks are preserved (no
   positional cascade); only the new arm B gets a Set_arm_meta. B's rank
   comes from [next]'s positional sugar (1-indexed source position 2),
   which here collides with C's preserved rank — a known sharp edge of
   bare positional sugar across mid-mutations. *)
let strict_ac_to_abc_expected : commit =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "B");
    Assoc (103, "B");
    Map (100, "B", 1002);
    Change_arity (100, 3);
    Set_arm_meta (100, 1002, 2.0);
  ]

(* RR arm appended at the root. Same shape as SP but no Set_arm_meta. *)
let rr_ab_to_abc_expected : commit =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "C");
    Assoc (103, "C");
    Map (100, "C", 1002);
    Change_arity (100, 3);
  ]

(* Deep arm add. complex_tree's normalized root is WFQ with children sorted
   to (SP, RR[D,E,F], RR[G,H]) at indices 0, 1, 2. The mid RR (at path [1])
   has parent vpifo 105 and arity 3; complex_tree leaves the counters at
   next_vpifo=112, next_step=1011. New FIFO NEW lives one level below the
   RR, so PE 2. *)
let complex_tree_add_deep_expected : commit =
  [
    Spawn (112, 2);
    Adopt (1011, 105, 112);
    Assoc (100, "NEW");
    Assoc (105, "NEW");
    Assoc (112, "NEW");
    Map (100, "NEW", 1009);
    Map (105, "NEW", 1011);
    Change_arity (105, 4);
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

(* ArmAddedWFQ *)

(* wfq_BA compiles with vpifos 100 (root WFQ), 101 (A), 102 (B); steps 1000
   (A), 1001 (B). Adding FIFO C at slot 2 with weight 3 mints v=103 (PE 1)
   and step 1002, plus a single Set_arm_meta on the new step (no SP-style
   shifts: WFQ slots are independent). *)
let wfq_ba_to_abc_expected : commit =
  [
    Spawn (103, 1);
    Adopt (1002, 100, 103);
    Assoc (100, "C");
    Assoc (103, "C");
    Map (100, "C", 1002);
    Change_arity (100, 3);
    Set_arm_meta (100, 1002, 3.0);
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
let complex_tree_partial_to_full_expected : commit =
  [
    Spawn (108, 1);
    Spawn (109, 2);
    Spawn (110, 2);
    Spawn (111, 2);
    Adopt (1010, 100, 108);
    Adopt (1007, 108, 109);
    Adopt (1008, 108, 110);
    Adopt (1009, 108, 111);
    Assoc (100, "D");
    Assoc (100, "E");
    Assoc (100, "F");
    Assoc (108, "D");
    Assoc (108, "E");
    Assoc (108, "F");
    Assoc (109, "D");
    Assoc (110, "E");
    Assoc (111, "F");
    Map (100, "D", 1010);
    Map (100, "E", 1010);
    Map (100, "F", 1010);
    Map (108, "D", 1007);
    Map (108, "E", 1008);
    Map (108, "F", 1009);
    Set_policy (108, RR, 3);
    Change_arity (100, 3);
    Set_arm_meta (100, 1010, 2.0);
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

(* WFQ root with three FIFO arms: vpifo IDs 100 (root), 101/102/103 (A/B/C);
   adopt steps 1000/1001/1002. Bumping B's weight 1 -> 5 should emit a single
   Set_arm_meta on the root for B's step. *)
let wfq_abc_to_one_weight_expected : commit = [ Set_arm_meta (100, 1001, 5.0) ]

let meta_changed_tests =
  [
    make_delta_test "wfq[A,B,C] -> wfq[A,B(5),C]" "wfq_ABC" "wfq_ABC_one_weight"
      wfq_abc_to_one_weight_expected;
  ]

(* ArmRemoved *)

(* SP[A,B,C] -> SP[A,B]: drop C (last, index 2). No SP weight shifts. *)
let strict_abc_to_ab_expected : commit =
  [
    Change_arity (100, 2);
    Unmap (100, "C", 1002);
    Deassoc (100, "C");
    Emancipate (1002, 100, 103);
    GC 103;
  ]

(* SP[A,B,C] -> SP[A,C]: drop B (mid, index 1). Existing SP arms keep their
   ranks (per paper's [init_slot_Strict(_, p) = p]); no Set_arm_meta is
   emitted for surviving siblings. *)
let strict_abc_to_ac_expected : commit =
  [
    Change_arity (100, 2);
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Emancipate (1001, 100, 102);
    GC 102;
  ]

(* RR[A,B,C] -> RR[A,B]: drop C from RR. No weight shifts (RR is unweighted). *)
let rr_abc_to_ab_expected : commit =
  [
    Change_arity (100, 2);
    Unmap (100, "C", 1002);
    Deassoc (100, "C");
    Emancipate (1002, 100, 103);
    GC 103;
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

(* ArmReplaced *)

(* RR[A,B] -> RR[A,D]: replace B (vpifo 102, step 1001) with FIFO D. The
   new arm rides on step 1001; B's super-node is set up via Designate(102,
   103). Ancestor routing on root vpifo 100 shifts B → D (Unmap/Deassoc
   then Assoc/Map). GC marks 102. *)
let rr_ab_to_ad_expected : commit =
  [
    Spawn (103, 1);
    Assoc (103, "D");
    Designate (102, 103);
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Assoc (100, "D");
    Map (100, "D", 1001);
    Undesignate 102;
    GC 102;
  ]

(* SP[A,B] -> SP[A,C]: arm-swap at slot 1, AND the slot's rank changes
   (B was rank 2 in strict_AB; C is rank 3 in strict_AC). The slot's
   meta gets a Set_arm_meta after the Designate/Undesignate flow. *)
let strict_ab_to_ac_expected : commit =
  [
    Spawn (103, 1);
    Assoc (103, "C");
    Designate (102, 103);
    Unmap (100, "B", 1001);
    Deassoc (100, "B");
    Assoc (100, "C");
    Map (100, "C", 1001);
    Undesignate 102;
    GC 102;
    Set_arm_meta (100, 1001, 3.0);
  ]

let one_arm_replaced_tests =
  [
    make_delta_test "rr[A,B] -> rr[A,D]" "rr_AB" "rr_AD" rr_ab_to_ad_expected;
    make_delta_test "strict[A,B] -> strict[A,C]" "strict_AB" "strict_AC"
      strict_ab_to_ac_expected;
  ]

(* ArmReplacedWFQ. wfq_ABC has root WFQ v=100 with leaves v=101/102/103
   for A/B/C on steps 1000/1001/1002. Replacing slot 2's arm (C → FIFO Z)
   *and* its weight (3 → 7) reuses step 1002: the new FIFO Z spawns on
   v=104 (PE 1), is [Designate]d onto v=103, ancestor routing on v=100
   swings C → Z, the slot's weight gets a single [Set_arm_meta], and
   v=103 is GC'd. *)
let wfq_abc_to_abz_diff_expected : commit =
  [
    Spawn (104, 1);
    Assoc (104, "Z");
    Designate (103, 104);
    Unmap (100, "C", 1002);
    Deassoc (100, "C");
    Assoc (100, "Z");
    Map (100, "Z", 1002);
    Undesignate 103;
    GC 103;
    Set_arm_meta (100, 1002, 7.0);
  ]

let one_arm_replaced_wfq_tests =
  [
    make_delta_test "wfq[A,B,C] -> wfq[A,B,Z(7)]" "wfq_ABC" "wfq_ABZ_diff"
      wfq_abc_to_abz_diff_expected;
  ]

(* Whole-tree replacement (Delta returns [VeryDifferent []]). prev =
   rr[A,B] (vpifos 100/101/102, steps 1000/1001); next = rr[D,E,F] which
   shares no arms, so Delta gives up at the root. The patch builds the
   new rr[D,E,F] off fresh ids (root v=103 on PE 0, leaves 104/105/106
   on PE 1; steps 1002/1003/1004), [Designate]s the old root (100) with
   the new root (103) so the port root's single step drains old before
   servicing new, and rewrites the port root's classifier from
   {A,B} → {D,E,F} all riding on [port_root_step]. GCs cover every prev
   vpifo. *)
let rr_ab_to_rr_def_expected : commit =
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
    Designate (100, 103);
    Unmap (99, "A", 999);
    Unmap (99, "B", 999);
    Deassoc (99, "A");
    Deassoc (99, "B");
    Assoc (99, "D");
    Assoc (99, "E");
    Assoc (99, "F");
    Map (99, "D", 999);
    Map (99, "E", 999);
    Map (99, "F", 999);
    Undesignate 100;
    GC 100;
    GC 101;
    GC 102;
  ]

(* Whole-tree replacement via constructor mismatch at the root (Delta
   returns [ArmReplaced { path = []; arm = RR[A,B] }]). prev =
   sp[A,B] (vpifos 100/101/102, steps 1000/1001); next = rr[A,B] —
   same children, different root policy, so Delta can't ride the
   existing slots. Same handler as the [VeryDifferent []] case above:
   builds the new tree off fresh ids, [Designate]s, drains. The class
   sets coincide here so the port root's existing routing is preserved
   and no {Unmap,Deassoc,Assoc,Map} land on it. *)
let strict_ab_to_rr_ab_expected : commit =
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
    Designate (100, 103);
    Undesignate 100;
    GC 100;
    GC 101;
    GC 102;
  ]

let whole_tree_replace_tests =
  [
    make_delta_test "rr[A,B] -> rr[D,E,F] (whole-tree replace)" "rr_AB" "rr_DEF"
      rr_ab_to_rr_def_expected;
    make_delta_test "strict[A,B] -> rr[A,B] (root constructor change)"
      "strict_AB" "rr_AB" strict_ab_to_rr_ab_expected;
  ]

(* ChangeRoot *)

(* SP[A, B, C] -> FIFO A. The FIFO leaf already lives inside prev as v101,
   adopted via step_1000 from the root v100. Re-rooting swings the port
   root's single step from v100 to v101 via [Emancipate]/[Adopt]; no
   [Emancipate] is emitted for v101's old parent edge because v100 is
   among the GC'd nodes, which severs the edge. The dropped classes B and
   C are [Unmap]/[Deassoc]'d off the port root, and the SP root v100 along
   with v102/v103 are GC'd. *)
let strict_abc_to_fifo_a_expected : commit =
  [
    Emancipate (999, 99, 100);
    Adopt (999, 99, 101);
    Unmap (99, "B", 999);
    Unmap (99, "C", 999);
    Deassoc (99, "B");
    Deassoc (99, "C");
    GC 100;
    GC 102;
    GC 103;
  ]

let change_root_tests =
  [
    make_delta_test "strict[A,B,C] -> fifo[A]" "strict_ABC" "fifo_A"
      strict_abc_to_fifo_a_expected;
  ]

(* Graft *)

(* prev = strict[A,B,C] compiles to vpifos 100 (root)/101/102/103 with
   adopt steps 1000/1001/1002 and pes [0; 1]; counters end at next_v=104,
   next_s=1003. complex_tree normalizes to
   wfq[(sp[A,B,C], 1); (rr[D,E,F], 2); (rr[G,H], 3)] (children sort by
   constructor tag SP < RR, and rr[D,E,F] < rr[G,H] by element compare),
   so prev sits at path [0]. The delta builds the new WFQ root (v=104,
   PE 2; fresh, above prev's max PE 1), the RR[D,E,F] sibling (v=105 on
   PE 0, leaves v=106/107/108 on PE 1, internal adopt steps 1003/1004/1005)
   and the RR[G,H] sibling (v=109 on PE 0, leaves v=110/111 on PE 1,
   internal adopt steps 1006/1007), then [Adopt]s the WFQ root's three
   children via steps 1008 (prev's root v=100), 1009 (RR-DEF), 1010
   (RR-GH). Each ancestor [Assoc]/[Map]s every class in its subtree; WFQ
   weights mirror the (pol, weight) sort order: 1, 2, 3. Final
   [Emancipate]/[Adopt] on the port root swings its single step from
   prev's old real root v100 to the new top v104, and the port root gains
   [Assoc]/[Map] entries on its single step for the five classes that
   [complex_tree] adds beyond [strict_ABC] (D, E, F, G, H in the
   normalized preorder); the three carried-over classes (A, B, C) keep
   their existing port-root wiring. None of prev's vpifos are respawned. *)
let strict_abc_to_complex_tree_expected : commit =
  [
    Spawn (104, 2);
    Spawn (105, 0);
    Spawn (106, 1);
    Spawn (107, 1);
    Spawn (108, 1);
    Spawn (109, 0);
    Spawn (110, 1);
    Spawn (111, 1);
    Adopt (1008, 104, 100);
    Adopt (1009, 104, 105);
    Adopt (1010, 104, 109);
    Adopt (1003, 105, 106);
    Adopt (1004, 105, 107);
    Adopt (1005, 105, 108);
    Adopt (1006, 109, 110);
    Adopt (1007, 109, 111);
    Assoc (104, "A");
    Assoc (104, "B");
    Assoc (104, "C");
    Assoc (104, "D");
    Assoc (104, "E");
    Assoc (104, "F");
    Assoc (104, "G");
    Assoc (104, "H");
    Assoc (105, "D");
    Assoc (105, "E");
    Assoc (105, "F");
    Assoc (106, "D");
    Assoc (107, "E");
    Assoc (108, "F");
    Assoc (109, "G");
    Assoc (109, "H");
    Assoc (110, "G");
    Assoc (111, "H");
    Map (104, "A", 1008);
    Map (104, "B", 1008);
    Map (104, "C", 1008);
    Map (104, "D", 1009);
    Map (104, "E", 1009);
    Map (104, "F", 1009);
    Map (104, "G", 1010);
    Map (104, "H", 1010);
    Map (105, "D", 1003);
    Map (105, "E", 1004);
    Map (105, "F", 1005);
    Map (109, "G", 1006);
    Map (109, "H", 1007);
    Set_policy (104, WFQ, 3);
    Set_policy (105, RR, 3);
    Set_policy (109, RR, 2);
    Set_arm_meta (104, 1008, 1.0);
    Set_arm_meta (104, 1009, 2.0);
    Set_arm_meta (104, 1010, 3.0);
    Emancipate (999, 99, 100);
    Adopt (999, 99, 104);
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
  ]

let graft_tests =
  [
    ( "strict[A,B,C] -> complex_tree (pes invariant)" >:: fun _ ->
      let c = patch_files "strict_ABC" "complex_tree" in
      assert_equal
        ~printer:(fun pes ->
          "[" ^ String.concat "; " (List.map string_of_int pes) ^ "]")
        [ 2; 0; 1 ] c.pes );
    make_delta_test "strict[A,B,C] -> complex_tree" "strict_ABC" "complex_tree"
      strict_abc_to_complex_tree_expected;
  ]

(* pes-extension regressions: when a ArmAdded or ArmReplaced inserts
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
  let expected : commit =
    [
      Spawn (102, 1);
      Spawn (103, 2);
      Spawn (104, 2);
      Adopt (1003, 100, 102);
      Adopt (1001, 102, 103);
      Adopt (1002, 102, 104);
      Assoc (100, "B");
      Assoc (100, "C");
      Assoc (102, "B");
      Assoc (102, "C");
      Assoc (103, "B");
      Assoc (104, "C");
      Map (100, "B", 1003);
      Map (100, "C", 1003);
      Map (102, "B", 1001);
      Map (102, "C", 1002);
      Set_policy (102, RR, 2);
      Change_arity (100, 2);
      Set_arm_meta (100, 1003, 2.0);
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

let pes_extension_tests = [ one_arm_added_extends_pes_test ]

(* Deep give-up: complex_tree -> complex_tree_swap_sp_arms differs only
   inside the SP subtree at root child 1 (multi-arm reorder). Delta
   gives up at that level and emits [ArmReplaced { path = [1]; arm }];
   IR's existing ArmReplaced handler routes through [Designate] on the
   parent's existing step. Smoke-test that patch returns [Some] — the
   exact instruction sequence is exercised by the precise ArmReplaced
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
   survivor against the existing FIFO B (v=102, step 1001). [Set_policy
   (102, SP*, 2)] is the substrate-facing marker that v=102 now hosts a
   super-node. Class D, new to the tree, gets routed all the way down
   the chain from port root to the slot's parent. *)
let designate_arm_test =
  "Designate: rr[A,B] gets D introduced at slot 1" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let c = Ir.patch_designate ~prev ~path:[ 1 ] ~arm in
  let expected : commit =
    [
      Spawn (103, 1);
      Assoc (103, "D");
      Designate (102, 103);
      Set_policy (102, SP_star, 2);
      Assoc (99, "D");
      Assoc (100, "D");
      Map (99, "D", 999);
      Map (100, "D", 1001);
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

(* Designate at the root of rr[A,B]: the whole tree gets designated
   against a new FIFO D. Only the port root sits above; the chain
   shortens to just [(99, 999)]. *)
let designate_root_test =
  "Designate: rr[A,B] whole-tree against FIFO D" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let c = Ir.patch_designate ~prev ~path:[] ~arm in
  let expected : commit =
    [
      Spawn (103, 0);
      Assoc (103, "D");
      Designate (100, 103);
      Set_policy (100, SP_star, 2);
      Assoc (99, "D");
      Map (99, "D", 999);
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

(* Quiesce slot 1 of rr[A,B]: tear down routing for class B along the
   chain from the port root down to the slot's parent. den(Quiesce) = id,
   so no shape change to the tree. *)
let quiesce_arm_test =
  "Quiesce: rr[A,B] at slot 1 (drains B)" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_quiesce ~prev ~path:[ 1 ] in
  let expected : commit =
    [
      Unmap (99, "B", 999);
      Unmap (100, "B", 1001);
      Deassoc (99, "B");
      Deassoc (100, "B");
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

(* Quiesce at root: tears down routing for every class on the port root.
   The whole tree is being drained. *)
let quiesce_root_test =
  "Quiesce: rr[A,B] whole-tree" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_quiesce ~prev ~path:[] in
  let expected : commit =
    [
      Unmap (99, "A", 999);
      Unmap (99, "B", 999);
      Deassoc (99, "A");
      Deassoc (99, "B");
    ]
  in
  assert_equal ~printer:Ir.string_of_commit expected c.commit

(* Undesignate: emit the bare ISA instruction targeting the loser v at
   the slot. The substrate handles the rewire. *)
let undesignate_arm_test =
  "Undesignate: rr[A,B] at slot 1" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_undesignate ~prev ~path:[ 1 ] in
  assert_equal ~printer:Ir.string_of_commit [ Undesignate 102 ] c.commit

let undesignate_root_test =
  "Undesignate: rr[A,B] at root" >:: fun _ ->
  let prev = compile "rr_AB" in
  let c = Ir.patch_undesignate ~prev ~path:[] in
  assert_equal ~printer:Ir.string_of_commit [ Undesignate 100 ] c.commit

(* End-to-end give-up idiom (sketch.md §4): Replace([], p2) expands to
   Designate([], p2) ; (true ; Quiesce([0])) ; (empty([0]) ; Undesignate([])).
   This test stitches the three lowerings in sequence and checks the
   combined commit shape. The Quiesce path is [0] because, post-Designate,
   the loser sits at index 0 inside the conceptual super-node. We can't
   walk that path through our decorated tree (we don't model super-nodes
   yet), so we quiesce at [] instead: equivalent here since the loser is
   the entire tree. *)
let giveup_idiom_test =
  "give-up idiom: Designate ; Quiesce ; Undesignate at root" >:: fun _ ->
  let prev = compile "rr_AB" in
  let arm = Pol.FIFO "D" in
  let d = Ir.patch_designate ~prev ~path:[] ~arm in
  let q = Ir.patch_quiesce ~prev ~path:[] in
  let u = Ir.patch_undesignate ~prev ~path:[] in
  let combined = d.commit @ q.commit @ u.commit in
  let expected : commit =
    [
      Spawn (103, 0);
      Assoc (103, "D");
      Designate (100, 103);
      Set_policy (100, SP_star, 2);
      Assoc (99, "D");
      Map (99, "D", 999);
      Unmap (99, "A", 999);
      Unmap (99, "B", 999);
      Deassoc (99, "A");
      Deassoc (99, "B");
      Undesignate 100;
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
       @ change_root_tests @ graft_tests @ pes_extension_tests
       @ deep_giveup_tests @ planner_production_tests

let () = run_test_tt_main suite
