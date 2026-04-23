open Frontend
open Ir
open OUnit2

let root_dir = "../../../../../"
let prog_dir = root_dir ^ "progs/"

(* [.sched] file -> pretty-printed IR string. *)
let compile_to_pretty filename =
  prog_dir ^ filename |> Parser.parse_file |> Policy.of_program |> Ir.of_policy
  |> string_of_program

(* ----------------------------------------------------------------------- *)
(* rr[A, B]                                                                *)
(* ----------------------------------------------------------------------- *)

(* Handwritten IR for rr[A, B] — mirrors what [Ir.of_policy] produces for
   [progs/work_conserving/rr_AB.sched] *)
let rr_ab_program : program =
  let root = 100 in
  let a_leaf = 101 in
  let b_leaf = 102 in
  let s_a = 1000 in
  let s_b = 1001 in
  let pe0 = 0 in
  let pe1 = 1 in
  [
    Spawn (root, pe0);
    Spawn (a_leaf, pe1);
    Spawn (b_leaf, pe1);
    Adopt (s_a, root, a_leaf);
    Adopt (s_b, root, b_leaf);
    Assoc (root, "A");
    Assoc (root, "B");
    Assoc (a_leaf, "A");
    Assoc (b_leaf, "B");
    Map (root, "A", s_a);
    Map (root, "B", s_b);
    Change_pol (root, RR, 2);
  ]

let expected_rr_ab =
  String.concat "\n"
    [
      "v100 = spawn(pe0)";
      "v101 = spawn(pe1)";
      "v102 = spawn(pe1)";
      "step_1000 = adopt(v100, v101)";
      "step_1001 = adopt(v100, v102)";
      "assoc(v100, A)";
      "assoc(v100, B)";
      "assoc(v101, A)";
      "assoc(v102, B)";
      "map(v100, A, step_1000)";
      "map(v100, B, step_1001)";
      "change_pol(v100, RR, 2)";
    ]

let test_rr_ab_handwritten =
  "rr[A, B] handwritten IR pretty-prints" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab (string_of_program rr_ab_program)

let test_rr_ab_pipeline =
  "rr[A, B] compiled from .sched matches the same golden" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_rr_ab
    (compile_to_pretty "work_conserving/rr_AB.sched")

(* ----------------------------------------------------------------------- *)
(* complex_tree                                                            *)
(* ----------------------------------------------------------------------- *)

(* Handwritten IR for complex_tree — mirrors what [Ir.of_policy] produces for
   [progs/work_conserving/complex_tree.sched]:

       classes A, B, C, D, E, F, G, H;
       left  = strict[A, B, C];
       mid   = rr[F, E, D];
       right = union[G, H];
       return wfq[(left, 1), (mid, 2), (right, 3)]

   After [Policy.normalize]:
   - [rr[F, E, D]]  sorts to  [rr[D, E, F]].
   - [union[G, H]]  is already sorted.
   - [strict[A, B, C]] is left as-is (SP children don't sort).
   - WFQ pairs sort by (child policy, weight) using OCaml's polymorphic
     [compare], which orders variants by declaration index in [Policy.t]:
     [FIFO < UNION < SP < RR < WFQ]. So the WFQ child order becomes
     [(union, 3); (sp, 1); (rr, 2)]. *)
let complex_tree_program : program =
  let pe0 = 0 in
  let pe1 = 1 in
  let pe2 = 2 in
  (* vPIFOs (pre-order, normalized child order). *)
  let wfq_root = 100 in
  let union_root = 101 in
  let g = 102 in
  let h = 103 in
  let sp_root = 104 in
  let a = 105 in
  let b = 106 in
  let c = 107 in
  let rr_root = 108 in
  let d = 109 in
  let e = 110 in
  let f = 111 in
  (* Step IDs (inner subtrees first, then the outer WFQ's adopts). *)
  let s_union_g = 1000 in
  let s_union_h = 1001 in
  let s_sp_a = 1002 in
  let s_sp_b = 1003 in
  let s_sp_c = 1004 in
  let s_rr_d = 1005 in
  let s_rr_e = 1006 in
  let s_rr_f = 1007 in
  let s_wfq_union = 1008 in
  let s_wfq_sp = 1009 in
  let s_wfq_rr = 1010 in
  [
    (* Spawns: WFQ root, then each subtree (root before its leaves), in the
       normalized order union → sp → rr. *)
    Spawn (wfq_root, pe0);
    Spawn (union_root, pe1);
    Spawn (g, pe2);
    Spawn (h, pe2);
    Spawn (sp_root, pe1);
    Spawn (a, pe2);
    Spawn (b, pe2);
    Spawn (c, pe2);
    Spawn (rr_root, pe1);
    Spawn (d, pe2);
    Spawn (e, pe2);
    Spawn (f, pe2);
    (* Adopts: outer WFQ's adopts first (local instructions come before the
       child fragments), then each inner subtree's adopts. *)
    Adopt (s_wfq_union, wfq_root, union_root);
    Adopt (s_wfq_sp, wfq_root, sp_root);
    Adopt (s_wfq_rr, wfq_root, rr_root);
    Adopt (s_union_g, union_root, g);
    Adopt (s_union_h, union_root, h);
    Adopt (s_sp_a, sp_root, a);
    Adopt (s_sp_b, sp_root, b);
    Adopt (s_sp_c, sp_root, c);
    Adopt (s_rr_d, rr_root, d);
    Adopt (s_rr_e, rr_root, e);
    Adopt (s_rr_f, rr_root, f);
    (* Assocs: WFQ root takes every class (in the order its children deliver
       them — union's classes first, then sp's, then rr's), then each subtree
       root takes its own arms, then each leaf takes its own. *)
    Assoc (wfq_root, "G");
    Assoc (wfq_root, "H");
    Assoc (wfq_root, "A");
    Assoc (wfq_root, "B");
    Assoc (wfq_root, "C");
    Assoc (wfq_root, "D");
    Assoc (wfq_root, "E");
    Assoc (wfq_root, "F");
    Assoc (union_root, "G");
    Assoc (union_root, "H");
    Assoc (g, "G");
    Assoc (h, "H");
    Assoc (sp_root, "A");
    Assoc (sp_root, "B");
    Assoc (sp_root, "C");
    Assoc (a, "A");
    Assoc (b, "B");
    Assoc (c, "C");
    Assoc (rr_root, "D");
    Assoc (rr_root, "E");
    Assoc (rr_root, "F");
    Assoc (d, "D");
    Assoc (e, "E");
    Assoc (f, "F");
    (* Maps: at the WFQ root, every class routes via the step that adopted
       its subtree; inner roots route each class to its leaf. *)
    Map (wfq_root, "G", s_wfq_union);
    Map (wfq_root, "H", s_wfq_union);
    Map (wfq_root, "A", s_wfq_sp);
    Map (wfq_root, "B", s_wfq_sp);
    Map (wfq_root, "C", s_wfq_sp);
    Map (wfq_root, "D", s_wfq_rr);
    Map (wfq_root, "E", s_wfq_rr);
    Map (wfq_root, "F", s_wfq_rr);
    Map (union_root, "G", s_union_g);
    Map (union_root, "H", s_union_h);
    Map (sp_root, "A", s_sp_a);
    Map (sp_root, "B", s_sp_b);
    Map (sp_root, "C", s_sp_c);
    Map (rr_root, "D", s_rr_d);
    Map (rr_root, "E", s_rr_e);
    Map (rr_root, "F", s_rr_f);
    (* Policies. *)
    Change_pol (wfq_root, WFQ, 3);
    Change_pol (union_root, UNION, 2);
    Change_pol (sp_root, SP, 3);
    Change_pol (rr_root, RR, 3);
    (* Weights: WFQ's per-arm weights (in the normalized child order
       union/sp/rr → 3.0/1.0/2.0), then SP's priorities (1.0 = highest). *)
    Change_weight (wfq_root, s_wfq_union, 3.0);
    Change_weight (wfq_root, s_wfq_sp, 1.0);
    Change_weight (wfq_root, s_wfq_rr, 2.0);
    Change_weight (sp_root, s_sp_a, 1.0);
    Change_weight (sp_root, s_sp_b, 2.0);
    Change_weight (sp_root, s_sp_c, 3.0);
  ]

let expected_complex_tree =
  String.concat "\n"
    [
      "v100 = spawn(pe0)";
      "v101 = spawn(pe1)";
      "v102 = spawn(pe2)";
      "v103 = spawn(pe2)";
      "v104 = spawn(pe1)";
      "v105 = spawn(pe2)";
      "v106 = spawn(pe2)";
      "v107 = spawn(pe2)";
      "v108 = spawn(pe1)";
      "v109 = spawn(pe2)";
      "v110 = spawn(pe2)";
      "v111 = spawn(pe2)";
      "step_1008 = adopt(v100, v101)";
      "step_1009 = adopt(v100, v104)";
      "step_1010 = adopt(v100, v108)";
      "step_1000 = adopt(v101, v102)";
      "step_1001 = adopt(v101, v103)";
      "step_1002 = adopt(v104, v105)";
      "step_1003 = adopt(v104, v106)";
      "step_1004 = adopt(v104, v107)";
      "step_1005 = adopt(v108, v109)";
      "step_1006 = adopt(v108, v110)";
      "step_1007 = adopt(v108, v111)";
      "assoc(v100, G)";
      "assoc(v100, H)";
      "assoc(v100, A)";
      "assoc(v100, B)";
      "assoc(v100, C)";
      "assoc(v100, D)";
      "assoc(v100, E)";
      "assoc(v100, F)";
      "assoc(v101, G)";
      "assoc(v101, H)";
      "assoc(v102, G)";
      "assoc(v103, H)";
      "assoc(v104, A)";
      "assoc(v104, B)";
      "assoc(v104, C)";
      "assoc(v105, A)";
      "assoc(v106, B)";
      "assoc(v107, C)";
      "assoc(v108, D)";
      "assoc(v108, E)";
      "assoc(v108, F)";
      "assoc(v109, D)";
      "assoc(v110, E)";
      "assoc(v111, F)";
      "map(v100, G, step_1008)";
      "map(v100, H, step_1008)";
      "map(v100, A, step_1009)";
      "map(v100, B, step_1009)";
      "map(v100, C, step_1009)";
      "map(v100, D, step_1010)";
      "map(v100, E, step_1010)";
      "map(v100, F, step_1010)";
      "map(v101, G, step_1000)";
      "map(v101, H, step_1001)";
      "map(v104, A, step_1002)";
      "map(v104, B, step_1003)";
      "map(v104, C, step_1004)";
      "map(v108, D, step_1005)";
      "map(v108, E, step_1006)";
      "map(v108, F, step_1007)";
      "change_pol(v100, WFQ, 3)";
      "change_pol(v101, UNION, 2)";
      "change_pol(v104, SP, 3)";
      "change_pol(v108, RR, 3)";
      "change_weight(v100, step_1008, 3.)";
      "change_weight(v100, step_1009, 1.)";
      "change_weight(v100, step_1010, 2.)";
      "change_weight(v104, step_1002, 1.)";
      "change_weight(v104, step_1003, 2.)";
      "change_weight(v104, step_1004, 3.)";
    ]

let test_complex_tree_handwritten =
  "complex_tree handwritten IR pretty-prints" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_complex_tree
    (string_of_program complex_tree_program)

let test_complex_tree_pipeline =
  "complex_tree compiled from .sched matches the same golden" >:: fun _ ->
  assert_equal ~printer:Fun.id expected_complex_tree
    (compile_to_pretty "work_conserving/complex_tree.sched")

let suite =
  "ir printer tests"
  >::: [
         test_rr_ab_handwritten;
         test_rr_ab_pipeline;
         test_complex_tree_handwritten;
         test_complex_tree_pipeline;
       ]

let () = run_test_tt_main suite
