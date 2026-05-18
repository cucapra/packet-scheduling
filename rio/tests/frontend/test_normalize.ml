open Rio_core
open Frontend
open OUnit2

let prog_dir = "../progs/work_conserving/"

(* Parse a [.sched] file and resolve it to a normalized [Policy.t]. *)
let normalized file =
  prog_dir ^ file ^ ".sched" |> Parser.parse_file |> Policy.of_program

let make_test name file expected =
  name >:: fun _ ->
  assert_equal expected (normalized file) ~printer:Policy.to_string

let fifo c = Policy.FIFO c

let normalize_tests =
  [
    (* A class declared but absent from the policy is silently dropped. *)
    make_test "unused class is dropped" "drop_class" (fifo "A");
    (* fifo[A] is already in normal form. *)
    make_test "fifo[A] is stable" "fifo_A" (fifo "A");
    (* union[G, H] resolves to a UNION of leaves. *)
    make_test "union[G, H]" "union_GH" (Policy.UNION [ fifo "G"; fifo "H" ]);
    (* strict[B, A]: SP children keep their written order. *)
    make_test "strict preserves arm order" "strict_BA"
      (Policy.SP [ fifo "B"; fifo "A" ]);
    (* rr[B, A, C]: RR children are sorted. *)
    make_test "rr sorts its arms" "rr_BAC"
      (Policy.RR [ fifo "A"; fifo "B"; fifo "C" ]);
    (* wfq[(B, 1), (A, 2)]: slots sort by arm, weights ride along. *)
    make_test "wfq sorts slots, weights follow" "wfq_BA"
      (Policy.WFQ ([ fifo "A"; fifo "B" ], [ 2.0; 1.0 ]));
    (* A nested tree: RR/UNION children sort, SP children don't, and the
       WFQ slots sort by arm (UNION < SP < RR by constructor order). *)
    make_test "complex tree" "complex_tree"
      (Policy.WFQ
         ( [
             Policy.UNION [ fifo "G"; fifo "H" ];
             Policy.SP [ fifo "A"; fifo "B"; fifo "C" ];
             Policy.RR [ fifo "D"; fifo "E"; fifo "F" ];
           ],
           [ 3.0; 1.0; 2.0 ] ));
  ]

let suite = "normalization tests" >::: normalize_tests
let () = run_test_tt_main suite
