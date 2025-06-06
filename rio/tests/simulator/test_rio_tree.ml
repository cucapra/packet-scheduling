open OUnit2
open Simulator
open Util

let rank = 0.0

let push_seq data tree =
  (* Assume PIFO works correctly; therefore fix a specific rank for all pushes.
     This makes our PIFOs FIFOs. *)
  List.fold_left (fun tree i -> Riotree.push tree i rank) tree data

let flush order tree =
  let rec flush_aux tree acc =
    match (Riotree.pop tree order, Riotree.size tree) with
    | None, 0 -> List.rev acc
    | None, _ -> failwith "ERROR: `pop` returns None on non-empty tree"
    | Some _, 0 -> failwith "ERROR: `pop` returns Some _ on size 0 tree"
    | Some (v, tree), _ -> flush_aux tree (v :: acc)
  in
  flush_aux tree []

(* How this test works:
  1. push all of `data` into Rio tree `tree`.
  2. repeatedly pop `tree` with `order` until empty.
  3. check `data` is permuted correctly, i.e. matches `permutation`. *)
let make_push_pop_test name data tree order permutation =
  name >:: fun _ ->
  assert_equal permutation
    (tree |> push_seq data |> flush order)
    ~printer:data_to_string

(* Basic Invariant Tests *)
let basic_tests =
  let topo = Topo.Node [ DecoratedStar [ A ]; DecoratedStar [ B ] ] in
  let tree = Riotree.create topo (fun i -> infer_flow (i mod 2)) in
  let order = Riotree.Order [ (Foot, rank); (Foot, rank) ] in

  [
    ( "`pop` on newly created tree is `None`" >:: fun _ ->
      assert_equal None (Riotree.pop tree order) );
    ( "newly created tree has size zero" >:: fun _ ->
      assert_equal 0 (Riotree.size tree) );
    ( "newly created tree is the correct shape" >:: fun _ ->
      assert_equal topo (Riotree.to_topo tree) );
  ]

(* Leaf Tests *)
let leaf_tests =
  let tree = Riotree.create (Topo.DecoratedStar [ A ]) (fun _ -> A) in
  let order = Riotree.Foot in
  let zero_to_nine = consecutive 0 9 in

  [
    make_push_pop_test "push-pop test on leaf using data {0, 1, 2, ..., 9}"
      zero_to_nine tree order zero_to_nine;
    make_push_pop_test "push-pop test on leaf using random data"
      [ 2; 3; 1; 4; 6; 5; 0 ] tree order [ 2; 3; 1; 4; 6; 5; 0 ];
  ]

(* One Level Topology Tests
          - - - O - - -
          | |  / \  | |
          O O O   O O O
          A B C   D E F
*)
let ff, fl, weird =
  ([ 0; 1; 2; 3; 4; 5 ], [ 5; 4; 3; 2; 1; 0 ], [ 4; 5; 1; 3; 2; 0 ])

let ff_sort, fl_sort, weird_sort =
  let priority_order l a b =
    match (List.nth_opt l (a mod 6), List.nth_opt l (b mod 6)) with
    | Some a, Some b -> a - b
    | _ -> failwith "ERROR: bad list"
  in
  ( List.stable_sort (priority_order ff),
    List.stable_sort (priority_order fl),
    List.stable_sort (priority_order weird) )

let one_level_tests =
  let topo =
    Topo.Node
      [
        DecoratedStar [ A ];
        DecoratedStar [ B ];
        DecoratedStar [ C ];
        DecoratedStar [ D ];
        DecoratedStar [ E ];
        DecoratedStar [ F ];
      ]
  in
  let ff_ord, fl_ord, weird_ord =
    let one_level_ord l =
      Riotree.Order (List.map (fun r -> (Riotree.Foot, float_of_int r)) l)
    in
    (one_level_ord ff, one_level_ord fl, one_level_ord weird)
  in
  let tree = Riotree.create topo infer_flow in
  let zero_to_thirty_five = consecutive 0 35 in

  [
    make_push_pop_test "one level push-pop test with \"forward first\" order"
      zero_to_thirty_five tree ff_ord
      (ff_sort zero_to_thirty_five);
    make_push_pop_test "one level push-pop test with \"forward last\" order"
      zero_to_thirty_five tree fl_ord
      (fl_sort zero_to_thirty_five);
    make_push_pop_test "one level push-pop test with \"weird\" order"
      zero_to_thirty_five tree weird_ord
      (weird_sort zero_to_thirty_five);
  ]

(* Two Level Topology Tests
          - - - - O - - - -
          |     /   \     |
          O    O     O    O
        /   \  C     D  /   \
       O     O         O     O
       A     B         E     F
*)
let two_level_tests =
  let topo =
    let left, right =
      ( Topo.Node [ DecoratedStar [ A ]; DecoratedStar [ B ] ],
        Topo.Node [ DecoratedStar [ E ]; DecoratedStar [ F ] ] )
    in
    Topo.Node [ left; DecoratedStar [ C ]; DecoratedStar [ D ]; right ]
  in
  let ff_ord, fl_ord =
    let ff_small, fl_small =
      ( Riotree.Order [ (Foot, 0.0); (Foot, 1.0) ],
        Riotree.Order [ (Foot, 1.0); (Foot, 0.0) ] )
    in
    ( Riotree.Order
        [ (ff_small, 0.0); (Foot, 1.0); (Foot, 2.0); (ff_small, 3.0) ],
      Riotree.Order
        [ (fl_small, 3.0); (Foot, 2.0); (Foot, 1.0); (fl_small, 0.0) ] )
  in
  let tree = Riotree.create topo infer_flow in
  let zero_to_forty_five = consecutive 0 45 in

  [
    make_push_pop_test "two level push-pop test with \"forward first\" order"
      zero_to_forty_five tree ff_ord
      (ff_sort zero_to_forty_five);
    make_push_pop_test "two level push-pop test with \"forward last\" order"
      zero_to_forty_five tree fl_ord
      (fl_sort zero_to_forty_five);
  ]

let suite =
  "Rio Tree tests"
  >::: basic_tests @ leaf_tests @ one_level_tests @ two_level_tests

let () = run_test_tt_main suite
