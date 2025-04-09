open OUnit2
open Simulator

let ( mod ) a b = (a mod b) + if a < 0 then b else 0
let fmt = Printf.sprintf

let intlist_to_string intlist =
  "[" ^ String.concat "; " (List.map string_of_int intlist) ^ "]"

let consecutive bot top =
  let rec consecutive_aux acc i =
    match i <= top with
    | true -> consecutive_aux (i :: acc) (i + 1)
    | false -> List.rev acc
  in
  consecutive_aux [] bot

type flow =
  | A
  | B
  | C
  | D
  | E
  | F

let infer_flow i =
  match i mod 6 with
  | 0 -> A
  | 1 -> B
  | 2 -> C
  | 3 -> D
  | 4 -> E
  | 5 -> F
  | _ -> failwith "Ruh Roh Raggy"

let rank = 0

let push_seq data tree =
  (* Assume PIFO works correctly; therefore fix a specific rank for all pushes.
     This makes our PIFOs FIFOs. *)
  List.fold_left (fun tree i -> Riotree.push tree i rank) tree data

let flush order tree =
  let rec flush_aux tree acc =
    match (Riotree.pop tree order, Riotree.size tree) with
    | None, 0 -> List.rev acc
    | None, i -> failwith (fmt "ERROR: `pop` returns None on size %d tree" i)
    | Some _, 0 -> failwith "ERROR: [pop] returns Some _ on size 0 tree"
    | Some (v, tree), _ -> flush_aux tree (v :: acc)
  in
  flush_aux tree []

(* How this test works:
  1. push all of `data` into an empty Rio tree written against `topo` and `canon`.
  2. repeatedly pop the tree with `order` until empty.
  3. check `data` is permuted correctly, i.e. matches `permutation`. *)
let make_push_pop_test name data tree order permutation =
  name >:: fun _ ->
  assert_equal permutation
    (tree |> push_seq data |> flush order)
    ~printer:intlist_to_string

(* Basic Invariant Tests *)
let basic_tests =
  let topo = Topo.Node [ CStar A; CStar B ] in
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
  let tree = Riotree.create (Topo.CStar A) (fun _ -> A) in
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
            |   / | \   |
            O O   O  O  O
*)
let topo = Topo.Node [ CStar A; CStar B; CStar C; CStar D; CStar E; CStar F ]
let tree = Riotree.create topo infer_flow

let ff, fl, weird =
  ([ 0; 1; 2; 3; 4; 5 ], [ 5; 4; 3; 2; 1; 0 ], [ 4; 5; 1; 3; 2; 0 ])

let ff_ord, fl_ord, weird_ord =
  let one_level_ord l =
    Riotree.Order (List.map (fun r -> (Riotree.Foot, r)) l)
  in
  (one_level_ord ff, one_level_ord fl, one_level_ord weird)

let ff_sort, fl_sort, weird_sort =
  let priority_order l a b =
    match (List.nth_opt l (a mod 6), List.nth_opt l (b mod 6)) with
    | Some a, Some b -> a - b
    | _ -> failwith "ERROR: bad list"
  in
  ( List.stable_sort (priority_order ff),
    List.stable_sort (priority_order fl),
    List.stable_sort (priority_order weird) )

let zero_to_thirty_five = consecutive 0 35

let one_level_tests =
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
        /   \           /   \
       O     O         O     O
*)
let topo =
  let left, right =
    (Topo.Node [ CStar A; CStar B ], Topo.Node [ CStar E; CStar F ])
  in
  Topo.Node [ left; CStar C; CStar D; right ]

let tree = Riotree.create topo infer_flow

let ff_ord, fl_ord =
  let ff_small_ord, fl_small_ord =
    ( Riotree.Order [ (Foot, 0); (Foot, 1) ],
      Riotree.Order [ (Foot, 1); (Foot, 0) ] )
  in
  ( Riotree.Order [ (ff_small_ord, 0); (Foot, 1); (Foot, 2); (ff_small_ord, 3) ],
    Riotree.Order [ (fl_small_ord, 3); (Foot, 2); (Foot, 1); (fl_small_ord, 0) ]
  )

let two_level_tests =
  [
    make_push_pop_test "two level push-pop test with \"forward first\" order"
      zero_to_thirty_five tree ff_ord
      (ff_sort zero_to_thirty_five);
    make_push_pop_test "two level push-pop test with \"forward last\" order"
      zero_to_thirty_five tree fl_ord
      (fl_sort zero_to_thirty_five);
  ]

let suite =
  "Rio Tree tests"
  >::: basic_tests @ leaf_tests @ one_level_tests @ two_level_tests

let () = run_test_tt_main suite
