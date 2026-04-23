(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

let make_counter ~start =
  let n = ref (start - 1) in
  fun () ->
    incr n;
    !n

(* The instructions for compiling a subtree, grouped by instruction "kind".
   By merging these [frags] with care, we can make a top-level
   concatenation that has all spawns first, then all adopts, etc. *)
type frag = {
  spawns : instr list;
  adopts : instr list;
  assocs : instr list;
  maps : instr list;
  change_pols : instr list;
  change_weights : instr list;
  root_v : vpifo; (* The root of the subtree that this [frag] creates *)
  classes : clss list; (* Classes handled by this subtree *)
}

(** Given a fresh vPIFO ID, the depth of PE to place the fresh vPIFO into, and
    the class to assciate the vPIFO with, complete all the necessary steps to
    stand up the FIFO. This is also a good place to get warmed up with [frag]s.
    A [frag] is the relevant component of the final program that pertains to
    setting up the present subtree. *)
let compile_FIFO ~v ~depth c =
  {
    spawns = [ Spawn (v, depth) ];
    assocs = [ Assoc (v, c) ];
    root_v = v;
    classes = [ c ];
    (* many frag fields are empty when setting up a vPIFO that runs FIFO *)
    adopts = [];
    maps = [];
    change_pols = [];
    change_weights = [];
  }

(* Compile a [Frontend.Policy.t] subtree rooted at [depth]. 
   PE assignment is depth-based — every node at depth [d] lives on [pe d]. *)
let rec compile_subtree ~fresh_v ~fresh_s ~depth (p : Frontend.Policy.t) : frag
    =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~depth c
  | P.UNION children ->
      compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:UNION children
  | P.RR children -> compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:RR children
  | P.SP children ->
      (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
      let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
      compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:SP ~weights children
  | P.WFQ (children, ws) ->
      compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty:WFQ ~weights:ws children

and compile_arm ~fresh_v ~fresh_s ~depth ~pol_ty ?weights children =
  (* Spawn self first, so that we get a lower ID number than the kids. *)
  let v = fresh_v () in
  let local_spawn = Spawn (v, depth) in
  (* Recurse on each child first; List.map is left-to-right in the stdlib
     so vpifo IDs come out in source order. *)
  let child_frags =
    List.map (compile_subtree ~fresh_v ~fresh_s ~depth:(depth + 1)) children
  in
  (* The children are all done at this point, and the relevant instructions are in [child_frags]. 
  Now we just need to connect the present node with the children. *)

  (* Adopt each child, and store the fresh step ID that we get at the moment we adopt it. *)
  let adoption_records =
    List.map
      (fun cf ->
        let s = fresh_s () in
        (Adopt (s, v, cf.root_v), s, cf))
      child_frags
  in
  (* Yank out the literal [adopt] commands from [adoption_records] *)
  let local_adopts = List.map (fun (a, _, _) -> a) adoption_records in
  (* Create a list of classes with which the children are [assoc]iated *)
  let all_classes = List.concat_map (fun cf -> cf.classes) child_frags in
  (* and [assoc]iate with them yourself *)
  let local_assocs = List.map (fun c -> Assoc (v, c)) all_classes in
  (* Add mappings to remember how to get to each child *)
  let local_maps =
    List.concat_map
      (fun (_, s, cf) -> List.map (fun c -> Map (v, c, s)) cf.classes)
      adoption_records
  in
  (* set the local policy and, if supplied, the relevant weights *)
  let local_change_pol = [ Change_pol (v, pol_ty, List.length children) ] in
  let local_change_weights =
    match weights with
    | None -> []
    | Some ws ->
        (* Frontend invariant: WFQ/Strict produce one weight per arm. *)
        assert (List.length ws = List.length adoption_records);
        List.map2
          (fun (_, s, _) w -> Change_weight (v, s, w))
          adoption_records ws
  in
  (* We have lots of instructions within [child_frags], and we have lots of local instructions. 
     Time to merge them carefully. *)
  {
    spawns = local_spawn :: List.concat_map (fun cf -> cf.spawns) child_frags;
    adopts = local_adopts @ List.concat_map (fun cf -> cf.adopts) child_frags;
    assocs = local_assocs @ List.concat_map (fun cf -> cf.assocs) child_frags;
    maps = local_maps @ List.concat_map (fun cf -> cf.maps) child_frags;
    change_pols =
      local_change_pol @ List.concat_map (fun cf -> cf.change_pols) child_frags;
    change_weights =
      local_change_weights
      @ List.concat_map (fun cf -> cf.change_weights) child_frags;
    root_v = v;
    classes = all_classes;
  }

let of_policy (p : Frontend.Policy.t) : program =
  let fresh_v = make_counter ~start:100 in
  let fresh_s = make_counter ~start:1000 in
  let frag = compile_subtree ~fresh_v ~fresh_s ~depth:0 p in
  List.concat
    [
      frag.spawns;
      frag.adopts;
      frag.assocs;
      frag.maps;
      frag.change_pols;
      frag.change_weights;
    ]

(* Re-export the JSON exporter as a submodule so consumers say
   [Ir.Json.from_program]. *)
module Json = Json
