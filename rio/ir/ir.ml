(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

include Instr

(* See [ir.mli] for the documented forms of these. *)
type node_id = int list

type identities = {
  vpifos : (node_id, vpifo) Hashtbl.t;
  steps : (node_id * int, step) Hashtbl.t;
}

type compiled = {
  prog : program;
  policy : Frontend.Policy.t;
  identities : identities;
  next_vpifo : int;
  next_step : int;
}

(* A counter starting at [start]. [fresh ()] returns the next value (first
   call returns [start]). [peek_next ()] reports what [fresh ()] would return
   on its next invocation, without advancing the counter. *)
let make_counter ~start =
  let n = ref (start - 1) in
  let fresh () =
    incr n;
    !n
  in
  let peek_next () = !n + 1 in
  (fresh, peek_next)

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

(** Given a fresh vPIFO ID, the depth of PE to place the fresh vPIFO into, the
    tree path of this leaf, the identity tables to register into, and the
    class to associate with, complete all the necessary steps to stand up the
    FIFO. This is also a good place to get warmed up with [frag]s. A [frag] is
    the relevant component of the final program that pertains to setting up
    the present subtree. *)
let compile_FIFO ~v ~depth ~path ~identities c =
  Hashtbl.add identities.vpifos path v;
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

(* Compile a [Frontend.Policy.t] subtree rooted at [depth]. [path] is the tree
   position of this subtree's root within the top-level policy, used to key
   the identity tables. PE assignment is depth-based — every node at depth
   [d] lives on [pe d]. *)
let rec compile_subtree ~fresh_v ~fresh_s ~depth ~path ~identities
    (p : Frontend.Policy.t) : frag =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> compile_FIFO ~v:(fresh_v ()) ~depth ~path ~identities c
  | P.UNION children ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:UNION
        children
  | P.RR children ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:RR
        children
  | P.SP children ->
      (* Strict priority: first child has priority 1.0 (highest), then 2.0, 3.0, … *)
      let weights = List.mapi (fun i _ -> float_of_int (i + 1)) children in
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:SP
        ~weights children
  | P.WFQ (children, ws) ->
      compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty:WFQ
        ~weights:ws children

and compile_arm ~fresh_v ~fresh_s ~depth ~path ~identities ~pol_ty ?weights
    children =
  (* Spawn self first, so that we get a lower ID number than the kids. *)
  let v = fresh_v () in
  Hashtbl.add identities.vpifos path v;
  let local_spawn = Spawn (v, depth) in
  (* Recurse on each child first; List.mapi is left-to-right in the stdlib so
     vpifo IDs come out in source order, and [i] gives us the child's index
     for both the path extension and the (parent_path, child_index) step
     key. *)
  let child_frags =
    List.mapi
      (fun i child ->
        compile_subtree ~fresh_v ~fresh_s ~depth:(depth + 1)
          ~path:(path @ [ i ]) ~identities child)
      children
  in
  (* The children are all done at this point, and the relevant instructions are in [child_frags].
  Now we just need to connect the present node with the children. *)

  (* Adopt each child, and store the fresh step ID that we get at the moment we adopt it.
     We register every step ID in [identities.steps] keyed by (this node's
     path, the child's index). *)
  let adoption_records =
    List.mapi
      (fun i cf ->
        let s = fresh_s () in
        Hashtbl.add identities.steps (path, i) s;
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

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v, peek_v = make_counter ~start:100 in
  let fresh_s, peek_s = make_counter ~start:1000 in
  (* Identity tables are created here and mutated in place as
     [compile_subtree]/[compile_arm] register each spawn and adopt. *)
  let identities = { vpifos = Hashtbl.create 16; steps = Hashtbl.create 16 } in
  let frag =
    compile_subtree ~fresh_v ~fresh_s ~depth:0 ~path:[] ~identities p
  in
  let prog =
    List.concat
      [
        frag.spawns;
        frag.adopts;
        frag.assocs;
        frag.maps;
        frag.change_pols;
        frag.change_weights;
      ]
  in
  {
    prog;
    policy = p;
    identities;
    next_vpifo = peek_v ();
    next_step = peek_s ();
  }

(* Re-export the per-program / per-instruction JSON exporters as a submodule
   so consumers say [Ir.Json.from_program]. The [identities] / counter
   metadata on [compiled] is intentionally not serialized — it's runtime
   state for [patch], not part of the IR's external surface. *)
module Json = Json
