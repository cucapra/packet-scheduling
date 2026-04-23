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

let of_policy (p : Frontend.Policy.t) : compiled =
  let fresh_v, peek_v = make_counter ~start:100 in
  let fresh_s, peek_s = make_counter ~start:1000 in
  let frag = compile_subtree ~fresh_v ~fresh_s ~depth:0 p in
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
  (* PR1: identities tables stay empty placeholders; PR2 will thread node_ids
     through [compile_subtree]/[compile_arm] to populate them. *)
  let identities = { vpifos = Hashtbl.create 16; steps = Hashtbl.create 16 } in
  {
    prog;
    policy = p;
    identities;
    next_vpifo = peek_v ();
    next_step = peek_s ();
  }

(* JSON exporter for compiled IR programs. Re-exports [from_instr] from the
   [Json] file-module and adds [from_compiled], which lives here because it
   needs the [compiled] type defined above. *)
module Json = struct
  include Json

  (* Stringified tree path. Root is the empty string; non-root is dot-
     separated child indices, e.g. [[0; 2]] -> ["0.2"]. *)
  let path_to_string (path : node_id) : string =
    path |> List.map string_of_int |> String.concat "."

  (* Stringified (parent_path, child_index) key. The slash separates the
     parent path from the child index, and an empty parent path collapses to
     just ["/<i>"]. E.g. [([], 2)] -> ["/2"]; [([0; 1], 2)] -> ["0.1/2"]. *)
  let step_key_to_string ((parent, child) : node_id * int) : string =
    Printf.sprintf "%s/%d" (path_to_string parent) child

  let from_compiled (c : compiled) : Yojson.Basic.t =
    let instrs = `List (List.map from_instr c.prog) in
    (* Sort by stringified key so the JSON output is deterministic regardless
       of Hashtbl insertion order. *)
    let sorted_pairs of_key tbl =
      Hashtbl.fold (fun k v acc -> (of_key k, `Int v) :: acc) tbl []
      |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    in
    let vpifo_pairs = sorted_pairs path_to_string c.identities.vpifos in
    let step_pairs = sorted_pairs step_key_to_string c.identities.steps in
    `Assoc
      [
        ("instrs", instrs);
        ( "identities",
          `Assoc
            [ ("vpifos", `Assoc vpifo_pairs); ("steps", `Assoc step_pairs) ] );
        ("next_vpifo", `Int c.next_vpifo);
        ("next_step", `Int c.next_step);
      ]
end
