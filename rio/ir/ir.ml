(** IR for the Rio → hardware-primitive intermediate language, as sketched in
    https://github.com/cucapra/packet-scheduling/discussions/93. *)

type pe = int
type vpifo = int
type step = int
type clss = string

type pol_ty =
  | FIFO
  | RR
  | SP
  | WFQ
  | UNION

type instr =
  | Spawn of vpifo * pe
  | Adopt of step * vpifo * vpifo
  | Assoc of vpifo * clss
  | Deassoc of vpifo * clss
  | Map of vpifo * clss * step
  | Change_pol of vpifo * pol_ty * int
  | Change_weight of vpifo * step * int

type program = instr list

let string_of_pe p = Printf.sprintf "pe%d" p
let string_of_vpifo v = Printf.sprintf "v%d" v
let string_of_step s = Printf.sprintf "step_%d" s

let string_of_pol_ty = function
  | FIFO -> "FIFO"
  | RR -> "RR"
  | SP -> "SP"
  | WFQ -> "WFQ"
  | UNION -> "UNION"

let string_of_instr = function
  | Spawn (v, p) ->
      Printf.sprintf "%s = spawn(%s)" (string_of_vpifo v) (string_of_pe p)
  | Adopt (s, parent, child) ->
      Printf.sprintf "%s = adopt(%s, %s)" (string_of_step s)
        (string_of_vpifo parent) (string_of_vpifo child)
  | Assoc (v, c) -> Printf.sprintf "assoc(%s, %s)" (string_of_vpifo v) c
  | Deassoc (v, c) -> Printf.sprintf "deassoc(%s, %s)" (string_of_vpifo v) c
  | Map (v, c, s) ->
      Printf.sprintf "map(%s, %s, %s)" (string_of_vpifo v) c (string_of_step s)
  | Change_pol (v, pt, n) ->
      Printf.sprintf "change_pol(%s, %s, %d)" (string_of_vpifo v)
        (string_of_pol_ty pt) n
  | Change_weight (v, s, w) ->
      Printf.sprintf "change_weight(%s, %s, %d)" (string_of_vpifo v)
        (string_of_step s) w

let string_of_program p = p |> List.map string_of_instr |> String.concat "\n"

exception UnsupportedPolicy of string

(* Every leaf is a [FIFO c]: a single-class vPIFO with no children. 
    Anything else (a nested policy node, EDF, …) raises [UnsupportedPolicy]. *)
let class_of_leaf (p : Frontend.Policy.t) : clss =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c -> c
  | P.UNION _ | P.Strict _ | P.RR _ | P.WFQ _ ->
      raise
        (UnsupportedPolicy
           "nested policy nodes are not supported in MS1; only one-level \
            work-conserving programs compile")
  | P.EDF _ -> raise (UnsupportedPolicy "EDF is not supported in MS1")

let make_counter () =
  let n = ref (-1) in
  fun () ->
    incr n;
    !n

(* Compile a one-level [UNION] / [RR] / [Strict] / [WFQ]. [weights] is
   [None] for round-robin and union; [Some ws] for strict / weighted-fair.
   Every child must be a [FIFO c] in MS1. *)
let compile_arm_node ~pol_ty ~children ~weights =
  let fresh_v = make_counter () in
  let fresh_s = make_counter () in
  let root_pe = 1 in
  let leaf_pe = 2 in
  let root = fresh_v () in
  (* For each child: resolve its class, allocate a leaf vPIFO and a step.
     List.map is left-to-right in the stdlib, so IDs come out in source
     order. *)
  let leaves =
    List.map
      (fun child ->
        let c = class_of_leaf child in
        let leaf = fresh_v () in
        let step = fresh_s () in
        (c, leaf, step))
      children
  in
  let spawn_root = [ Spawn (root, root_pe) ] in
  (* The list [leaves] now has all the info we need to generate our adopt, assoc, map commands *)
  let spawn_leaves =
    List.map (fun (_, leaf, _) -> Spawn (leaf, leaf_pe)) leaves
  in
  let adopts =
    List.map (fun (_, leaf, step) -> Adopt (step, root, leaf)) leaves
  in
  let root_assocs = List.map (fun (c, _, _) -> Assoc (root, c)) leaves in
  let leaf_assocs = List.map (fun (c, leaf, _) -> Assoc (leaf, c)) leaves in
  let maps = List.map (fun (c, _, step) -> Map (root, c, step)) leaves in
  let change_pol_instr = [ Change_pol (root, pol_ty, List.length children) ] in
  let change_weight_instrs =
    match weights with
    | None -> []
    | Some ws ->
        if List.length ws <> List.length leaves then
          raise (UnsupportedPolicy "internal error: weight/arm count mismatch");
        List.map2
          (fun (_, _, step) w -> Change_weight (root, step, w))
          leaves ws
  in
  List.concat
    [
      spawn_root;
      spawn_leaves;
      adopts;
      root_assocs;
      leaf_assocs;
      maps;
      change_pol_instr;
      change_weight_instrs;
    ]

let of_policy (p : Frontend.Policy.t) : program =
  let module P = Frontend.Policy in
  match p with
  | P.FIFO c ->
      (* A single-class top-level policy: one vPIFO with one [Assoc]. *)
      [ Spawn (0, 1); Assoc (0, c) ]
  | P.UNION children -> compile_arm_node ~pol_ty:UNION ~children ~weights:None
  | P.RR children -> compile_arm_node ~pol_ty:RR ~children ~weights:None
  | P.Strict children ->
      (* Strict priority: first child has priority 1 (highest), then 2, 3, … *)
      let weights = Some (List.mapi (fun i _ -> i + 1) children) in
      compile_arm_node ~pol_ty:SP ~children ~weights
  | P.WFQ (children, ws) ->
      (* [Frontend.Policy] stores weights as [float] converted from DSL
         [int]s. Convert back for the IR. *)
      let weights = Some (List.map int_of_float ws) in
      compile_arm_node ~pol_ty:WFQ ~children ~weights
  | P.EDF _ -> raise (UnsupportedPolicy "EDF is not supported in MS1")
