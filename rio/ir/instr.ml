(** IR types and string-conversion helpers. *)

type pe = int
type pifo = int
type step = int
type clss = string

type pol_ty =
  | FIFO
  | RR
  | SP
  | WFQ
  | SP_star
(* SP_star: the IR-level marker that [v] hosts a designated super-node (an
   SP* in the paper's terminology). Carries the contract:

   - Shape. A designated super-node has exactly two children, adopted at
     indices 0 (loser) and 1 (survivor) with [Set_arm_meta] ranks 1.0 and 2.0
     respectively, so the loser is favored while it drains.
   - Lifecycle. Emitted exactly once per super-node by [patch_designate] via
     [Set_policy (sp_v, SP_star, 2)]; cleared implicitly at [Undesignate]
     when [GC sp_v] retires the v. Never emitted by [of_policy] or by the
     source DSL.
   - Same-PE invariant (paper/code-divergences.md A4). [sp_v], the
     loser-subtree root, and the survivor-subtree root are guaranteed to
     live on the same PE. [patch_designate] places all three at
     [pe_of_depth(arm_depth)] so the substrate may coalesce them into a
     single physical slot; that coalescing is the substrate's prerogative,
     not an ISA-level lowering choice.
   - Mirror. [Decorated.SP]'s trailing [bool] flag is the decorated-tree
     analogue at the source-shape level. *)

type instr =
  | Spawn of pifo * pe
  | Adopt of step * pifo * pifo
  | Emancipate of step * pifo
  | Assoc of pifo * clss
  | Deassoc of pifo * clss
  | Map of pifo * clss * step
  | Unmap of pifo * clss
  | Set_policy of pifo * pol_ty * int
  | Change_arity of pifo * int
  | Set_arm_meta of pifo * step * float
  | GC of pifo
  | Designate of pifo * pifo
  | Undesignate of pifo

type commit = instr list

let string_of_pe p = Printf.sprintf "pe%d" p
let string_of_pifo v = Printf.sprintf "v%d" v
let string_of_step s = Printf.sprintf "step_%d" s

let string_of_pol_ty = function
  | FIFO -> "FIFO"
  | RR -> "RR"
  | SP -> "SP"
  | WFQ -> "WFQ"
  | SP_star -> "SP*"

let string_of_instr = function
  | Spawn (v, p) ->
      Printf.sprintf "%s = spawn(%s)" (string_of_pifo v) (string_of_pe p)
  | Adopt (s, parent, child) ->
      Printf.sprintf "%s = adopt(%s, %s)" (string_of_step s)
        (string_of_pifo parent) (string_of_pifo child)
  | Emancipate (s, parent) ->
      Printf.sprintf "emancipate(%s, %s)" (string_of_step s)
        (string_of_pifo parent)
  | Assoc (v, c) -> Printf.sprintf "assoc(%s, %s)" (string_of_pifo v) c
  | Deassoc (v, c) -> Printf.sprintf "deassoc(%s, %s)" (string_of_pifo v) c
  | Map (v, c, s) ->
      Printf.sprintf "map(%s, %s, %s)" (string_of_pifo v) c (string_of_step s)
  | Unmap (v, c) -> Printf.sprintf "unmap(%s, %s)" (string_of_pifo v) c
  | Set_policy (v, pt, n) ->
      Printf.sprintf "set_policy(%s, %s, %d)" (string_of_pifo v)
        (string_of_pol_ty pt) n
  | Change_arity (v, n) ->
      Printf.sprintf "change_arity(%s, %d)" (string_of_pifo v) n
  | Set_arm_meta (v, s, w) ->
      Printf.sprintf "set_arm_meta(%s, %s, %F)" (string_of_pifo v)
        (string_of_step s) w
  | GC v -> Printf.sprintf "gc(%s)" (string_of_pifo v)
  | Designate (v, survivor) ->
      Printf.sprintf "designate(%s, %s)" (string_of_pifo v)
        (string_of_pifo survivor)
  | Undesignate v -> Printf.sprintf "undesignate(%s)" (string_of_pifo v)

let string_of_commit p = p |> List.map string_of_instr |> String.concat "\n"
