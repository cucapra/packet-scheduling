(** IR types and string-conversion helpers. *)

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
  | Emancipate of step * vpifo * vpifo
  | Assoc of vpifo * clss
  | Deassoc of vpifo * clss
  | Map of vpifo * clss * step
  | Unmap of vpifo * clss * step
  | Change_pol of vpifo * pol_ty * int
  | Change_weight of vpifo * step * float
  | GC of vpifo
  | Designate of vpifo * vpifo

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
  | Emancipate (s, parent, child) ->
      Printf.sprintf "emancipate(%s, %s, %s)" (string_of_step s)
        (string_of_vpifo parent) (string_of_vpifo child)
  | Assoc (v, c) -> Printf.sprintf "assoc(%s, %s)" (string_of_vpifo v) c
  | Deassoc (v, c) -> Printf.sprintf "deassoc(%s, %s)" (string_of_vpifo v) c
  | Map (v, c, s) ->
      Printf.sprintf "map(%s, %s, %s)" (string_of_vpifo v) c (string_of_step s)
  | Unmap (v, c, s) ->
      Printf.sprintf "unmap(%s, %s, %s)" (string_of_vpifo v) c
        (string_of_step s)
  | Change_pol (v, pt, n) ->
      Printf.sprintf "change_pol(%s, %s, %d)" (string_of_vpifo v)
        (string_of_pol_ty pt) n
  | Change_weight (v, s, w) ->
      Printf.sprintf "change_weight(%s, %s, %F)" (string_of_vpifo v)
        (string_of_step s) w
  | GC v -> Printf.sprintf "gc(%s)" (string_of_vpifo v)
  | Designate (v, survivor) ->
      Printf.sprintf "designate(%s, %s)" (string_of_vpifo v)
        (string_of_vpifo survivor)

let string_of_program p = p |> List.map string_of_instr |> String.concat "\n"
