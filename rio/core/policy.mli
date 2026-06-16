(* SP arms are paired with their priority rank (lower = higher priority).
   The [bool] is the "designated" flag, distinguishing the paper's [Strict*]
   from [Strict]. Semantics treats the two as identical; the bit exists so
   that [Undesignate]'s precondition (subtree below sits under a designated
   SP) is structural. The DSL parses only undesignated SPs; the flag flips
   to [true] only via the [Designate] lowering. *)
type t =
  | FIFO of Ast.clss
  | SP of (t * float) list * bool
  | RR of t list
  | WFQ of (t * float) list

exception UnboundVariable of Ast.var
exception UndeclaredClass of Ast.clss
exception DuplicateClass of Ast.clss

val of_program : Ast.program -> t
(** Resolve a parsed program (declarations + assignments + return) into a single
    [t]. Variables are substituted in by following assignments; declared classes
    that never appear in the returned policy are silently dropped, per the DSL
    semantics. *)

val to_string : t -> string

val walk : t -> int list -> t
(** [walk p path] descends into [p] following each child index in [path].
    [walk p []] is [p]. Raises if the path goes through a [FIFO] leaf or out of
    bounds. *)
