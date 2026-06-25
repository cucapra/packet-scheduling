open Rio_core
open Rio_planner
open Frontend

(* Walk a parsed policy expression and collect the class names it mentions, so
   the user does not have to declare them up front. Variables are not expected
   here (the REPL parses a bare policy_only), but if one slipped through,
   Pol.of_program would raise UnboundVariable on the substitution pass. *)
let rec classes_of_stream (s : Ast.stream) : Ast.clss list =
  match s with
  | Fifo c | EarliestDeadline c | ShortestJobNext c | ShortestRemaining c ->
      [ c ]
  | RoundRobin ps
  | RateControlled ps
  | LeakyBucket (ps, _, _)
  | TokenBucket (ps, _, _)
  | StopAndGo (ps, _) -> List.concat_map classes_of_stream ps
  | Strict prs | WeightedFair prs ->
      List.concat_map (fun (p, _) -> classes_of_stream p) prs
  | Var _ -> []

let pol_of_typed (s : Ast.stream) : Pol.t =
  let classes = List.sort_uniq compare (classes_of_stream s) in
  Pol.of_program (classes, [], s)

let pol_of_file (path : string) : Pol.t =
  path |> Parser.parse_file |> Pol.of_program

(* Render a policy as an indented tree. Inner nodes (RR/SP/WFQ) put their
   constructor name on its own line and indent their children by two spaces;
   FIFO leaves stay on one line. SP and WFQ arms carry a [rank N] / [weight N]
   tag on the same line as the child's first token. *)
let rec render_pol indent prefix (p : Pol.t) =
  let head tag = indent ^ prefix ^ tag in
  let child_indent = indent ^ "  " in
  match p with
  | FIFO c -> [ head (Printf.sprintf "fifo[%s]" c) ]
  | RR arms -> head "rr" :: List.concat_map (render_pol child_indent "") arms
  | SP (arm_metas, _designated) ->
      head "strict"
      :: List.concat_map
           (fun (a, r) ->
             render_pol child_indent (Printf.sprintf "[rank %g] " r) a)
           arm_metas
  | WFQ arm_metas ->
      head "wfq"
      :: List.concat_map
           (fun (a, w) ->
             render_pol child_indent (Printf.sprintf "[weight %g] " w) a)
           arm_metas

let pretty_pol p = String.concat "\n" (render_pol "  " "" p)

let string_of_guard = function
  | Planner.True -> "True"
  | Planner.Empty path ->
      Printf.sprintf "Empty %s" (Planner.Delta.path_to_string path)

let pretty_seq = function
  | [] -> "  (empty; policies are equivalent)"
  | seq ->
      let guards = List.map (fun (g, _) -> string_of_guard g) seq in
      let guard_w = List.fold_left max 0 (List.map String.length guards) in
      seq
      |> List.mapi (fun i (g, d) ->
          Printf.sprintf "  %d. %-*s | %s" (i + 1) guard_w (string_of_guard g)
            (Planner.Delta.to_string d))
      |> String.concat "\n"

let print_running p = Printf.printf "running:\n%s\n\n%!" (pretty_pol p)

let print_transition prev next =
  let seq = Planner.analyze prev next in
  Printf.printf "gseq:\n%s\n" (pretty_seq seq);
  print_running next

let handle_input prev input =
  let next =
    match input with
    | `Typed s -> pol_of_typed s
    | `File path -> pol_of_file path
  in
  (match prev with
  | None -> print_running next
  | Some p -> print_transition p next);
  Some next

(* A line beginning with `#` (after leading whitespace) is a user comment and
   is ignored, just like a blank line. *)
let classify_line line : [ `Quit | `Empty | `Load of string | `Typed of string ]
    =
  let line = String.trim line in
  if line = "" then `Empty
  else if String.length line >= 1 && line.[0] = '#' then `Empty
  else if line = ":quit" || line = ":q" then `Quit
  else if String.length line >= 5 && String.sub line 0 5 = ":load" then
    `Load (String.trim (String.sub line 5 (String.length line - 5)))
  else `Typed line

let report_error fmt =
  Printf.ksprintf (fun s -> Printf.printf "error: %s\n%!" s) fmt

let step state line =
  match classify_line line with
  | `Quit -> `Done
  | `Empty -> `Continue state
  | `Load path -> (
      try `Continue (handle_input state (`File path)) with
      | Parser.FileNotFound f ->
          report_error "file not found: %s" f;
          `Continue state
      | Parser.ParserError _ ->
          report_error "parse error in %s" path;
          `Continue state
      | Pol.UndeclaredClass c ->
          report_error "undeclared class %s" c;
          `Continue state
      | Pol.DuplicateClass c ->
          report_error "duplicate class %s" c;
          `Continue state
      | Pol.UnboundVariable v ->
          report_error "unbound variable %s" v;
          `Continue state)
  | `Typed src -> (
      try
        `Continue (handle_input state (`Typed (Parser.parse_policy_string src)))
      with
      | Parser.ParserError _ ->
          report_error "parse error";
          `Continue state
      | Pol.DuplicateClass c ->
          report_error "duplicate class %s" c;
          `Continue state
      | Pol.UnboundVariable v ->
          report_error "unbound variable %s" v;
          `Continue state)

let banner () =
  print_endline "rio declarative-mode REPL.";
  print_endline
    "  type a policy expression (e.g. `rr[A, B]`) or `:load file.sched`.";
  print_endline "  lines starting with `#` are ignored as comments.";
  print_endline "  `:quit` to exit."

let prompt () =
  print_string "rio> ";
  flush stdout

let rec loop state =
  prompt ();
  match input_line stdin with
  | exception End_of_file ->
      print_newline ();
      ()
  | line -> (
      match step state line with
      | `Done -> ()
      | `Continue state' -> loop state')

let () =
  banner ();
  loop None
