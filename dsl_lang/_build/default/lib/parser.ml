
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TRANSIENT
    | STRICT
    | RBRACE
    | LBRACE
    | FIFO
    | FAIR
    | EOF
    | COMMA
    | CLSS of (
# 12 "lib/parser.mly"
       (string)
# 23 "lib/parser.ml"
  )
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
    open Ast

    let list_expr ls =
    match ls with
    | [l] -> l 
    | _ -> Strict ls (* what to put for typ?? *)


# 40 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_prog) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState02 : (('s, _menhir_box_prog) _menhir_cell1_TRANSIENT, _menhir_box_prog) _menhir_state
    (** State 02.
        Stack shape : TRANSIENT.
        Start symbol: prog. *)

  | MenhirState04 : (('s, _menhir_box_prog) _menhir_cell1_STRICT, _menhir_box_prog) _menhir_state
    (** State 04.
        Stack shape : STRICT.
        Start symbol: prog. *)

  | MenhirState06 : (('s, _menhir_box_prog) _menhir_cell1_FIFO, _menhir_box_prog) _menhir_state
    (** State 06.
        Stack shape : FIFO.
        Start symbol: prog. *)

  | MenhirState08 : (('s, _menhir_box_prog) _menhir_cell1_FAIR, _menhir_box_prog) _menhir_state
    (** State 08.
        Stack shape : FAIR.
        Start symbol: prog. *)

  | MenhirState12 : (('s, _menhir_box_prog) _menhir_cell1_lexp, _menhir_box_prog) _menhir_state
    (** State 12.
        Stack shape : lexp.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_lexp = 
  | MenhirCell1_lexp of 's * ('s, 'r) _menhir_state * (Ast.policy)

and ('s, 'r) _menhir_cell1_FAIR = 
  | MenhirCell1_FAIR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FIFO = 
  | MenhirCell1_FIFO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRICT = 
  | MenhirCell1_STRICT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_TRANSIENT = 
  | MenhirCell1_TRANSIENT of 's * ('s, 'r) _menhir_state

and _menhir_box_prog = 
  | MenhirBox_prog of (Ast.policy) [@@unboxed]

let _menhir_action_01 =
  fun _3 ->
    (
# 46 "lib/parser.mly"
                                               ( Fifo(_3) )
# 97 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_02 =
  fun _3 ->
    (
# 47 "lib/parser.mly"
                                               ( Fair(_3) )
# 105 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_03 =
  fun _3 ->
    (
# 48 "lib/parser.mly"
                                               ( Strict(_3) )
# 113 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_04 =
  fun _3 ->
    (
# 49 "lib/parser.mly"
                                               ( Transient(_3) )
# 121 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_05 =
  fun _1 ->
    (
# 39 "lib/parser.mly"
                                               ( list_expr _1 )
# 129 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_06 =
  fun _1 _3 ->
    (
# 42 "lib/parser.mly"
                                             ( _1::_3 )
# 137 "lib/parser.ml"
     : (Ast.policy list))

let _menhir_action_07 =
  fun _1 ->
    (
# 43 "lib/parser.mly"
                                             ( [_1] )
# 145 "lib/parser.ml"
     : (Ast.policy list))

let _menhir_action_08 =
  fun _1 ->
    (
# 35 "lib/parser.mly"
                                               ( Class(_1) )
# 153 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_09 =
  fun _1 ->
    (
# 36 "lib/parser.mly"
                                               ( _1 )
# 161 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_10 =
  fun _1 ->
    (
# 32 "lib/parser.mly"
                                               ( _1 )
# 169 "lib/parser.ml"
     : (Ast.policy))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | CLSS _ ->
        "CLSS"
    | COMMA ->
        "COMMA"
    | EOF ->
        "EOF"
    | FAIR ->
        "FAIR"
    | FIFO ->
        "FIFO"
    | LBRACE ->
        "LBRACE"
    | RBRACE ->
        "RBRACE"
    | STRICT ->
        "STRICT"
    | TRANSIENT ->
        "TRANSIENT"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_TRANSIENT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_s = MenhirState02 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRANSIENT ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STRICT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_s = MenhirState04 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRANSIENT ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FIFO (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_s = MenhirState06 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRANSIENT ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FAIR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let _menhir_s = MenhirState08 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TRANSIENT ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TRANSIENT ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | STRICT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | FIFO ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | FAIR ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | CLSS _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_08 _1 in
          (match (_tok : MenhirBasics.token) with
          | EOF ->
              let _1 = _v in
              let _v = _menhir_action_10 _1 in
              MenhirBox_prog _v
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
