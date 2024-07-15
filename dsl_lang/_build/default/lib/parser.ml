
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | VAR of (
# 20 "lib/parser.mly"
       (string)
# 15 "lib/parser.ml"
  )
    | STRICT
    | RBRACE
    | LBRACE
    | FIFO
    | FAIR
    | EQUALS
    | EOF
    | COMMA
    | CLSS of (
# 21 "lib/parser.mly"
       (string)
# 28 "lib/parser.ml"
  )
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
    open Ast

    let fifo_list ls =
    match ls with
    | [l] -> l 
    | _ -> Fifo ls 

    let rr_list ls =
    match ls with
    | [l] -> l 
    | _ -> Fair ls

    let strict_list ls =
    match ls with
    | [l] -> l 
    | _ -> Strict ls

# 54 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_prog) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: prog. *)

  | MenhirState02 : (('s, _menhir_box_prog) _menhir_cell1_VAR, _menhir_box_prog) _menhir_state
    (** State 02.
        Stack shape : VAR.
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

  | MenhirState12 : (('s, _menhir_box_prog) _menhir_cell1_exp, _menhir_box_prog) _menhir_state
    (** State 12.
        Stack shape : exp.
        Start symbol: prog. *)


and ('s, 'r) _menhir_cell1_exp = 
  | MenhirCell1_exp of 's * ('s, 'r) _menhir_state * (Ast.policy)

and ('s, 'r) _menhir_cell1_FAIR = 
  | MenhirCell1_FAIR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FIFO = 
  | MenhirCell1_FIFO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRICT = 
  | MenhirCell1_STRICT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * (
# 20 "lib/parser.mly"
       (string)
# 104 "lib/parser.ml"
)

and _menhir_box_prog = 
  | MenhirBox_prog of (Ast.policy) [@@unboxed]

let _menhir_action_01 =
  fun _1 _3 ->
    (
# 51 "lib/parser.mly"
                                               ( _1::_3 )
# 115 "lib/parser.ml"
     : (Ast.policy list))

let _menhir_action_02 =
  fun _1 ->
    (
# 52 "lib/parser.mly"
                                        ( [_1] )
# 123 "lib/parser.ml"
     : (Ast.policy list))

let _menhir_action_03 =
  fun _1 ->
    (
# 55 "lib/parser.mly"
                ( _1 )
# 131 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_04 =
  fun _3 ->
    (
# 44 "lib/parser.mly"
                                               ( fifo_list _3 )
# 139 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_05 =
  fun _3 ->
    (
# 45 "lib/parser.mly"
                                               ( strict_list _3 )
# 147 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_06 =
  fun _3 ->
    (
# 46 "lib/parser.mly"
                                               ( rr_list _3 )
# 155 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_07 =
  fun _1 ->
    (
# 47 "lib/parser.mly"
                                               ( Class(_1) )
# 163 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_08 =
  fun _1 _3 ->
    (
# 48 "lib/parser.mly"
                            ( Assn(_1, _3) )
# 171 "lib/parser.ml"
     : (Ast.policy))

let _menhir_action_09 =
  fun _1 ->
    (
# 41 "lib/parser.mly"
                                               ( _1 )
# 179 "lib/parser.ml"
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
    | EQUALS ->
        "EQUALS"
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
    | VAR _ ->
        "VAR"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _v _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let _1 = _v in
          let _v = _menhir_action_09 _1 in
          MenhirBox_prog _v
      | _ ->
          _eRR ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | EQUALS ->
          let _menhir_s = MenhirState02 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLSS _v ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLSS _v ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLSS _v ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
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
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLSS _v ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_07 _1 in
      _menhir_goto_policy _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_policy : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_22 _menhir_stack _v _tok
      | MenhirState02 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState04 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState08 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_20 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_VAR -> _ -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_VAR (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_08 _1 _3 in
      _menhir_goto_policy _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_03 _1 in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_exp (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState12 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STRICT ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FIFO ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | FAIR ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CLSS _v ->
              _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | RBRACE ->
          let _1 = _v in
          let _v = _menhir_action_02 _1 in
          _menhir_goto_arglist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_arglist : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prog) _menhir_state -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState04 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState06 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState08 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_18 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_STRICT -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_STRICT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_05 _3 in
      _menhir_goto_policy _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_FIFO -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_FIFO (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_04 _3 in
      _menhir_goto_policy _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_14 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_FAIR -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_FAIR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_06 _3 in
      _menhir_goto_policy _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_prog) _menhir_cell1_exp -> _ -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_exp (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_01 _1 _3 in
      _menhir_goto_arglist _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prog =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STRICT ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FIFO ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | FAIR ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CLSS _v ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let prog =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prog v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
