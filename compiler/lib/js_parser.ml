
module Basics = struct
  
  exception Error
  
  type token = Js_token.token
  
end

include Basics

let _eRR =
  Basics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState499
  | MenhirState494
  | MenhirState492
  | MenhirState491
  | MenhirState490
  | MenhirState489
  | MenhirState487
  | MenhirState483
  | MenhirState478
  | MenhirState471
  | MenhirState468
  | MenhirState465
  | MenhirState463
  | MenhirState461
  | MenhirState460
  | MenhirState454
  | MenhirState451
  | MenhirState449
  | MenhirState447
  | MenhirState443
  | MenhirState440
  | MenhirState438
  | MenhirState435
  | MenhirState434
  | MenhirState429
  | MenhirState427
  | MenhirState423
  | MenhirState418
  | MenhirState416
  | MenhirState412
  | MenhirState409
  | MenhirState407
  | MenhirState406
  | MenhirState404
  | MenhirState402
  | MenhirState400
  | MenhirState397
  | MenhirState395
  | MenhirState393
  | MenhirState388
  | MenhirState387
  | MenhirState380
  | MenhirState378
  | MenhirState377
  | MenhirState369
  | MenhirState362
  | MenhirState358
  | MenhirState357
  | MenhirState353
  | MenhirState351
  | MenhirState350
  | MenhirState347
  | MenhirState345
  | MenhirState343
  | MenhirState341
  | MenhirState339
  | MenhirState337
  | MenhirState335
  | MenhirState333
  | MenhirState331
  | MenhirState329
  | MenhirState327
  | MenhirState325
  | MenhirState323
  | MenhirState321
  | MenhirState319
  | MenhirState317
  | MenhirState315
  | MenhirState312
  | MenhirState311
  | MenhirState310
  | MenhirState309
  | MenhirState308
  | MenhirState307
  | MenhirState306
  | MenhirState305
  | MenhirState304
  | MenhirState303
  | MenhirState302
  | MenhirState301
  | MenhirState300
  | MenhirState299
  | MenhirState298
  | MenhirState297
  | MenhirState296
  | MenhirState291
  | MenhirState276
  | MenhirState275
  | MenhirState273
  | MenhirState269
  | MenhirState267
  | MenhirState266
  | MenhirState265
  | MenhirState264
  | MenhirState263
  | MenhirState262
  | MenhirState261
  | MenhirState260
  | MenhirState258
  | MenhirState254
  | MenhirState252
  | MenhirState250
  | MenhirState248
  | MenhirState246
  | MenhirState244
  | MenhirState242
  | MenhirState240
  | MenhirState238
  | MenhirState236
  | MenhirState234
  | MenhirState232
  | MenhirState230
  | MenhirState226
  | MenhirState225
  | MenhirState224
  | MenhirState222
  | MenhirState220
  | MenhirState218
  | MenhirState216
  | MenhirState215
  | MenhirState214
  | MenhirState213
  | MenhirState210
  | MenhirState208
  | MenhirState206
  | MenhirState205
  | MenhirState204
  | MenhirState203
  | MenhirState202
  | MenhirState201
  | MenhirState199
  | MenhirState197
  | MenhirState196
  | MenhirState195
  | MenhirState194
  | MenhirState193
  | MenhirState190
  | MenhirState188
  | MenhirState186
  | MenhirState185
  | MenhirState182
  | MenhirState179
  | MenhirState178
  | MenhirState177
  | MenhirState176
  | MenhirState175
  | MenhirState171
  | MenhirState168
  | MenhirState166
  | MenhirState165
  | MenhirState164
  | MenhirState163
  | MenhirState161
  | MenhirState160
  | MenhirState159
  | MenhirState155
  | MenhirState153
  | MenhirState149
  | MenhirState148
  | MenhirState147
  | MenhirState146
  | MenhirState145
  | MenhirState144
  | MenhirState143
  | MenhirState142
  | MenhirState141
  | MenhirState140
  | MenhirState136
  | MenhirState132
  | MenhirState130
  | MenhirState128
  | MenhirState126
  | MenhirState124
  | MenhirState122
  | MenhirState120
  | MenhirState118
  | MenhirState114
  | MenhirState101
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState84
  | MenhirState82
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState52
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState40
  | MenhirState39
  | MenhirState34
  | MenhirState32
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState23
  | MenhirState22
  | MenhirState16
  | MenhirState15
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState9
  | MenhirState4
  | MenhirState3
  | MenhirState2
  | MenhirState0
  
(*
 * src: ocamlyaccified from Marcel Laverdet 'fbjs2' via emacs macros, itself
 * extracted from the official ECMAscript specification at:
 *  http://www.ecma-international.org/publications/standards/ecma-262.htm
 *
 * see also http://en.wikipedia.org/wiki/ECMAScript_syntax
 *
 * related work:
 *  - http://marijnhaverbeke.nl/parse-js/, js parser in common lisp
 *    (which has been since ported to javascript by nodejs people)
 *  - jslint
 *)

module J = Javascript
open Js_token

let var name = J.S {J.name;J.var=None}

(* This is need to fake menhir while using `--infer`. *)
let _tok = EOF Parse_info.zero


let rec _menhir_run136 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136

and _menhir_goto_expression_no_in : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_COMMA _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState412 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState412)
    | Js_token.T_SEMICOLON _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (J.expression))) = _menhir_stack in
        let _v : (J.expression option) =     ( Some x ) in
        _menhir_goto_option_expression_no_in_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_or_terminated_list_T_COMMA_object_key_value_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.property_name_and_value_list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RCURLY _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (pi2 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi1 : (Parse_info.t))), _, (x : (J.property_name_and_value_list))) = _menhir_stack in
            let _v : (J.property_name_and_value_list * Parse_info.t * Parse_info.t) =                                  ( (x, pi1, pi2) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (block : (J.property_name_and_value_list * Parse_info.t * Parse_info.t)) = _v in
            let _v : (Parse_info.t * J.expression) =    ( let pairs, pi_start, _pi_end = block in pi_start, J.EObj pairs ) in
            _menhir_goto_object_literal _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState478 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (x : (J.property_name * J.expression))), (_2 : (Parse_info.t))), _, (xs : (J.property_name_and_value_list))) = _menhir_stack in
        let _v : (J.property_name_and_value_list) =                                                                ( x :: xs ) in
        _menhir_goto_separated_or_terminated_list_T_COMMA_object_key_value_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_element_list_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression option list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_COMMA _v ->
        _menhir_run462 _menhir_env (Obj.magic _menhir_stack) MenhirState468 _v
    | Js_token.T_RBRACKET _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression option list))) = _menhir_stack in
        let _v : (J.array_litteral) =                     ( List.rev _1 ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.array_litteral))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( (pi, J.EArr _2) ) in
            _menhir_goto_array_literal _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState468

and _menhir_goto_separated_nonempty_list_T_COMMA_assignment_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.arguments) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (J.arguments)) = _v in
        let _v : (J.arguments) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_T_COMMA_assignment_expression__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (J.arguments)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (J.expression))), (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (J.arguments) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_T_COMMA_assignment_expression_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (Parse_info.t)) = _v in
            let (((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))), _, (_3 : (J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( let (start, e) = _1 in (start, J.EAccess (e, _3)) ) in
            _menhir_goto_call_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (Parse_info.t)) = _v in
            let (((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))), _, (e2 : (J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( let (start, e1) = _1 in (start, J.EAccess (e1,e2)) ) in
            _menhir_goto_member_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState163 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState163)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.expression))) = _menhir_stack in
            let _v : (J.statement * J.location) =                          ( (J.Throw_statement _2, J.Pi pi) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (s : (J.statement * J.location)) = _v in
            let _v : (J.statement * J.location) =                         ( s ) in
            _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_LCURLY _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_CASE _v ->
                    _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _v
                | Js_token.T_DEFAULT _ | Js_token.T_RCURLY _ ->
                    _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState185
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState188 _v
            | Js_token.T_CASE _ | Js_token.T_DEFAULT _ | Js_token.T_RCURLY _ ->
                _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState188
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState188)
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState400 | MenhirState402 | MenhirState393 | MenhirState395 | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (J.expression))) = _menhir_stack in
            let _v : (J.expression option) =     ( Some x ) in
            _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState210 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState260 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState291 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_30 : (Parse_info.t)) = _v in
            let (((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (body : (J.statement * J.location))), (_3 : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))) = _menhir_stack in
            let _v : (J.statement * J.location) = let condition =
              let _3 = _30 in
              let item = item0 in
              let _1 = _10 in
                                                                       ( item )
            in
                ( (J.Do_while_statement (body, condition), J.Pi pi) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (s : (J.statement * J.location)) = _v in
            let _v : (J.statement * J.location) =                         ( s ) in
            _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState353 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (Parse_info.t)) = _v in
            let (((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))), _, (e2 : (J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =    ( let (start, e1) = _1 in (start, J.EAccess(e1, e2)) ) in
            _menhir_goto_member_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState380 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (Parse_info.t)) = _v in
            let (((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))), _, (_3 : (J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =    ( let (start, e) = _1 in (start, J.EAccess(e, _3)) ) in
            _menhir_goto_call_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState407 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState409)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =                                      ( (pi, e) ) in
            _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState499 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.EOF _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (Parse_info.t)) = _v in
            let (_menhir_stack, _menhir_s, (e : (J.expression))) = _menhir_stack in
            let _v : (Javascript.expression) =                     ( e ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Javascript.expression)) = _v in
            Obj.magic _1
        | Js_token.T_COMMA _v ->
            _menhir_run136 _menhir_env (Obj.magic _menhir_stack) _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_COMMA _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState369)
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.statement * J.location) =                            ( J.Expression_statement _1, J.N ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                         ( s ) in
        _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_assignment_expression_no_in : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState226 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), _, (_2 : (J.binop))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =      ( J.EBin(_2,_1,_3) ) in
        _menhir_goto_assignment_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (condition : (J.expression))), (_2 : (Parse_info.t))), _, (consequence : (J.expression))), (_4 : (Parse_info.t))), _, (alternative : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.ECond (condition, consequence, alternative) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (J.expression)) = _v in
        let _v : (J.expression) =                                                                   ( _1 ) in
        _menhir_goto_conditional_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t))), _, (_2 : (J.expression))) = _menhir_stack in
        let _v : (J.initialiser) =                                         ( _2, J.Pi _1 ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (J.initialiser)) = _v in
        let _v : (J.initialiser option) =     ( Some x ) in
        _menhir_goto_option_initializer_no_in_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState412 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), (_2 : (Parse_info.t))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                                         ( J.ESeq (_1, _3) ) in
        _menhir_goto_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                ( _1 ) in
        _menhir_goto_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_assignment_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), _, (_2 : (J.binop))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.EBin (_2, _1, _3) ) in
        _menhir_goto_assignment_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (condition : (J.expression))), (_2 : (Parse_info.t))), _, (consequence : (J.expression))), (_4 : (Parse_info.t))), _, (alternative : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.ECond (condition, consequence, alternative) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (J.expression)) = _v in
        let _v : (J.expression) =                                                       ( _1 ) in
        _menhir_goto_conditional_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), (_2 : (Parse_info.t))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                             ( J.ESeq (_1, _3) ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState499 | MenhirState2 | MenhirState14 | MenhirState42 | MenhirState182 | MenhirState186 | MenhirState208 | MenhirState407 | MenhirState400 | MenhirState402 | MenhirState393 | MenhirState395 | MenhirState258 | MenhirState380 | MenhirState353 | MenhirState291 | MenhirState190 | MenhirState179 | MenhirState155 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                          ( _1 ) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState153 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState153 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState153)
        | Js_token.T_RPAREN _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (J.expression))) = _menhir_stack in
            let _v : (J.arguments) =     ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_T_COMMA_assignment_expression_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (pi : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
        let _v : (J.initialiser) =                                        ( e, J.Pi pi ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (J.initialiser)) = _v in
        let _v : (J.initialiser option) =     ( Some x ) in
        _menhir_goto_option_initializer__ _menhir_env _menhir_stack _v
    | MenhirState319 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState321 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState321)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState321 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (condition : (J.expression))), (_2 : (Parse_info.t))), _, (consequence : (J.expression))), (_4 : (Parse_info.t))), _, (alternative : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.ECond (condition, consequence, alternative) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (J.expression)) = _v in
        let _v : (J.expression) =                                                                    ( _1 ) in
        _menhir_goto_conditional_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState358 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), _, (_2 : (J.binop))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.EBin (_2,_1,_3) ) in
        _menhir_goto_assignment_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState369 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression))), (_2 : (Parse_info.t))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                                          ( J.ESeq(_1,_3) ) in
        _menhir_goto_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState463 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (J.array_litteral))), _, (_2 : (J.expression))) = _menhir_stack in
        let _v : (J.expression option list) =                                     ( (Some _2)::_1 ) in
        _menhir_goto_element_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState471 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (_1 : (J.expression option list))), _, (_2 : (J.array_litteral))), _, (_3 : (J.expression))) = _menhir_stack in
        let _v : (J.expression option list) =                                                  ( (Some _3) :: (List.rev_append _2 _1) ) in
        _menhir_goto_element_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression option list) =                                     ( [Some _1] ) in
        _menhir_goto_element_list_rev _menhir_env _menhir_stack _menhir_s _v
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (x0 : (J.property_name))), (_20 : (Parse_info.t))), _, (y0 : (J.expression))) = _menhir_stack in
        let _v : (J.property_name * J.expression) = let pair =
          let y = y0 in
          let _2 = _20 in
          let x = x0 in
              ( (x, y) )
        in
                                                                              ( pair ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState478 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState478 _v
            | Js_token.T_STRING _v ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState478 _v
            | Js_token.T_RCURLY _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (x : (J.property_name * J.expression))), (_2 : (Parse_info.t))) = _menhir_stack in
                let _v : (J.property_name_and_value_list) =                  ( [x] ) in
                _menhir_goto_separated_or_terminated_list_T_COMMA_object_key_value_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState478)
        | Js_token.T_RCURLY _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (J.property_name * J.expression))) = _menhir_stack in
            let _v : (J.property_name_and_value_list) =        ( [x] ) in
            _menhir_goto_separated_or_terminated_list_T_COMMA_object_key_value_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_assignment_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (J.expression)) = _v in
    let _v : (J.expression) =                                       ( _1 ) in
    _menhir_goto_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_conditional_expression_no_in : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (J.expression)) = _v in
    let _v : (J.expression) =                                 ( _1 ) in
    _menhir_goto_assignment_expression_no_in _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_conditional_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (J.expression)) = _v in
    let _v : (J.expression) =                           ( _1 ) in
    _menhir_goto_assignment_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_run98 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_run122 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122

and _menhir_run84 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState84 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84

and _menhir_run88 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88

and _menhir_run90 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run92 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run94 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run96 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run124 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124

and _menhir_run126 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126

and _menhir_run130 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130

and _menhir_run128 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_run132 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132

and _menhir_goto_conditional_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (J.expression)) = _v in
    let _v : (J.expression) =                                        ( _1 ) in
    _menhir_goto_assignment_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_postfix_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.unop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState492 | MenhirState491 | MenhirState490 | MenhirState489 | MenhirState483 | MenhirState461 | MenhirState460 | MenhirState312 | MenhirState310 | MenhirState308 | MenhirState306 | MenhirState304 | MenhirState302 | MenhirState300 | MenhirState298 | MenhirState276 | MenhirState267 | MenhirState265 | MenhirState263 | MenhirState218 | MenhirState206 | MenhirState204 | MenhirState202 | MenhirState196 | MenhirState194 | MenhirState176 | MenhirState165 | MenhirState161 | MenhirState160 | MenhirState159 | MenhirState66 | MenhirState149 | MenhirState147 | MenhirState68 | MenhirState145 | MenhirState70 | MenhirState143 | MenhirState141 | MenhirState72 | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (op : (J.unop)) = _v in
        let (_menhir_stack, _menhir_s, (e : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.EUn (op, e) ) in
        _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (op : (J.unop)) = _v in
        let (_menhir_stack, _menhir_s, (e : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( J.EUn (op, e) ) in
        _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_post_in_expression_no_in : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_AND _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState254 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254)
    | Js_token.T_BIT_AND _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState252 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState252)
    | Js_token.T_BIT_OR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState250 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250)
    | Js_token.T_BIT_XOR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState248 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248)
    | Js_token.T_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246)
    | Js_token.T_GREATER_THAN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState244 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState244)
    | Js_token.T_GREATER_THAN_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState242 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242)
    | Js_token.T_INSTANCEOF _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState240 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240)
    | Js_token.T_LESS_THAN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238)
    | Js_token.T_LESS_THAN_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState236 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState236)
    | Js_token.T_NOT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState234 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState234)
    | Js_token.T_OR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232)
    | Js_token.T_PLING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224)
    | Js_token.T_STRICT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222)
    | Js_token.T_STRICT_NOT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220)
    | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_IN _ | Js_token.T_SEMICOLON _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                             ( _1 ) in
        _menhir_goto_conditional_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_post_in_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState499 | MenhirState2 | MenhirState14 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState407 | MenhirState402 | MenhirState400 | MenhirState395 | MenhirState393 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState321 | MenhirState319 | MenhirState291 | MenhirState258 | MenhirState208 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState168 | MenhirState42 | MenhirState155 | MenhirState153 | MenhirState63 | MenhirState136 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_AND _v ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_OR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
        | Js_token.T_PLING _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
            let _v : (J.expression) =                       ( _1 ) in
            _menhir_goto_conditional_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.NotEqEq    )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Le         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Lt         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.InstanceOf )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                  ( J.In )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Ge         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Gt         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.EqEqEq     )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_AND _v ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Or         )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.NotEq      )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.EqEq       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Bxor       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Band       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Bor        )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.And        )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_EQUAL _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.NotEqEq    )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_EQUAL _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.EqEqEq     )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_AND _v ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.Or         )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState234 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_EQUAL _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.NotEq      )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                                  ( J.Le         )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                                  ( J.Lt         )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                                  ( J.InstanceOf )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                                  ( J.Ge         )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                                  ( J.Gt         )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_EQUAL _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.EqEq       )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.Bxor       )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_OR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.Bor        )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.Band       )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_AND _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                                      ( J.And        )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.NotEqEq    )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState317 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.EqEqEq     )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_AND _v ->
            _menhir_run132 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Or         )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState325 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.NotEq      )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Le         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Lt         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState331 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.InstanceOf )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState333 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _1 = _10 in
                  ( J.In )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState335 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Ge         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState337 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
        let _v : (J.expression) = let op =
          let _10 = _100 in
          let op =
            let _1 = _10 in
                                    ( J.Gt         )
          in
                                                                ( op )
        in
           ( J.EBin (op, left, right) ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState339 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.EqEq       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Bxor       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState343 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Bor        )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState345 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.Band       )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState347 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_AND _v ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_OR _v ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_BIT_XOR _v ->
            _menhir_run126 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_EQUAL _v ->
            _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN _v ->
            _menhir_run96 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_GREATER_THAN_EQUAL _v ->
            _menhir_run94 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            _menhir_run92 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_INSTANCEOF _v ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN _v ->
            _menhir_run88 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_LESS_THAN_EQUAL _v ->
            _menhir_run84 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_NOT_EQUAL _v ->
            _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_EQUAL _v ->
            _menhir_run98 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_STRICT_NOT_EQUAL _v ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), (_100 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _10 = _100 in
              let op =
                let _1 = _10 in
                                        ( J.And        )
              in
                                                                    ( op )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run146 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run140 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140

and _menhir_run144 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144

and _menhir_run148 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148

and _menhir_run142 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142

and _menhir_goto_option_pair_default_clause_list_case_clause___ : _menhir_env -> 'ttv_tail -> ((J.statement_list * J.case_clause list) option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_RCURLY _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_5 : (Parse_info.t)) = _v in
        let (((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))), (_30 : (Parse_info.t))), (_3 : (Parse_info.t))), _, (x0 : (J.case_clause list))), (y0 : ((J.statement_list * J.case_clause list) option))) = _menhir_stack in
        let _v : (J.statement * J.location) = let pair =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
        let subject =
          let _3 = _30 in
          let item = item0 in
          let _1 = _10 in
                                                                   ( item )
        in
           ( let switch = match pair with
       | cases, None ->
         J.Switch_statement (subject, cases, None, [])
       | cases, Some (default, more_cases) ->
         J.Switch_statement (subject, cases, Some default, more_cases)
      in switch, J.Pi pi ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                      ( s ) in
        _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_post_in_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_AND _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState347)
    | Js_token.T_BIT_AND _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState345 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState345)
    | Js_token.T_BIT_OR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState343)
    | Js_token.T_BIT_XOR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState341)
    | Js_token.T_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState339)
    | Js_token.T_GREATER_THAN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState337)
    | Js_token.T_GREATER_THAN_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState335 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState335)
    | Js_token.T_IN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState333)
    | Js_token.T_INSTANCEOF _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState331 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState331)
    | Js_token.T_LESS_THAN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329)
    | Js_token.T_LESS_THAN_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState327 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState327)
    | Js_token.T_NOT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState325)
    | Js_token.T_OR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState323 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState323)
    | Js_token.T_PLING _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState319 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState319)
    | Js_token.T_STRICT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState317 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState317)
    | Js_token.T_STRICT_NOT_EQUAL _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState315 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState315)
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                    ( _1 ) in
        _menhir_goto_conditional_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.unop) =              ( J.IncrA ) in
    _menhir_goto_postfix_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.unop) =              ( J.DecrA ) in
    _menhir_goto_postfix_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_assignment_operator : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.binop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
    | MenhirState406 | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState226 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState226)
    | MenhirState357 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState358)
    | _ ->
        _menhir_fail ()

and _menhir_goto_pre_in_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Bnot   )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState499 | MenhirState2 | MenhirState14 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState407 | MenhirState402 | MenhirState400 | MenhirState395 | MenhirState393 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState321 | MenhirState319 | MenhirState317 | MenhirState315 | MenhirState291 | MenhirState258 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState222 | MenhirState220 | MenhirState208 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState168 | MenhirState42 | MenhirState155 | MenhirState153 | MenhirState136 | MenhirState132 | MenhirState130 | MenhirState128 | MenhirState126 | MenhirState124 | MenhirState122 | MenhirState120 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_LSHIFT _v ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_RSHIFT _v ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.T_RSHIFT3 _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
            let _v : (J.expression) =                      ( _1 ) in
            _menhir_goto_post_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Lsr   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Plus  )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Mul   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState141 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Mod   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState141)
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState143 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Div   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Minus )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Asr   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Lsl   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.DecrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.DecrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Delete )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161)
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState165 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Void   )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState165)
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Typeof )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176)
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState194 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Pl     )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState194)
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState196 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Not    )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState196)
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState202 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Neg    )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState202)
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState204 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.IncrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState204)
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState206 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.IncrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState206)
    | MenhirState412 | MenhirState213 | MenhirState230 | MenhirState226 | MenhirState224 | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_LSHIFT _v ->
            _menhir_run148 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_RSHIFT _v ->
            _menhir_run146 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_RSHIFT3 _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState218 _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_EQUAL _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IN _ | Js_token.T_INSTANCEOF _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
            let _v : (J.expression) =                      ( _1 ) in
            _menhir_goto_post_in_expression_no_in _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState218)
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState263 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState263 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Delete )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState263)
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState265 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.DecrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState265)
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState267 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.DecrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267)
    | MenhirState275 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState276 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Bnot   )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState276)
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState298 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Lsr   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState298)
    | MenhirState299 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState300 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Asr   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState300)
    | MenhirState301 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState302 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Plus  )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302)
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState304 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Mul   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState304)
    | MenhirState305 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState306 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState306 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Mod   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState306)
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState308 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState308 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState308 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState308 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState308 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Minus )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState308)
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_MINUS _v ->
            _menhir_run144 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.T_PLUS _v ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState310 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Lsl   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState310)
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState312 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState312 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (left : (J.expression))), _, (_10 : (Parse_info.t))), _, (right : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Div   )
            in
               ( J.EBin (op, left, right) ) in
            _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState312)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState460 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState460 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.IncrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState460)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState461 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState461 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.IncrB  )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState461)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Neg    )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState483)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState489 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState489 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Not    )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState489)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState490 _v
        | Js_token.T_DIV _v ->
            _menhir_run142 _menhir_env (Obj.magic _menhir_stack) MenhirState490 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState490 _v
        | Js_token.T_MOD _v ->
            _menhir_run140 _menhir_env (Obj.magic _menhir_stack) MenhirState490 _v
        | Js_token.T_MULT _v ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState490 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Pl     )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState490)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Typeof )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState491)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DECR_NB _v ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState492 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState492 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (e : (J.expression))) = _menhir_stack in
            let _v : (J.expression) = let op =
              let _1 = _10 in
                           ( J.Void   )
            in
               ( J.EUn (op, e) ) in
            _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState492)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer_no_in___ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.variable_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState387 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, (x0 : (J.ident))), _, (y0 : (J.initialiser option))), (_2 : (Parse_info.t))), _, (xs : (J.variable_declaration list))) = _menhir_stack in
        let _v : (J.variable_declaration list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer_no_in___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_SEMICOLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _v
            | Js_token.T_SEMICOLON _ ->
                _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack) MenhirState393
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState393)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_case_clause_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.case_clause list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_DEFAULT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_COLON _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_BIT_NOT _v ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_BREAK _v ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_CONTINUE _v ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_DEBUGGER _v ->
                    _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_DECR _v ->
                    _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_DECR_NB _v ->
                    _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_DELETE _v ->
                    _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_DO _v ->
                    _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_FALSE _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_FOR _v ->
                    _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_IF _v ->
                    _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_INCR _v ->
                    _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_INCR_NB _v ->
                    _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_LBRACKET _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_LCURLY _v ->
                    _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_LPAREN _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_MINUS _v ->
                    _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_NEW _v ->
                    _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_NOT _v ->
                    _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_NULL _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_NUMBER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_PLUS _v ->
                    _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_REGEX _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_RETURN _v ->
                    _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_SEMICOLON _v ->
                    _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_STRING _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_SWITCH _v ->
                    _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_THIS _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_THROW _v ->
                    _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_TRUE _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_TRY _v ->
                    _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_TYPEOF _v ->
                    _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_VAR _v ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_VOID _v ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_WHILE _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_WITH _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState423 _v
                | Js_token.T_CASE _ | Js_token.T_RCURLY _ ->
                    _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState423
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState423)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | Js_token.T_RCURLY _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : ((J.statement_list * J.case_clause list) option) =     ( None ) in
            _menhir_goto_option_pair_default_clause_list_case_clause___ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState427 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, (x0 : (J.statement_list))), _, (y0 : (J.case_clause list))) = _menhir_stack in
        let _v : ((J.statement_list * J.case_clause list) option) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( Some x ) in
        _menhir_goto_option_pair_default_clause_list_case_clause___ _menhir_env _menhir_stack _v
    | MenhirState429 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (J.case_clause))), _, (xs : (J.case_clause list))) = _menhir_stack in
        let _v : (J.case_clause list) =     ( x :: xs ) in
        _menhir_goto_list_case_clause_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_pre_in_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_DECR_NB _v ->
        _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | Js_token.T_DIV _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState311 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState311)
    | Js_token.T_INCR_NB _v ->
        _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState296 _v
    | Js_token.T_LSHIFT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState309 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState309)
    | Js_token.T_MINUS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState307 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState307)
    | Js_token.T_MOD _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState305 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState305)
    | Js_token.T_MULT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState303 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState303)
    | Js_token.T_PLUS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState301)
    | Js_token.T_RSHIFT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState299)
    | Js_token.T_RSHIFT3 _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState296 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState297 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState297)
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                   ( _1 ) in
        _menhir_goto_post_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState296

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.AsrEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run103 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.LsrEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run104 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.PlusEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.StarEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run106 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.ModEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.MinusEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run108 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.LslEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.SlashEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.BxorEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run111 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.BorEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.BandEq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_run113 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.binop) =                     ( J.Eq ) in
    _menhir_goto_assignment_operator _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce181 : _menhir_env -> 'ttv_tail * _menhir_state * (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
    let _v : (J.expression) =    ( _1 ) in
    _menhir_goto_pre_in_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_new_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState499 | MenhirState2 | MenhirState3 | MenhirState4 | MenhirState9 | MenhirState12 | MenhirState14 | MenhirState15 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState25 | MenhirState26 | MenhirState412 | MenhirState407 | MenhirState213 | MenhirState400 | MenhirState402 | MenhirState393 | MenhirState395 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState321 | MenhirState319 | MenhirState317 | MenhirState315 | MenhirState311 | MenhirState309 | MenhirState307 | MenhirState305 | MenhirState303 | MenhirState301 | MenhirState299 | MenhirState297 | MenhirState291 | MenhirState275 | MenhirState266 | MenhirState264 | MenhirState262 | MenhirState258 | MenhirState216 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState230 | MenhirState226 | MenhirState224 | MenhirState222 | MenhirState220 | MenhirState208 | MenhirState205 | MenhirState203 | MenhirState201 | MenhirState195 | MenhirState193 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState175 | MenhirState168 | MenhirState164 | MenhirState42 | MenhirState44 | MenhirState45 | MenhirState46 | MenhirState155 | MenhirState153 | MenhirState63 | MenhirState148 | MenhirState146 | MenhirState67 | MenhirState144 | MenhirState69 | MenhirState142 | MenhirState140 | MenhirState136 | MenhirState80 | MenhirState120 | MenhirState132 | MenhirState130 | MenhirState126 | MenhirState128 | MenhirState124 | MenhirState122 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState98 | MenhirState82 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState71 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Parse_info.t * J.expression)) = _v in
        let _v : (J.expression) =                    ( snd _1 ) in
        _menhir_goto_left_hand_side_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Parse_info.t * J.expression)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =                            ( (pi, J.ENew (snd _2,None)) ) in
        _menhir_goto_new_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Parse_info.t * J.expression)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =                            ( (pi, J.ENew (snd _2,None)) ) in
        _menhir_goto_new_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce245 : _menhir_env -> ('ttv_tail * _menhir_state * (J.ident)) * _menhir_state * (J.initialiser option) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let ((_menhir_stack, _menhir_s, (x0 : (J.ident))), _, (y0 : (J.initialiser option))) = _menhir_stack in
    let _v : (J.variable_declaration list) = let x =
      let y = y0 in
      let x = x0 in
          ( (x, y) )
    in
        ( [ x ] ) in
    _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer_no_in___ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run387 : _menhir_env -> ('ttv_tail * _menhir_state * (J.ident)) * _menhir_state * (J.initialiser option) -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState387 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState387

and _menhir_goto_loption_separated_nonempty_list_T_COMMA_assignment_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.arguments) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_RPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_30 : (Parse_info.t)) = _v in
        let ((_menhir_stack, _menhir_s, (_10 : (Parse_info.t))), _, (xs00 : (J.arguments))) = _menhir_stack in
        let _v : (J.arguments) = let args =
          let _3 = _30 in
          let xs0 = xs00 in
          let _1 = _10 in
          let item =
            let xs = xs0 in
                ( xs )
          in
                                                                   ( item )
        in
                                                                              ( args ) in
        (match _menhir_s with
        | MenhirState77 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (J.arguments)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( let (start, e) = _1 in (start, J.ECall(e, _2, J.Pi start)) ) in
            _menhir_goto_call_expression _menhir_env _menhir_stack _menhir_s _v
        | MenhirState60 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (J.arguments)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( let (start, e) = _1 in (start, J.ECall(e, _2, J.Pi start)) ) in
            _menhir_goto_call_expression _menhir_env _menhir_stack _menhir_s _v
        | MenhirState199 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (a : (J.arguments)) = _v in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (e : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =    ( (pi, J.ENew(snd e,Some a)) ) in
            _menhir_goto_member_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | MenhirState350 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (J.arguments)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =    ( let (start, e) = _1 in (start, J.ECall(e, _2, J.Pi start)) ) in
            _menhir_goto_call_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | MenhirState377 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (J.arguments)) = _v in
            let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =    ( let (start, e) = _1 in (start, J.ECall(e, _2, J.Pi start)) ) in
            _menhir_goto_call_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
        | MenhirState487 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (a : (J.arguments)) = _v in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (e1 : (Parse_info.t * J.expression))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( (pi, J.ENew(snd e1, Some a)) ) in
            _menhir_goto_member_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run467 : _menhir_env -> 'ttv_tail * _menhir_state * (J.array_litteral) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (Parse_info.t)) = _v in
    let (_menhir_stack, _menhir_s, (_1 : (J.array_litteral))) = _menhir_stack in
    let _v : (J.array_litteral) =                   ( None :: _1 ) in
    _menhir_goto_elison_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_BREAK _v ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_CONTINUE _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_DEBUGGER _v ->
        _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_DECR _v ->
        _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_DELETE _v ->
        _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_DO _v ->
        _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_FOR _v ->
        _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run447 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_IF _v ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_INCR _v ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_MINUS _v ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_NEW _v ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_NOT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_PLUS _v ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_RETURN _v ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_SEMICOLON _v ->
        _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_SWITCH _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_THROW _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_TRY _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_VAR _v ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_VOID _v ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_WITH _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | Js_token.T_RCURLY _ ->
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_goto_option_finally_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.block option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_4 : (J.block option)) = _v in
    let (((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.block))), _, (_3 : (J.ident * J.block))) = _menhir_stack in
    let _v : (J.statement * J.location) =                                  ( (J.Try_statement (_2, Some _3, _4), J.Pi pi) ) in
    _menhir_goto_try_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_try_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                      ( s ) in
    _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_run435 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState435 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState435

and _menhir_reduce89 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.case_clause list) =     ( [] ) in
    _menhir_goto_list_case_clause_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run186 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState186 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState186

and _menhir_goto_left_hand_side_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_ASSIGN _v ->
        _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_BIT_AND_ASSIGN _v ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_BIT_OR_ASSIGN _v ->
        _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_BIT_XOR_ASSIGN _v ->
        _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_DIV_ASSIGN _v ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_LSHIFT_ASSIGN _v ->
        _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_MINUS_ASSIGN _v ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_MOD_ASSIGN _v ->
        _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_MULT_ASSIGN _v ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_PLUS_ASSIGN _v ->
        _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_RSHIFT3_ASSIGN _v ->
        _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.T_RSHIFT_ASSIGN _v ->
        _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState357 _v
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.expression))) = _menhir_stack in
        let _v : (J.expression) =    ( _1 ) in
        _menhir_goto_pre_in_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState357

and _menhir_goto_left_hand_side_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 | MenhirState4 | MenhirState9 | MenhirState12 | MenhirState15 | MenhirState25 | MenhirState26 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState317 | MenhirState315 | MenhirState311 | MenhirState309 | MenhirState307 | MenhirState305 | MenhirState303 | MenhirState301 | MenhirState299 | MenhirState297 | MenhirState275 | MenhirState266 | MenhirState264 | MenhirState262 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState222 | MenhirState220 | MenhirState205 | MenhirState203 | MenhirState201 | MenhirState195 | MenhirState193 | MenhirState175 | MenhirState164 | MenhirState44 | MenhirState45 | MenhirState46 | MenhirState47 | MenhirState148 | MenhirState146 | MenhirState67 | MenhirState144 | MenhirState69 | MenhirState142 | MenhirState140 | MenhirState120 | MenhirState132 | MenhirState130 | MenhirState126 | MenhirState128 | MenhirState124 | MenhirState122 | MenhirState98 | MenhirState82 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState499 | MenhirState2 | MenhirState14 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState407 | MenhirState400 | MenhirState402 | MenhirState393 | MenhirState395 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState321 | MenhirState319 | MenhirState291 | MenhirState258 | MenhirState208 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState168 | MenhirState42 | MenhirState155 | MenhirState153 | MenhirState63 | MenhirState136 | MenhirState80 | MenhirState118 | MenhirState114 | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_ASSIGN _v ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_BIT_AND_ASSIGN _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_BIT_OR_ASSIGN _v ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_BIT_XOR_ASSIGN _v ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_DIV_ASSIGN _v ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_LSHIFT_ASSIGN _v ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_MINUS_ASSIGN _v ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_MOD_ASSIGN _v ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_MULT_ASSIGN _v ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_PLUS_ASSIGN _v ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_RSHIFT3_ASSIGN _v ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.T_RSHIFT_ASSIGN _v ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
    | MenhirState412 | MenhirState216 | MenhirState230 | MenhirState226 | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_ASSIGN _v ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_BIT_AND_ASSIGN _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_BIT_OR_ASSIGN _v ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_BIT_XOR_ASSIGN _v ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_DIV_ASSIGN _v ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_LSHIFT_ASSIGN _v ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_MINUS_ASSIGN _v ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_MOD_ASSIGN _v ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_MULT_ASSIGN _v ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_PLUS_ASSIGN _v ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_RSHIFT3_ASSIGN _v ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_RSHIFT_ASSIGN _v ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState225 _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_DECR_NB _ | Js_token.T_DIV _ | Js_token.T_EQUAL _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IN _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState225)
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_ASSIGN _v ->
            _menhir_run113 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_BIT_AND_ASSIGN _v ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_BIT_OR_ASSIGN _v ->
            _menhir_run111 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_BIT_XOR_ASSIGN _v ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_DIV_ASSIGN _v ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_IN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState406 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState407)
        | Js_token.T_LSHIFT_ASSIGN _v ->
            _menhir_run108 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_MINUS_ASSIGN _v ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_MOD_ASSIGN _v ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_MULT_ASSIGN _v ->
            _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_PLUS_ASSIGN _v ->
            _menhir_run104 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_RSHIFT3_ASSIGN _v ->
            _menhir_run103 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_RSHIFT_ASSIGN _v ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState406 _v
        | Js_token.T_AND _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_XOR _ | Js_token.T_COMMA _ | Js_token.T_DECR_NB _ | Js_token.T_DIV _ | Js_token.T_EQUAL _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_MINUS _ | Js_token.T_MOD _ | Js_token.T_MULT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ ->
            _menhir_reduce181 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState406)
    | _ ->
        _menhir_fail ()

and _menhir_reduce107 : _menhir_env -> 'ttv_tail * _menhir_state * (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
    let _v : (Parse_info.t * J.expression) =                         ( _1 ) in
    _menhir_goto_new_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Parse_info.t * J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run155 : _menhir_env -> 'ttv_tail * _menhir_state * (Parse_info.t * J.expression) -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState155 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState155

and _menhir_goto_option_initializer_no_in_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.initialiser option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run387 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_IN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState258 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState258)
        | Js_token.T_SEMICOLON _ ->
            _menhir_reduce245 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState388 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run387 _menhir_env (Obj.magic _menhir_stack) _v
        | Js_token.T_SEMICOLON _ ->
            _menhir_reduce245 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer____ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.variable_declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (J.variable_declaration list)) = _v in
        let (((_menhir_stack, _menhir_s, (x0 : (J.ident))), (y0 : (J.initialiser option))), (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (J.variable_declaration list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer____ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (list : (J.variable_declaration list)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (J.statement * J.location) =    ( J.Variable_statement list, J.Pi pi ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                         ( s ) in
        _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_if_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                      ( s ) in
    _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_for_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                      ( s ) in
    _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_for_in_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                      ( s ) in
    _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_new_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t * J.expression)) = _v in
    let _v : (J.expression) =                                ( snd _1 ) in
    _menhir_goto_left_hand_side_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | Js_token.T_RPAREN _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState63 in
        let _v : (J.arguments) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_T_COMMA_assignment_expression__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_reduce59 : _menhir_env -> 'ttv_tail * _menhir_state * (J.array_litteral) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (J.array_litteral))) = _menhir_stack in
    let _v : (J.array_litteral) =                    (_1) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_COMMA _v ->
            _menhir_run467 _menhir_env (Obj.magic _menhir_stack) MenhirState465 _v
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState465 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_3 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.array_litteral))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( (pi, J.EArr _2) ) in
            _menhir_goto_array_literal _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState465)
    | MenhirState468 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_COMMA _v ->
            _menhir_run467 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState471)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_T_COMMA_variable__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.formal_parameter_list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_LCURLY _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState449 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_LCURLY _v ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState451 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState451)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_source_element : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.source_element * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_BREAK _v ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_CONTINUE _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_DEBUGGER _v ->
        _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_DECR _v ->
        _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_DELETE _v ->
        _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_DO _v ->
        _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_FOR _v ->
        _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run447 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_IF _v ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_INCR _v ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_MINUS _v ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_NEW _v ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_NOT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_PLUS _v ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_RETURN _v ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_SEMICOLON _v ->
        _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_SWITCH _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_THROW _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_TRY _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_VAR _v ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_VOID _v ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.T_WITH _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState454 _v
    | Js_token.EOF _ | Js_token.T_RCURLY _ ->
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState454
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState454

and _menhir_goto_option_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.expression option))) = _menhir_stack in
        let _v : (J.statement * J.location) =                            ( (J.Return_statement _2, J.Pi pi) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                         ( s ) in
        _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
    | MenhirState393 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_SEMICOLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState395 _v
            | Js_token.T_RPAREN _ ->
                _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack) MenhirState395
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState395)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState395 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState397 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState397)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState400 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_SEMICOLON _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_DECR _v ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_DELETE _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_FUNCTION _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_INCR _v ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_MINUS _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_NEW _v ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_NOT _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_PLUS _v ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_VOID _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState402 _v
            | Js_token.T_RPAREN _ ->
                _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack) MenhirState402
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState402)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState402 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RPAREN _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState404 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState404)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement_list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState418 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (J.statement * J.location))), _, (xs : (J.statement_list))) = _menhir_stack in
        let _v : (J.statement_list) =     ( x :: xs ) in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s, (_1 : (Parse_info.t))), _, (x0 : (J.expression))), (_20 : (Parse_info.t))), _, (y0 : (J.statement_list))) = _menhir_stack in
        let _v : (J.case_clause) = let pair =
          let y = y0 in
          let _2 = _20 in
          let x = x0 in
              ( (x, y) )
        in
                                                                       ( pair ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_CASE _v ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState429 _v
        | Js_token.T_DEFAULT _ | Js_token.T_RCURLY _ ->
            _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState429
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState429)
    | MenhirState423 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, (_1 : (Parse_info.t))), (_2 : (Parse_info.t))), _, (list : (J.statement_list))) = _menhir_stack in
        let _v : (J.statement_list) =                                      ( list ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_CASE _v ->
            _menhir_run186 _menhir_env (Obj.magic _menhir_stack) MenhirState427 _v
        | Js_token.T_RCURLY _ ->
            _menhir_reduce89 _menhir_env (Obj.magic _menhir_stack) MenhirState427
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState427)
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RCURLY _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (pi2 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi1 : (Parse_info.t))), _, (x : (J.statement_list))) = _menhir_stack in
            let _v : (J.block * Parse_info.t * Parse_info.t) =                                  ( (x, pi1, pi2) ) in
            (match _menhir_s with
            | MenhirState0 | MenhirState494 | MenhirState40 | MenhirState454 | MenhirState163 | MenhirState178 | MenhirState423 | MenhirState188 | MenhirState418 | MenhirState210 | MenhirState416 | MenhirState409 | MenhirState404 | MenhirState397 | MenhirState260 | MenhirState261 | MenhirState362 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (block : (J.block * Parse_info.t * Parse_info.t)) = _v in
                let _v : (J.statement * J.location) =    ( let statements, pi_start, _pi_end = block in
     J.Block statements, J.Pi pi_start ) in
                _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v
            | MenhirState440 | MenhirState435 | MenhirState177 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (block : (J.block * Parse_info.t * Parse_info.t)) = _v in
                let _v : (J.block) =    ( let statements, _, _ = block in statements ) in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                (match _menhir_s with
                | MenhirState177 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Js_token.T_CATCH _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState434 in
                        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Js_token.T_LPAREN _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Js_token.T_IDENTIFIER _v ->
                                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState438 _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState438)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | Js_token.T_FINALLY _v ->
                        _menhir_run435 _menhir_env (Obj.magic _menhir_stack) MenhirState434 _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState434)
                | MenhirState435 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t))), _, (_2 : (J.block))) = _menhir_stack in
                    let _v : (J.block) =                    ( _2 ) in
                    (match _menhir_s with
                    | MenhirState434 ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_3 : (J.block)) = _v in
                        let ((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.block))) = _menhir_stack in
                        let _v : (J.statement * J.location) =                                 ( (J.Try_statement (_2, None, Some _3), J.Pi pi) ) in
                        _menhir_goto_try_statement _menhir_env _menhir_stack _menhir_s _v
                    | MenhirState443 ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (x : (J.block)) = _v in
                        let _v : (J.block option) =     ( Some x ) in
                        _menhir_goto_option_finally_ _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        _menhir_fail ())
                | MenhirState440 ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s, (_1 : (Parse_info.t))), (_100 : (Parse_info.t))), _, (item00 : (J.ident))), (_300 : (Parse_info.t))), _, (y0 : (J.block))) = _menhir_stack in
                    let _v : (J.ident * J.block) = let pair =
                      let y = y0 in
                      let _30 = _300 in
                      let item0 = item00 in
                      let _10 = _100 in
                      let x =
                        let _3 = _30 in
                        let item = item0 in
                        let _1 = _10 in
                                                                                 ( item )
                      in
                          ( (x, y) )
                    in
                                                                         ( pair ) in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Js_token.T_FINALLY _v ->
                        _menhir_run435 _menhir_env (Obj.magic _menhir_stack) MenhirState443 _v
                    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_s = MenhirState443 in
                        let _v : (J.block option) =     ( None ) in
                        _menhir_goto_option_finally_ _menhir_env _menhir_stack _menhir_s _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState443)
                | _ ->
                    _menhir_fail ())
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_call_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LBRACKET _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState377 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState380 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState380)
    | Js_token.T_LPAREN _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState377 _v
    | Js_token.T_PERIOD _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState377 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState378 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState378)
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
        let _v : (J.expression) =                                 ( snd _1 ) in
        _menhir_goto_left_hand_side_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState377

and _menhir_goto_call_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LBRACKET _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState77 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState80 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | Js_token.T_LPAREN _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | Js_token.T_PERIOD _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState77 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
        let _v : (J.expression) =                    ( snd _1 ) in
        _menhir_goto_left_hand_side_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_goto_member_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState499 | MenhirState2 | MenhirState3 | MenhirState4 | MenhirState9 | MenhirState12 | MenhirState14 | MenhirState15 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState25 | MenhirState26 | MenhirState412 | MenhirState407 | MenhirState213 | MenhirState400 | MenhirState402 | MenhirState393 | MenhirState395 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState321 | MenhirState319 | MenhirState317 | MenhirState315 | MenhirState311 | MenhirState309 | MenhirState307 | MenhirState305 | MenhirState303 | MenhirState301 | MenhirState299 | MenhirState297 | MenhirState291 | MenhirState275 | MenhirState266 | MenhirState264 | MenhirState262 | MenhirState258 | MenhirState216 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState230 | MenhirState226 | MenhirState224 | MenhirState222 | MenhirState220 | MenhirState208 | MenhirState205 | MenhirState203 | MenhirState201 | MenhirState195 | MenhirState193 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState175 | MenhirState168 | MenhirState164 | MenhirState42 | MenhirState44 | MenhirState45 | MenhirState46 | MenhirState155 | MenhirState153 | MenhirState63 | MenhirState148 | MenhirState146 | MenhirState67 | MenhirState144 | MenhirState69 | MenhirState142 | MenhirState140 | MenhirState136 | MenhirState80 | MenhirState120 | MenhirState132 | MenhirState130 | MenhirState126 | MenhirState128 | MenhirState124 | MenhirState122 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState98 | MenhirState82 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState71 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_LBRACKET _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | Js_token.T_PERIOD _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_LBRACKET _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | Js_token.T_PERIOD _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState199 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState199)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_LBRACKET _v ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) MenhirState487 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState487 _v
        | Js_token.T_PERIOD _v ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState487 _v
        | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COLON _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RBRACKET _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RPAREN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            _menhir_reduce107 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState487)
    | _ ->
        _menhir_fail ()

and _menhir_reduce124 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.initialiser option) =     ( None ) in
    _menhir_goto_option_initializer_no_in_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run216 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState216 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState216

and _menhir_goto_option_initializer__ : _menhir_env -> 'ttv_tail -> (J.initialiser option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_COMMA _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState171 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState171)
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x0 : (J.ident))), (y0 : (J.initialiser option))) = _menhir_stack in
        let _v : (J.variable_declaration list) = let x =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
            ( [ x ] ) in
        _menhir_goto_separated_nonempty_list_T_COMMA_pair_variable_option_initializer____ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_separated_nonempty_list_T_COMMA_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.formal_parameter_list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (J.formal_parameter_list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (J.ident))), (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (J.formal_parameter_list) =     ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_T_COMMA_variable_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState449 | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (J.formal_parameter_list)) = _v in
        let _v : (J.formal_parameter_list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_T_COMMA_variable__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_WHILE _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_LPAREN _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_BIT_NOT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_DECR _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_DECR_NB _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_DELETE _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_FALSE _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_FUNCTION _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_INCR _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_INCR_NB _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_LBRACKET _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_LCURLY _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_LPAREN _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_MINUS _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_NEW _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_NOT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_NULL _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_NUMBER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_PLUS _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_REGEX _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_STRING _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_THIS _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_TRUE _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_TYPEOF _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | Js_token.T_VOID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState291)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState362 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (l : (J.Label.t))), (_2 : (Parse_info.t))), _, (s : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) =                               ( J.Labelled_statement (l, s), J.N ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                      ( s ) in
        _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_2 : (Parse_info.t))), _, (_3 : (Parse_info.t))), _, (x0 : (J.ident))), _, (y0 : (J.initialiser option))), (_5 : (Parse_info.t))), _, (right : (J.expression))), (_7 : (Parse_info.t))), _, (body : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) = let left =
          let y = y0 in
          let x = x0 in
              ( (x, y) )
        in
           ( J.ForIn_statement (J.Right left, right, body), J.Pi pi ) in
        _menhir_goto_for_in_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState397 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_2 : (Parse_info.t))), _, (_3 : (Parse_info.t))), _, (initial : (J.variable_declaration list))), (_5 : (Parse_info.t))), _, (condition : (J.expression option))), (_7 : (Parse_info.t))), _, (increment : (J.expression option))), (_9 : (Parse_info.t))), _, (statement : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) =    ( J.For_statement (J.Right initial, condition, increment, statement), J.Pi pi ) in
        _menhir_goto_for_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState404 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_2 : (Parse_info.t))), _, (initial : (J.expression option))), (_4 : (Parse_info.t))), _, (condition : (J.expression option))), (_6 : (Parse_info.t))), _, (increment : (J.expression option))), (_8 : (Parse_info.t))), _, (statement : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) =    ( J.For_statement (J.Left initial, condition, increment, statement), J.Pi pi ) in
        _menhir_goto_for_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState409 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_2 : (Parse_info.t))), _, (left : (J.expression))), _, (_4 : (Parse_info.t))), _, (right : (J.expression))), (_6 : (Parse_info.t))), _, (body : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) =    ( J.ForIn_statement (J.Left left, right, body), J.Pi pi ) in
        _menhir_goto_for_in_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_ELSE _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_BIT_NOT _v ->
                _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_BREAK _v ->
                _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_CONTINUE _v ->
                _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_DEBUGGER _v ->
                _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_DECR _v ->
                _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_DECR_NB _v ->
                _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_DELETE _v ->
                _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_DO _v ->
                _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_FALSE _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_FOR _v ->
                _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_IF _v ->
                _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_INCR _v ->
                _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_INCR_NB _v ->
                _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_LBRACKET _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_LCURLY _v ->
                _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_LPAREN _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_MINUS _v ->
                _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_NEW _v ->
                _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_NOT _v ->
                _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_NULL _v ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_NUMBER _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_PLUS _v ->
                _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_REGEX _v ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_RETURN _v ->
                _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_SEMICOLON _v ->
                _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_STRING _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_SWITCH _v ->
                _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_THIS _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_THROW _v ->
                _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_TRUE _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_TRY _v ->
                _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_TYPEOF _v ->
                _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_VAR _v ->
                _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_VOID _v ->
                _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_WHILE _v ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | Js_token.T_WITH _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState416 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState416)
        | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))), (_30 : (Parse_info.t))), _, (t : (J.statement * J.location))) = _menhir_stack in
            let _v : (J.statement * J.location) = let condition =
              let _3 = _30 in
              let item = item0 in
              let _1 = _10 in
                                                                       ( item )
            in
                 ( (J.If_statement (condition, t, None), J.Pi pi) ) in
            _menhir_goto_if_statement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState416 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))), (_30 : (Parse_info.t))), _, (t : (J.statement * J.location))), (_4 : (Parse_info.t))), _, (e : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) = let condition =
          let _3 = _30 in
          let item = item0 in
          let _1 = _10 in
                                                                   ( item )
        in
             ( (J.If_statement (condition, t, Some e), J.Pi pi) ) in
        _menhir_goto_if_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState178 | MenhirState423 | MenhirState418 | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_BREAK _v ->
            _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_CONTINUE _v ->
            _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_DEBUGGER _v ->
            _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_DECR _v ->
            _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_DELETE _v ->
            _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_DO _v ->
            _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_FOR _v ->
            _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_IF _v ->
            _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_INCR _v ->
            _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_MINUS _v ->
            _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_NEW _v ->
            _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_NOT _v ->
            _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_PLUS _v ->
            _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_RETURN _v ->
            _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_SEMICOLON _v ->
            _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_SWITCH _v ->
            _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_THROW _v ->
            _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_TRY _v ->
            _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_VAR _v ->
            _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_VOID _v ->
            _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_WHILE _v ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_WITH _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState418 _v
        | Js_token.T_CASE _ | Js_token.T_DEFAULT _ | Js_token.T_RCURLY _ ->
            _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState418
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState418)
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))), (_30 : (Parse_info.t))), _, (body : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) = let condition =
          let _3 = _30 in
          let item = item0 in
          let _1 = _10 in
                                                                   ( item )
        in
             ( (J.While_statement (condition, body), J.Pi pi) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                      ( s ) in
        _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState454 | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.source_element * J.location) =    ( let statement, pi = _1 in J.Statement statement, pi ) in
        _menhir_goto_source_element _menhir_env _menhir_stack _menhir_s _v
    | MenhirState494 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s, (_1 : (Parse_info.t))), (_10 : (Parse_info.t))), _, (item0 : (J.expression))), (_30 : (Parse_info.t))), _, (_3 : (J.statement * J.location))) = _menhir_stack in
        let _v : (J.statement * J.location) = let _2 =
          let _3 = _30 in
          let item = item0 in
          let _1 = _10 in
                                                                   ( item )
        in
                                                      ( assert false ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                      ( s ) in
        _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_either_T_SEMICOLON_T_VIRTUAL_SEMICOLON_ : _menhir_env -> 'ttv_tail -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_2 : (Parse_info.t)) = _v in
    let (_menhir_stack, _menhir_s, (s : (J.statement * J.location))) = _menhir_stack in
    let _v : (J.statement * J.location) =                                                                   ( s ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_label_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.Label.t option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState269 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (J.Label.t option)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (J.statement * J.location) =                         ( (J.Continue_statement _2,J.Pi pi) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                         ( s ) in
        _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (J.Label.t option)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (J.statement * J.location) =                      ( (J.Break_statement _2, J.Pi pi) ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (J.statement * J.location)) = _v in
        let _v : (J.statement * J.location) =                         ( s ) in
        _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_member_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LBRACKET _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState350 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState353 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState353)
    | Js_token.T_LPAREN _v ->
        _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState350 _v
    | Js_token.T_PERIOD _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState350 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState351 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState351)
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =                                   ( _1 ) in
        _menhir_goto_new_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState350

and _menhir_goto_primary_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =      ( e ) in
    _menhir_goto_member_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_elison_rev : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.array_litteral) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _v
        | Js_token.T_COMMA _ | Js_token.T_RBRACKET _ ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState463)
    | MenhirState468 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RBRACKET _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_4 : (Parse_info.t)) = _v in
            let (((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (_2 : (J.expression option list))), _, (_3 : (J.array_litteral))) = _menhir_stack in
            let _v : (Parse_info.t * J.expression) =      ( (pi, J.EArr (List.rev_append _2 (List.rev _3))) ) in
            _menhir_goto_array_literal _menhir_env _menhir_stack _menhir_s _v
        | Js_token.T_BIT_NOT _ | Js_token.T_COMMA _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DELETE _ | Js_token.T_FALSE _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_REGEX _ | Js_token.T_STRING _ | Js_token.T_THIS _ | Js_token.T_TRUE _ | Js_token.T_TYPEOF _ | Js_token.T_VOID _ ->
            _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce97 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.formal_parameter_list) =     ( [] ) in
    _menhir_goto_loption_separated_nonempty_list_T_COMMA_variable__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_source_element_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Javascript.program) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState454 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (J.source_element * J.location))), _, (xs : (Javascript.program))) = _menhir_stack in
        let _v : (Javascript.program) =     ( x :: xs ) in
        _menhir_goto_list_source_element_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RCURLY _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (pi2 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi1 : (Parse_info.t))), _, (x : (Javascript.program))) = _menhir_stack in
            let _v : (J.function_body * Parse_info.t * Parse_info.t) =                                  ( (x, pi1, pi2) ) in
            (match _menhir_s with
            | MenhirState451 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (block : (J.function_body * Parse_info.t * Parse_info.t)) = _v in
                let (((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (name : (J.ident))), (_10 : (Parse_info.t))), _, (xs00 : (J.formal_parameter_list))), (_30 : (Parse_info.t))) = _menhir_stack in
                let _v : (J.function_declaration * J.location) = let args =
                  let _3 = _30 in
                  let xs0 = xs00 in
                  let _1 = _10 in
                  let item =
                    let xs = xs0 in
                        ( xs )
                  in
                                                                           ( item )
                in
                   ( let elements, _pi_start, pi_end = block in
     (name, args, elements, J.Pi pi_end), J.Pi pi ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_1 : (J.function_declaration * J.location)) = _v in
                let _v : (J.source_element * J.location) =    ( let declaration, pi = _1 in J.Function_declaration declaration, pi ) in
                _menhir_goto_source_element _menhir_env _menhir_stack _menhir_s _v
            | MenhirState39 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (block : (J.function_body * Parse_info.t * Parse_info.t)) = _v in
                let (((((_menhir_stack, _menhir_s, (pi : (Parse_info.t))), _, (name : (J.ident option))), (_10 : (Parse_info.t))), _, (xs00 : (J.formal_parameter_list))), (_30 : (Parse_info.t))) = _menhir_stack in
                let _v : (Parse_info.t * J.expression) = let args =
                  let _3 = _30 in
                  let xs0 = xs00 in
                  let _1 = _10 in
                  let item =
                    let xs = xs0 in
                        ( xs )
                  in
                                                                           ( item )
                in
                   ( let elements, _pi_start, _pi_end = block in
     pi, J.EFun (name, args, elements, J.Pi pi) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (e : (Parse_info.t * J.expression)) = _v in
                let _v : (Parse_info.t * J.expression) =                          ( e ) in
                _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.EOF _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_2 : (Parse_info.t)) = _v in
            let (_menhir_stack, _menhir_s, (l : (Javascript.program))) = _menhir_stack in
            let _v : (Javascript.program) =                          ( l ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Javascript.program)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement_no_semi : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                        ( s ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce116 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.expression option) =     ( None ) in
    _menhir_goto_option_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce93 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.statement_list) =     ( [] ) in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce78 : _menhir_env -> 'ttv_tail * _menhir_state * (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string * Parse_info.t))) = _menhir_stack in
    let _v : (J.identifier) =                 ( fst _1 ) in
    match _menhir_s with
    | MenhirState28 | MenhirState32 | MenhirState447 | MenhirState449 | MenhirState438 | MenhirState214 | MenhirState387 | MenhirState166 | MenhirState171 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (J.identifier)) = _v in
        let _v : (J.ident) =                 ( var i ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState28 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (J.ident))) = _menhir_stack in
            let _v : (J.ident option) =     ( Some x ) in
            _menhir_goto_option_variable_ _menhir_env _menhir_stack _menhir_s _v
        | MenhirState449 | MenhirState34 | MenhirState32 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_COMMA _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
            | Js_token.T_RPAREN _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (J.ident))) = _menhir_stack in
                let _v : (J.formal_parameter_list) =     ( [ x ] ) in
                _menhir_goto_separated_nonempty_list_T_COMMA_variable_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState171 | MenhirState166 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_ASSIGN _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_BIT_NOT _v ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_DECR _v ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_DECR_NB _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_DELETE _v ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_FALSE _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_FUNCTION _v ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_INCR _v ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_INCR_NB _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_LBRACKET _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_LCURLY _v ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_LPAREN _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_MINUS _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_NEW _v ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_NOT _v ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_NULL _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_NUMBER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_PLUS _v ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_REGEX _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_STRING _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_THIS _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_TRUE _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_TYPEOF _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | Js_token.T_VOID _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState168 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState168)
            | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (J.initialiser option) =     ( None ) in
                _menhir_goto_option_initializer__ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState214 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_ASSIGN _v ->
                _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState215 _v
            | Js_token.T_COMMA _ | Js_token.T_IN _ | Js_token.T_SEMICOLON _ ->
                _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState215
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState215)
        | MenhirState387 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_ASSIGN _v ->
                _menhir_run216 _menhir_env (Obj.magic _menhir_stack) MenhirState388 _v
            | Js_token.T_COMMA _ | Js_token.T_SEMICOLON _ ->
                _menhir_reduce124 _menhir_env (Obj.magic _menhir_stack) MenhirState388
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState388)
        | MenhirState438 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_RPAREN _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_LCURLY _v ->
                    _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState440 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState440)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState447 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_LPAREN _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState449 _v
                | Js_token.T_RPAREN _ ->
                    _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState449
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState449)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (J.identifier)) = _v in
        let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =      ( let (start, e1) = _1 in (start, J.EDot(e1,i)) ) in
        _menhir_goto_member_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (J.identifier)) = _v in
        let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =      ( let (start, e) = _1 in (start, J.EDot (e, _3)) ) in
        _menhir_goto_call_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState494 | MenhirState40 | MenhirState454 | MenhirState163 | MenhirState178 | MenhirState423 | MenhirState188 | MenhirState418 | MenhirState210 | MenhirState416 | MenhirState409 | MenhirState404 | MenhirState397 | MenhirState260 | MenhirState261 | MenhirState362 | MenhirState273 | MenhirState269 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (J.identifier)) = _v in
        let _v : (J.Label.t) =               ( J.Label.of_string _1 ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState273 | MenhirState269 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (J.Label.t))) = _menhir_stack in
            let _v : (J.Label.t option) =     ( Some x ) in
            _menhir_goto_option_label_ _menhir_env _menhir_stack _menhir_s _v
        | MenhirState0 | MenhirState494 | MenhirState40 | MenhirState454 | MenhirState163 | MenhirState178 | MenhirState423 | MenhirState188 | MenhirState418 | MenhirState210 | MenhirState416 | MenhirState409 | MenhirState404 | MenhirState397 | MenhirState260 | MenhirState362 | MenhirState261 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_COLON _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Js_token.T_BIT_NOT _v ->
                    _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_BREAK _v ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_CONTINUE _v ->
                    _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_DEBUGGER _v ->
                    _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_DECR _v ->
                    _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_DECR_NB _v ->
                    _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_DELETE _v ->
                    _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_DO _v ->
                    _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_FALSE _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_FOR _v ->
                    _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_IDENTIFIER _v ->
                    _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_IF _v ->
                    _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_INCR _v ->
                    _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_INCR_NB _v ->
                    _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_LBRACKET _v ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_LCURLY _v ->
                    _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_LPAREN _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_MINUS _v ->
                    _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_NEW _v ->
                    _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_NOT _v ->
                    _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_NULL _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_NUMBER _v ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_PLUS _v ->
                    _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_REGEX _v ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_RETURN _v ->
                    _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_SEMICOLON _v ->
                    _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_STRING _v ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_SWITCH _v ->
                    _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_THIS _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_THROW _v ->
                    _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_TRUE _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_TRY _v ->
                    _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_TYPEOF _v ->
                    _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_VAR _v ->
                    _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_VOID _v ->
                    _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_WHILE _v ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | Js_token.T_WITH _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState362)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState351 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (J.identifier)) = _v in
        let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =    ( let (start, e1) = _1 in (start, J.EDot(e1,i)) ) in
        _menhir_goto_member_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState378 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_3 : (J.identifier)) = _v in
        let ((_menhir_stack, _menhir_s, (_1 : (Parse_info.t * J.expression))), _, (_2 : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =    ( let (start, e) = _1 in (start, J.EDot(e,_3)) ) in
        _menhir_goto_call_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_expression_no_in_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.expression option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_SEMICOLON _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState400 _v
        | Js_token.T_SEMICOLON _ ->
            _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack) MenhirState400
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState400)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_statement_need_semi : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.statement * J.location) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_SEMICOLON _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Parse_info.t)) = _v in
        let _v : (Parse_info.t) =                 ( _1 ) in
        _menhir_goto_either_T_SEMICOLON_T_VIRTUAL_SEMICOLON_ _menhir_env _menhir_stack _v
    | Js_token.T_VIRTUAL_SEMICOLON _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Parse_info.t)) = _v in
        let _v : (Parse_info.t) =                            ( _1 ) in
        _menhir_goto_either_T_SEMICOLON_T_VIRTUAL_SEMICOLON_ _menhir_env _menhir_stack _v
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (statement : (J.statement * J.location))) = _menhir_stack in
        (match try
          Some (                                 (
    (* 7.9.1 - 1 *)
    (* When, as the program is parsed from left to right, a token (called the offending token)
       is encountered that is not allowed by any production of the grammar, then a semicolon
       is automatically inserted before the offending token if one or more of the following
       conditions is true:
       - The offending token is }.
       - The offending token is separated from the previous
         token by at least one LineTerminator. *)

    (* 7.9.1 - 2 *)
    (* When, as the program is parsed from left to right, the end of the input stream of tokens *)
    (* is encountered and the parser is unable to parse the input token stream as a single *)
    (* complete ECMAScript Program, then a semicolon is automatically inserted at the end *)
    (* of the input stream. *)

    (* @@@@@@@@@ HACK @@@@@@@@@@ *)
    (* menhir internal's         *)
    (* look the current token:   *)
    (* - if it is on another line (linebreak inbetween), accept the statement *)
    (* - fail otherwise *)
    (* @@@@@@@@@ HACK @@@@@@@@@@ *)

    match _tok with
      | EOF _ | T_RCURLY _ -> statement
      | token ->
        let info = Js_token.info_of_tok token in
        match info.Parse_info.fol with
          | Some true -> statement
          | _ -> (raise _eRR)
  ) : (J.statement * J.location))
        with
        | Error ->
            None with
        | Some _v ->
            _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
        | None ->
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce126 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (J.Label.t option) =     ( None ) in
    _menhir_goto_option_label_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_property_name : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.property_name) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_COLON _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_primary_expression_no_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState499 | MenhirState2 | MenhirState3 | MenhirState4 | MenhirState9 | MenhirState12 | MenhirState13 | MenhirState14 | MenhirState15 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState25 | MenhirState26 | MenhirState412 | MenhirState407 | MenhirState402 | MenhirState400 | MenhirState213 | MenhirState395 | MenhirState393 | MenhirState380 | MenhirState369 | MenhirState358 | MenhirState353 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState321 | MenhirState319 | MenhirState317 | MenhirState315 | MenhirState311 | MenhirState309 | MenhirState307 | MenhirState305 | MenhirState303 | MenhirState301 | MenhirState299 | MenhirState297 | MenhirState291 | MenhirState275 | MenhirState266 | MenhirState264 | MenhirState262 | MenhirState258 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState230 | MenhirState226 | MenhirState224 | MenhirState222 | MenhirState220 | MenhirState216 | MenhirState208 | MenhirState205 | MenhirState203 | MenhirState201 | MenhirState197 | MenhirState195 | MenhirState193 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState175 | MenhirState168 | MenhirState164 | MenhirState42 | MenhirState44 | MenhirState45 | MenhirState46 | MenhirState155 | MenhirState153 | MenhirState148 | MenhirState146 | MenhirState144 | MenhirState142 | MenhirState140 | MenhirState136 | MenhirState132 | MenhirState130 | MenhirState128 | MenhirState126 | MenhirState124 | MenhirState122 | MenhirState120 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState98 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState82 | MenhirState80 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState63 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Parse_info.t * J.expression)) = _v in
        let _v : (Parse_info.t * J.expression) =                          ( e ) in
        _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 | MenhirState494 | MenhirState40 | MenhirState454 | MenhirState163 | MenhirState178 | MenhirState423 | MenhirState188 | MenhirState418 | MenhirState210 | MenhirState416 | MenhirState409 | MenhirState404 | MenhirState397 | MenhirState260 | MenhirState362 | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (e : (Parse_info.t * J.expression)) = _v in
        let _v : (Parse_info.t * J.expression) =    ( e ) in
        _menhir_goto_member_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_object_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =                          ( e ) in
    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (string * Parse_info.t)) = _v in
    let _v : (J.property_name) =                      ( J.PNS (fst s) ) in
    _menhir_goto_property_name _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (string * Parse_info.t)) = _v in
    let _v : (J.property_name) =                      ( J.PNI (fst i) ) in
    _menhir_goto_property_name _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_array_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (a : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =                                   ( a ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run462 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Parse_info.t)) = _v in
    let _v : (J.array_litteral) =            ( [] ) in
    _menhir_goto_elison_rev _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce285 : _menhir_env -> 'ttv_tail * _menhir_state * (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (string * Parse_info.t))) = _menhir_stack in
    let _v : (J.identifier * Parse_info.t) =                 ( _1 ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (J.identifier * Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =                      ( let (i, pi) = _1 in (pi, J.EVar (var i)) ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_option_variable_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (J.ident option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
        | Js_token.T_RPAREN _ ->
            _menhir_reduce97 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack)

and _menhir_goto_boolean_literal : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t * J.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (b : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =                      ( b ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce91 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Javascript.program) =     ( [] ) in
    _menhir_goto_list_source_element_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run164 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164

and _menhir_run166 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState166 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState166

and _menhir_run175 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState175 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState175

and _menhir_run177 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState177 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState177

and _menhir_run179 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState179 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState179

and _menhir_run181 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState182 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState182)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run189 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (J.statement * J.location) =                   ( J.Empty_statement, J.Pi pi ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                      ( s ) in
    _menhir_goto_statement_no_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_run190 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _v
    | Js_token.EOF _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DEFAULT _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FOR _ | Js_token.T_IF _ | Js_token.T_RCURLY _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_SWITCH _ | Js_token.T_THROW _ | Js_token.T_TRY _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        _menhir_reduce116 _menhir_env (Obj.magic _menhir_stack) MenhirState190
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190

and _menhir_run193 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState193 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193

and _menhir_run195 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState195 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195

and _menhir_run197 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState197 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState197

and _menhir_run201 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201

and _menhir_run178 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_BREAK _v ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_CONTINUE _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_DEBUGGER _v ->
        _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_DECR _v ->
        _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_DELETE _v ->
        _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_DO _v ->
        _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_FOR _v ->
        _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_IF _v ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_INCR _v ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_MINUS _v ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_NEW _v ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_NOT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_PLUS _v ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_RETURN _v ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_SEMICOLON _v ->
        _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_SWITCH _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_THROW _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_TRY _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_VAR _v ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_VOID _v ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_WITH _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _v
    | Js_token.T_RCURLY _ ->
        _menhir_reduce93 _menhir_env (Obj.magic _menhir_stack) MenhirState178
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178

and _menhir_run203 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203

and _menhir_run205 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205

and _menhir_run207 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState208 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState208)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run211 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.EOF _ | Js_token.T_AND _ | Js_token.T_ASSIGN _ | Js_token.T_BIT_AND _ | Js_token.T_BIT_AND_ASSIGN _ | Js_token.T_BIT_NOT _ | Js_token.T_BIT_OR _ | Js_token.T_BIT_OR_ASSIGN _ | Js_token.T_BIT_XOR _ | Js_token.T_BIT_XOR_ASSIGN _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_COMMA _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DIV _ | Js_token.T_DIV_ASSIGN _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_EQUAL _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_GREATER_THAN _ | Js_token.T_GREATER_THAN_EQUAL _ | Js_token.T_IDENTIFIER _ | Js_token.T_IF _ | Js_token.T_IN _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_INSTANCEOF _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LESS_THAN _ | Js_token.T_LESS_THAN_EQUAL _ | Js_token.T_LPAREN _ | Js_token.T_LSHIFT _ | Js_token.T_LSHIFT_ASSIGN _ | Js_token.T_MINUS _ | Js_token.T_MINUS_ASSIGN _ | Js_token.T_MOD _ | Js_token.T_MOD_ASSIGN _ | Js_token.T_MULT _ | Js_token.T_MULT_ASSIGN _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NOT_EQUAL _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_OR _ | Js_token.T_PERIOD _ | Js_token.T_PLING _ | Js_token.T_PLUS _ | Js_token.T_PLUS_ASSIGN _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_RSHIFT _ | Js_token.T_RSHIFT3 _ | Js_token.T_RSHIFT3_ASSIGN _ | Js_token.T_RSHIFT_ASSIGN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRICT_EQUAL _ | Js_token.T_STRICT_NOT_EQUAL _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        _menhir_reduce285 _menhir_env (Obj.magic _menhir_stack)
    | Js_token.T_COLON _ ->
        _menhir_reduce78 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run447 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState447 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState447

and _menhir_run212 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_LPAREN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_BIT_NOT _v ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_DECR _v ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_DECR_NB _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_DELETE _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_FALSE _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_FUNCTION _v ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_IDENTIFIER _v ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_INCR _v ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_INCR_NB _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_LBRACKET _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_LCURLY _v ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_LPAREN _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_MINUS _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_NEW _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_NOT _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_NULL _v ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_NUMBER _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_PLUS _v ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_REGEX _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_STRING _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_THIS _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_TRUE _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_TYPEOF _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState213 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Js_token.T_IDENTIFIER _v ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState214 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState214)
        | Js_token.T_VOID _v ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState213 _v
        | Js_token.T_SEMICOLON _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState213 in
            let _v : (J.expression option) =     ( None ) in
            _menhir_goto_option_expression_no_in_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState213)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run261 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_BREAK _v ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_CONTINUE _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_DEBUGGER _v ->
        _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_DECR _v ->
        _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_DELETE _v ->
        _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_DO _v ->
        _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_FOR _v ->
        _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_IF _v ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_INCR _v ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_MINUS _v ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_NEW _v ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_NOT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_PLUS _v ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_RETURN _v ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_SEMICOLON _v ->
        _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_SWITCH _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_THROW _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_TRY _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_VAR _v ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_VOID _v ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | Js_token.T_WITH _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState261

and _menhir_run262 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState262 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262

and _menhir_run264 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264

and _menhir_run266 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState266 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266

and _menhir_run268 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (J.statement * J.location) =                  ( J.Debugger_statement, J.Pi pi ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (J.statement * J.location)) = _v in
    let _v : (J.statement * J.location) =                         ( s ) in
    _menhir_goto_statement_need_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_run269 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _v
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState269
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState269

and _menhir_run273 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _v
    | Js_token.EOF _ | Js_token.T_BIT_NOT _ | Js_token.T_BREAK _ | Js_token.T_CASE _ | Js_token.T_CONTINUE _ | Js_token.T_DEBUGGER _ | Js_token.T_DECR _ | Js_token.T_DECR_NB _ | Js_token.T_DEFAULT _ | Js_token.T_DELETE _ | Js_token.T_DO _ | Js_token.T_ELSE _ | Js_token.T_FALSE _ | Js_token.T_FOR _ | Js_token.T_FUNCTION _ | Js_token.T_IF _ | Js_token.T_INCR _ | Js_token.T_INCR_NB _ | Js_token.T_LBRACKET _ | Js_token.T_LCURLY _ | Js_token.T_LPAREN _ | Js_token.T_MINUS _ | Js_token.T_NEW _ | Js_token.T_NOT _ | Js_token.T_NULL _ | Js_token.T_NUMBER _ | Js_token.T_PLUS _ | Js_token.T_RCURLY _ | Js_token.T_REGEX _ | Js_token.T_RETURN _ | Js_token.T_SEMICOLON _ | Js_token.T_STRING _ | Js_token.T_SWITCH _ | Js_token.T_THIS _ | Js_token.T_THROW _ | Js_token.T_TRUE _ | Js_token.T_TRY _ | Js_token.T_TYPEOF _ | Js_token.T_VAR _ | Js_token.T_VIRTUAL_SEMICOLON _ | Js_token.T_VOID _ | Js_token.T_WHILE _ | Js_token.T_WITH _ ->
        _menhir_reduce126 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273

and _menhir_run275 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState275 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState275

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState499 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState494 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState492 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState491 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState490 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState489 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState487 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState483 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState478 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState471 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState468 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState465 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState463 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState461 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState460 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState454 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState451 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState449 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState447 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState443 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState440 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState438 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState435 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState434 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState429 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState427 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState423 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState418 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState416 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState412 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState409 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState407 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState406 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState404 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState402 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState400 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState397 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState395 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState393 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState388 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState387 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState380 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState378 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState377 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState369 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState362 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState358 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState357 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState353 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState351 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState350 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState347 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState345 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState343 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState341 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState339 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState337 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState335 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState333 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState331 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState329 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState327 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState325 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState323 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState321 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState319 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState317 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState315 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState312 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState311 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState310 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState309 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState308 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState307 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState306 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState305 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState304 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState303 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState302 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState301 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState300 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState299 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState298 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState297 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState296 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState291 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState276 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState275 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState273 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState269 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState267 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState266 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState265 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState264 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState263 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState262 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState261 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState260 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState258 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState254 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState252 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState250 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState248 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState246 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState244 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState242 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState240 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState238 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState236 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState234 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState232 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState230 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState226 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState225 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState224 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState222 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState220 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState218 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState216 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState215 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState214 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState213 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState210 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState208 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState206 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState205 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState204 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState203 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState202 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState201 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState199 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState197 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState196 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState195 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState194 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState193 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState190 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState188 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState186 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState185 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState182 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState179 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState178 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState177 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState176 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState175 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState171 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState168 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState166 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState165 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState164 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState163 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState161 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState160 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState159 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState155 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState153 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState149 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState146 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState144 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState141 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState140 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState122 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =               ( (pi, J.EBool true) ) in
    _menhir_goto_boolean_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =                      ( (pi, J.EVar (var "this")) ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string * Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =                      ( let (s, start) = _1 in (start, J.EStr (s, `Utf8)) ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string * Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =            (
   let (s, pi) = _1 in
   let len = String.length s in
   let regexp, option =
     if s.[len - 1] = '/'
     then String.sub s 1 (len - 2),None
     else
       let i = String.rindex s '/' in
       String.sub s 1 (i - 1),Some (String.sub s (i+1) (len - i - 1))
   in
   (pi, J.ERegexp (regexp, option)) ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (r : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =                                   ( r ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * float * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (string * float * Parse_info.t)) = _v in
    let _v : (Parse_info.t * float) =             ( let (_, f, pi) = _1 in (pi, f) ) in
    match _menhir_s with
    | MenhirState499 | MenhirState0 | MenhirState494 | MenhirState2 | MenhirState3 | MenhirState4 | MenhirState9 | MenhirState12 | MenhirState13 | MenhirState14 | MenhirState15 | MenhirState22 | MenhirState471 | MenhirState463 | MenhirState23 | MenhirState25 | MenhirState26 | MenhirState40 | MenhirState454 | MenhirState163 | MenhirState178 | MenhirState423 | MenhirState188 | MenhirState418 | MenhirState210 | MenhirState416 | MenhirState412 | MenhirState409 | MenhirState407 | MenhirState213 | MenhirState400 | MenhirState402 | MenhirState404 | MenhirState393 | MenhirState395 | MenhirState397 | MenhirState260 | MenhirState380 | MenhirState369 | MenhirState362 | MenhirState358 | MenhirState353 | MenhirState261 | MenhirState347 | MenhirState345 | MenhirState343 | MenhirState341 | MenhirState339 | MenhirState337 | MenhirState335 | MenhirState333 | MenhirState331 | MenhirState329 | MenhirState327 | MenhirState325 | MenhirState323 | MenhirState321 | MenhirState319 | MenhirState317 | MenhirState315 | MenhirState311 | MenhirState309 | MenhirState307 | MenhirState305 | MenhirState303 | MenhirState301 | MenhirState299 | MenhirState297 | MenhirState291 | MenhirState275 | MenhirState266 | MenhirState264 | MenhirState262 | MenhirState258 | MenhirState216 | MenhirState254 | MenhirState252 | MenhirState250 | MenhirState248 | MenhirState246 | MenhirState244 | MenhirState242 | MenhirState240 | MenhirState238 | MenhirState236 | MenhirState234 | MenhirState232 | MenhirState230 | MenhirState226 | MenhirState224 | MenhirState222 | MenhirState220 | MenhirState208 | MenhirState205 | MenhirState203 | MenhirState201 | MenhirState197 | MenhirState195 | MenhirState193 | MenhirState190 | MenhirState186 | MenhirState182 | MenhirState179 | MenhirState175 | MenhirState168 | MenhirState164 | MenhirState42 | MenhirState44 | MenhirState45 | MenhirState46 | MenhirState155 | MenhirState153 | MenhirState63 | MenhirState148 | MenhirState146 | MenhirState67 | MenhirState144 | MenhirState69 | MenhirState142 | MenhirState140 | MenhirState136 | MenhirState80 | MenhirState120 | MenhirState132 | MenhirState130 | MenhirState126 | MenhirState128 | MenhirState124 | MenhirState122 | MenhirState118 | MenhirState114 | MenhirState100 | MenhirState98 | MenhirState82 | MenhirState96 | MenhirState94 | MenhirState92 | MenhirState90 | MenhirState88 | MenhirState84 | MenhirState71 | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Parse_info.t * float)) = _v in
        let _v : (Parse_info.t * J.expression) =                      ( let (start, n) = _1 in (start, J.ENum n) ) in
        _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 | MenhirState478 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (n : (Parse_info.t * float)) = _v in
        let _v : (J.property_name) =                      ( J.PNN (snd n) ) in
        _menhir_goto_property_name _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =              ( (pi, J.EVar (var "null")) ) in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (Parse_info.t * J.expression)) = _v in
    let _v : (Parse_info.t * J.expression) =                      ( n ) in
    _menhir_goto_primary_expression_no_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | Js_token.T_STRING _v ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | Js_token.T_RCURLY _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _v : (unit) =        () in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Js_token.T_RCURLY _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (pi2 : (Parse_info.t)) = _v in
            let ((_menhir_stack, _menhir_s, (pi1 : (Parse_info.t))), _, (x : (unit))) = _menhir_stack in
            let _v : (unit * Parse_info.t * Parse_info.t) =                                  ( (x, pi1, pi2) ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (block : (unit * Parse_info.t * Parse_info.t)) = _v in
            let _v : (Parse_info.t * J.expression) =    ( let _pairs, pi_start, _pi_end = block in pi_start, J.EObj [] ) in
            _menhir_goto_object_literal _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_COMMA _v ->
        _menhir_run462 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_RBRACKET _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState23 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Parse_info.t)) = _v in
        let (_menhir_stack, _menhir_s, (pi : (Parse_info.t))) = _menhir_stack in
        let _v : (Parse_info.t * J.expression) =      ( (pi, J.EArr []) ) in
        _menhir_goto_array_literal _menhir_env _menhir_stack _menhir_s _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string * Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce285 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | Js_token.T_LPAREN _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState28 in
        let _v : (J.ident option) =     ( None ) in
        _menhir_goto_option_variable_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (pi : (Parse_info.t)) = _v in
    let _v : (Parse_info.t * J.expression) =               ( (pi, J.EBool false) ) in
    _menhir_goto_boolean_literal _menhir_env _menhir_stack _menhir_s _v

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> (Parse_info.t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Javascript.program) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run275 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_BREAK _v ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_CONTINUE _v ->
        _menhir_run269 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_DEBUGGER _v ->
        _menhir_run268 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_DECR _v ->
        _menhir_run266 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run264 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_DELETE _v ->
        _menhir_run262 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_DO _v ->
        _menhir_run261 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_FOR _v ->
        _menhir_run212 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run447 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_IF _v ->
        _menhir_run207 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_INCR _v ->
        _menhir_run205 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run203 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run178 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_MINUS _v ->
        _menhir_run201 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_NEW _v ->
        _menhir_run197 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_NOT _v ->
        _menhir_run195 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_PLUS _v ->
        _menhir_run193 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_RETURN _v ->
        _menhir_run190 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_SEMICOLON _v ->
        _menhir_run189 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_SWITCH _v ->
        _menhir_run181 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_THROW _v ->
        _menhir_run179 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_TRY _v ->
        _menhir_run177 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run175 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_VAR _v ->
        _menhir_run166 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_VOID _v ->
        _menhir_run164 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_WHILE _v ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.T_WITH _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | Js_token.EOF _ ->
        _menhir_reduce91 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and standalone_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Javascript.expression) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Js_token.T_BIT_NOT _v ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_DECR _v ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_DECR_NB _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_DELETE _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_FALSE _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_FUNCTION _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_IDENTIFIER _v ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_INCR _v ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_INCR_NB _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_LBRACKET _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_LCURLY _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_LPAREN _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_MINUS _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_NEW _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_NOT _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_NULL _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_NUMBER _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_PLUS _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_REGEX _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_STRING _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_THIS _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_TRUE _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_TYPEOF _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | Js_token.T_VOID _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState499)
  

