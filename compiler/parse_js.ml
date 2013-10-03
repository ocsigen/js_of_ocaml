

let is_comment = function
  | Parser_js.TCommentSpace _
  | Parser_js.TCommentNewline _
  | Parser_js.TComment _ -> true
  | _ -> false

let make_lexer f =
  (fun lexbuf ->
    let t = f lexbuf in
    if not(is_comment t)
    then begin
      Lexer_js._last_non_whitespace_like_token := Some t;
    end;
    t)

let iter_with_previous_opt f = function
  | [] -> ()
  | e::l ->
      f None e;
      let rec iter_with_previous_ previous = function
        | [] -> ()
        | e::l -> f (Some previous) e ; iter_with_previous_ e l
      in iter_with_previous_ e l

let push v l = l := v :: !l
let pop l =
  let v = List.hd !l in
  l := List.tl !l;
  v


let rparens_of_if toks =
  let open Parser_js in
  let toks = List.filter (fun x -> not (is_comment x)) toks in
  let stack = ref [] in
  let rparens_if = ref [] in
  iter_with_previous_opt (fun prev x ->
    (match x with
      | T_LPAREN _ -> push prev stack;
      | T_RPAREN info ->
        if !stack <> []
        then begin
          match pop stack with
            | Some (T_IF _) -> push info rparens_if
            | _ -> ()
        end
      | _ -> ()
    )
  ) toks;
  !rparens_if

let info_of_tok t =
  let open Parser_js in
      match t with
  | TUnknown ii -> ii
  | TCommentSpace ii -> ii
  | TCommentNewline ii -> ii
  | TComment ii -> ii
  | EOF ii -> ii

  | T_NUMBER (s, ii) -> ii
  | T_IDENTIFIER (s, ii) -> ii
  | T_STRING (s, ii) -> ii
  | T_REGEX (s, ii) -> ii

  | T_FUNCTION ii -> ii
  | T_IF ii -> ii
  | T_IN ii -> ii
  | T_INSTANCEOF ii -> ii
  | T_RETURN ii -> ii
  | T_SWITCH ii -> ii
  | T_THIS ii -> ii
  | T_THROW ii -> ii
  | T_TRY ii -> ii
  | T_VAR ii -> ii
  | T_WHILE ii -> ii
  | T_WITH ii -> ii
  | T_CONST ii -> ii
  | T_NULL ii -> ii
  | T_FALSE ii -> ii
  | T_TRUE ii -> ii
  | T_BREAK ii -> ii
  | T_CASE ii -> ii
  | T_CATCH ii -> ii
  | T_CONTINUE ii -> ii
  | T_DEFAULT ii -> ii
  | T_DO ii -> ii
  | T_FINALLY ii -> ii
  | T_FOR ii -> ii
  | T_ELSE ii -> ii
  | T_NEW ii -> ii
  | T_LCURLY ii -> ii
  | T_RCURLY ii -> ii
  | T_LPAREN ii -> ii
  | T_RPAREN ii -> ii
  | T_LBRACKET ii -> ii
  | T_RBRACKET ii -> ii
  | T_SEMICOLON ii -> ii
  | T_COMMA ii -> ii
  | T_PERIOD ii -> ii
  | T_RSHIFT3_ASSIGN ii -> ii
  | T_RSHIFT_ASSIGN ii -> ii
  | T_LSHIFT_ASSIGN ii -> ii
  | T_BIT_XOR_ASSIGN ii -> ii
  | T_BIT_OR_ASSIGN ii -> ii
  | T_BIT_AND_ASSIGN ii -> ii
  | T_MOD_ASSIGN ii -> ii
  | T_DIV_ASSIGN ii -> ii
  | T_MULT_ASSIGN ii -> ii
  | T_MINUS_ASSIGN ii -> ii
  | T_PLUS_ASSIGN ii -> ii
  | T_ASSIGN ii -> ii
  | T_PLING ii -> ii
  | T_COLON ii -> ii
  | T_OR ii -> ii
  | T_AND ii -> ii
  | T_BIT_OR ii -> ii
  | T_BIT_XOR ii -> ii
  | T_BIT_AND ii -> ii
  | T_EQUAL ii -> ii
  | T_NOT_EQUAL ii -> ii
  | T_STRICT_EQUAL ii -> ii
  | T_STRICT_NOT_EQUAL ii -> ii
  | T_LESS_THAN_EQUAL ii -> ii
  | T_GREATER_THAN_EQUAL ii -> ii
  | T_LESS_THAN ii -> ii
  | T_GREATER_THAN ii -> ii
  | T_LSHIFT ii -> ii
  | T_RSHIFT ii -> ii
  | T_RSHIFT3 ii -> ii
  | T_PLUS ii -> ii
  | T_MINUS ii -> ii
  | T_DIV ii -> ii
  | T_MULT ii -> ii
  | T_MOD ii -> ii
  | T_NOT ii -> ii
  | T_BIT_NOT ii -> ii
  | T_INCR ii -> ii
  | T_DECR ii -> ii
  | T_DELETE ii -> ii
  | T_TYPEOF ii -> ii
  | T_VOID ii -> ii
  | T_VIRTUAL_SEMICOLON ii -> ii

let compute_line x prev : Parse_info.t option =
  let (x,_) as tok = info_of_tok x in
  let (prev,_) = info_of_tok prev in
  if prev <> x
  then Some tok
  else None

let rec adjust_tokens xs =
  let open Parser_js in
  let rparens_if = rparens_of_if xs in
  let hrparens_if =
    let h = Hashtbl.create 101 in
    List.iter (fun s -> Hashtbl.add h s true) rparens_if;
    h in

  match xs with
    | [] -> []
    | y::ys ->
      let res = ref [] in
      push y res;
      let rec aux prev f xs =
        match xs with
          | [] -> ()
          | e::l ->
            if is_comment e
            then begin
              push e res;
              aux prev f l
            end else begin
              f prev e;
              aux e f l
            end
      in
      let f = (fun prev x ->
        match prev, x with
          | (T_LCURLY _ | T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _),
        T_RCURLY _ ->
            push x res;
          (* also one after ? *)
          (* push (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)

          | _, T_RCURLY fake ->
            push (T_VIRTUAL_SEMICOLON fake) res;
            push x res;
        (* also one after ? *)
        (* push (T.T_VIRTUAL_SEMICOLON (Ast.fakeInfo ())) res; *)

          | (T_SEMICOLON _ | T_VIRTUAL_SEMICOLON _),
            EOF _ ->
            push x res;
          | _, EOF fake ->
            push (T_VIRTUAL_SEMICOLON fake) res;
            push x res;

          | T_RCURLY _,
            (T_IDENTIFIER _ |
             T_IF _ | T_VAR _ | T_FOR _ | T_RETURN _ |
             T_SWITCH _ |
             T_FUNCTION _ | T_THIS _ |
             T_BREAK _ | T_NEW _

            )
            ->
            begin match compute_line x prev with
              | None -> ()
              | Some fake -> push (T_VIRTUAL_SEMICOLON fake) res;
            end;
            push x res;

        (* this is valid only if the RPAREN is not the closing paren
         * of a if
         *)
          | T_RPAREN info,
              (T_VAR _ | T_IF _ | T_THIS _ | T_FOR _ | T_RETURN _ |
                  T_IDENTIFIER _ | T_CONTINUE _
              ) when not (Hashtbl.mem hrparens_if info)
                  ->
            begin match compute_line x prev with
              | None -> ()
              | Some fake -> push (T_VIRTUAL_SEMICOLON fake) res;
            end;
            push x res;


          | T_RBRACKET _,
              (T_FOR _ | T_IF _ | T_VAR _ | T_IDENTIFIER _)
              ->
            begin match compute_line x prev with
              | None -> ()
              | Some fake -> push (T_VIRTUAL_SEMICOLON fake) res;
            end;
            push x res;


          | (T_IDENTIFIER _ | T_NULL _ | T_STRING _ | T_REGEX _
                | T_FALSE _ | T_TRUE _
          ),
              (T_VAR _ | T_IDENTIFIER _ | T_IF _ | T_THIS _ |
                  T_RETURN _ | T_BREAK _ | T_ELSE _
              )
              ->
            begin match compute_line x prev with
              | None -> ()
              | Some fake -> push (T_VIRTUAL_SEMICOLON fake) res;
            end;
            push x res;

          | _, _ -> push x res
      )
      in
      aux y f ys;
      List.rev !res

type st = {
  mutable rest : Parser_js.token list;
  mutable current : Parser_js.token ;
  mutable passed : Parser_js.token list }

let lex file =
  let open Parser_js in
  let ic = open_in file in
  Lexer_js.reset();
  let lexbuf = Lexing.from_channel ic in
  let f = make_lexer Lexer_js.initial in
  let rec loop lexbuf acc =
    let t = f lexbuf in
    match t with
      | EOF _ -> List.rev (t::acc)
      | _ -> loop lexbuf (t :: acc)
  in
  let toks = loop lexbuf [] in
  let toks = adjust_tokens toks in
  let toks = List.filter (fun x -> not (is_comment x)) toks in
  let cur = {
    rest = toks;
    passed = [];
    current = List.hd toks
  }
  in
  (fun lb ->
    match cur.rest with
      | [] -> assert false
      | x::tl ->
        cur.rest <- tl;
        cur.current <- x;
        cur.passed <- x::cur.passed;
        x
  ),lexbuf,cur
let parse file =
  let lexer_fun,lexbuf,state = lex file in
  try
    let p = Parser_js.program lexer_fun lexbuf  in
    let buf = Buffer.create 1024 in
    let f = Pretty_print.to_buffer buf in
    Pretty_print.set_compact f true;
    Js_output.program f (fun _ -> None) (fun _ -> assert false) p;
    Printf.printf "%s" (Buffer.contents buf)
  with Parsing.Parse_error ->
    let (l,c) = info_of_tok (List.hd state.rest) in
    Printf.eprintf "error at l:%d col:%d\n" l c;

    exit 1

let _ =
  parse (Sys.argv.(1))
