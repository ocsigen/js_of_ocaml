# 29 "deriving_json/deriving_Json_lexer.mll"
 
  module Lexing =
    (*
      We override Lexing.engine in order to avoid creating a new position
      record each time a rule is matched.
      This reduces total parsing time by about 31%.
    *)
  struct
    include Lexing

    external c_engine : lex_tables -> int -> lexbuf -> int = "caml_lex_engine"

    let engine tbl state buf =
      let result = c_engine tbl state buf in
      (*
      if result >= 0 then begin
	buf.lex_start_p <- buf.lex_curr_p;
	buf.lex_curr_p <- {buf.lex_curr_p
			   with pos_cnum = buf.lex_abs_pos + buf.lex_curr_pos};
      end;
      *)
      result
  end

  open Printf
  open Lexing

  type lexbuf = {
    buf : Deriving_bi_outbuf.t;
      (* Buffer used to accumulate substrings *)

    mutable lnum : int;
      (* Current line number (starting from 1) *)

    mutable bol : int;
      (* Absolute position of the first character of the current line
	 (starting from 0) *)

    lexbuf : Lexing.lexbuf;

  }

  let dec c =
    Char.code c - 48

  let hex c =
    match c with
	'0'..'9' -> int_of_char c - int_of_char '0'
      | 'a'..'f' -> int_of_char c - int_of_char 'a' + 10
      | 'A'..'F' -> int_of_char c - int_of_char 'A' + 10
      | _ -> assert false

   let utf8_of_bytes buf a b c d =
     let i = (a lsl 12) lor (b lsl 8) lor (c lsl 4) lor d in
     if i < 0x80 then
       Deriving_bi_outbuf.add_char buf (Char.chr i)
     else if i < 0x800 then begin
       Deriving_bi_outbuf.add_char buf (Char.chr (0xc0 lor ((i lsr 6) land 0x1f)));
       Deriving_bi_outbuf.add_char buf (Char.chr (0x80 lor (i land 0x3f)))
     end else (* i < 0x10000 *) begin
       Deriving_bi_outbuf.add_char buf (Char.chr (0xe0 lor ((i lsr 12) land 0xf)));
       Deriving_bi_outbuf.add_char buf (Char.chr (0x80 lor ((i lsr 6) land 0x3f)));
       Deriving_bi_outbuf.add_char buf (Char.chr (0x80 lor (i land 0x3f)))
     end

  let json_error msg = failwith ("Deriving.Json: " ^ msg)

  let custom_error descr v lexbuf =
    let offs = lexbuf.lex_abs_pos in
    let bol = v.bol in
    let pos1 = offs + lexbuf.lex_start_pos - bol in
    let pos2 = max pos1 (offs + lexbuf.lex_curr_pos - bol - 1) in
    let bytes =
      if pos1 = pos2 then
	sprintf "byte %i" (pos1+1)
      else
	sprintf "bytes %i-%i" (pos1+1) (pos2+1)
    in
    let msg = sprintf "Line %i, %s:\n%s" v.lnum bytes descr in
    json_error msg


  let lexer_error descr v lexbuf =
    custom_error
      (sprintf "%s '%s'" descr (Lexing.lexeme lexbuf))
      v lexbuf

  let min10 = min_int / 10 - (if min_int mod 10 = 0 then 0 else 1)
  let max10 = max_int / 10 + (if max_int mod 10 = 0 then 0 else 1)

  exception Int_overflow

  let extract_positive_int lexbuf =
    let start = lexbuf.lex_start_pos in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n >= max10 then
	raise Int_overflow
      else
	n := 10 * !n + dec s.[i]
    done;
    if !n < 0 then
      raise Int_overflow
    else
      !n

  let make_positive_int v lexbuf =
      try `Int (extract_positive_int lexbuf)
      with Int_overflow ->
        lexer_error "Int overflow" v lexbuf

  let extract_negative_int lexbuf =
    let start = lexbuf.lex_start_pos + 1 in
    let stop = lexbuf.lex_curr_pos in
    let s = lexbuf.lex_buffer in
    let n = ref 0 in
    for i = start to stop - 1 do
      if !n <= min10 then
	raise Int_overflow
      else
	n := 10 * !n - dec s.[i]
    done;
    if !n > 0 then
      raise Int_overflow
    else
      !n

  let make_negative_int v lexbuf =
      try `Int (extract_negative_int lexbuf)
      with Int_overflow ->
        lexer_error "Int overflow" v lexbuf

  let newline v lexbuf =
    v.lnum <- v.lnum + 1;
    v.bol <- lexbuf.lex_abs_pos + lexbuf.lex_curr_pos

  let add_lexeme buf lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    Deriving_bi_outbuf.add_substring buf lexbuf.lex_buffer lexbuf.lex_start_pos len

  let map_lexeme f lexbuf =
    let len = lexbuf.lex_curr_pos - lexbuf.lex_start_pos in
    f lexbuf.lex_buffer lexbuf.lex_start_pos len

  type variant_kind = [ `Edgy_bracket | `Square_bracket | `Double_quote ]
  type tuple_kind = [ `Parenthesis | `Square_bracket ]

# 152 "deriving_json/deriving_Json_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\252\255\001\000\254\255\255\255\002\000\247\255\248\255\
    \008\000\250\255\251\255\252\255\253\255\254\255\255\255\072\000\
    \095\000\133\000\249\255\003\000\253\255\254\255\255\255\004\000\
    \252\255\253\255\254\255\255\255\008\000\252\255\253\255\254\255\
    \004\000\255\255\005\000\255\255\006\000\000\000\253\255\024\000\
    \254\255\007\000\255\255\020\000\253\255\254\255\000\000\003\000\
    \005\000\255\255\051\000\252\255\253\255\001\000\000\000\014\000\
    \000\000\255\255\007\000\017\000\001\000\254\255\034\000\252\255\
    \253\255\156\000\255\255\166\000\254\255\188\000\198\000\253\255\
    \254\255\255\255\217\000\230\000\253\255\254\255\255\255\243\000\
    \004\001\017\001\253\255\254\255\255\255\027\001\037\001\050\001\
    \250\255\251\255\034\000\062\001\084\001\023\000\002\000\003\000\
    \255\255\032\000\031\000\044\000\050\000\040\000\036\000\254\255\
    \048\000\057\000\061\000\058\000\070\000\060\000\056\000\253\255\
    \099\001\116\001\126\001\151\001\136\001\161\001\183\001\193\001\
    \006\000\253\255\254\255\255\255\197\000\253\255\254\255\255\255\
    \226\000\253\255\254\255\255\255\203\001\252\255\253\255\254\255\
    \255\255\213\001\226\001\252\255\253\255\254\255\255\255\236\001\
    ";
  Lexing.lex_backtrk = 
   "\255\255\255\255\002\000\255\255\255\255\255\255\255\255\255\255\
    \007\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \003\000\255\255\001\000\255\255\004\000\003\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\001\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\002\000\002\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\002\000\255\255\000\000\255\255\001\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\255\255\000\000\
    \001\000\255\255\255\255\255\255\255\255\000\000\001\000\255\255\
    \255\255\255\255\003\000\003\000\004\000\004\000\004\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\003\000\255\255\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\000\000\
    ";
  Lexing.lex_default = 
   "\002\000\000\000\002\000\000\000\000\000\007\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\021\000\000\000\000\000\000\000\025\000\
    \000\000\000\000\000\000\000\000\029\000\000\000\000\000\000\000\
    \255\255\000\000\255\255\000\000\255\255\255\255\000\000\255\255\
    \000\000\041\000\000\000\045\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\052\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\064\000\000\000\
    \000\000\255\255\000\000\255\255\000\000\255\255\072\000\000\000\
    \000\000\000\000\255\255\077\000\000\000\000\000\000\000\255\255\
    \255\255\083\000\000\000\000\000\000\000\255\255\255\255\089\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \122\000\000\000\000\000\000\000\126\000\000\000\000\000\000\000\
    \130\000\000\000\000\000\000\000\134\000\000\000\000\000\000\000\
    \000\000\255\255\140\000\000\000\000\000\000\000\000\000\255\255\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\037\000\000\000\000\000\000\000\037\000\000\000\037\000\
    \038\000\042\000\030\000\037\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \037\000\000\000\004\000\255\255\014\000\000\000\037\000\000\000\
    \123\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
    \027\000\014\000\032\000\033\000\000\000\039\000\000\000\000\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\040\000\000\000\000\000\000\000\000\000\041\000\
    \000\000\015\000\015\000\015\000\015\000\015\000\015\000\065\000\
    \113\000\096\000\066\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\003\000\255\255\014\000\000\000\
    \000\000\026\000\058\000\095\000\013\000\057\000\061\000\112\000\
    \012\000\015\000\015\000\015\000\015\000\015\000\015\000\048\000\
    \011\000\049\000\055\000\059\000\010\000\047\000\009\000\008\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\046\000\056\000\060\000\097\000\098\000\112\000\
    \099\000\016\000\016\000\016\000\016\000\016\000\016\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\053\000\100\000\101\000\102\000\103\000\105\000\106\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\107\000\054\000\
    \108\000\016\000\016\000\016\000\016\000\016\000\016\000\109\000\
    \110\000\111\000\000\000\000\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\000\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\068\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\073\000\074\000\
    \074\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \001\000\255\255\006\000\020\000\024\000\035\000\121\000\042\000\
    \031\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\080\000\044\000\000\000\078\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \127\000\000\000\063\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\051\000\078\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\086\000\131\000\
    \000\000\084\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\084\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\092\000\
    \000\000\000\000\090\000\091\000\091\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\113\000\000\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \000\000\000\000\000\000\093\000\000\000\000\000\000\000\000\000\
    \094\000\000\000\000\000\112\000\090\000\091\000\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\119\000\000\000\
    \119\000\000\000\000\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\104\000\000\000\000\000\
    \000\000\000\000\000\000\112\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\117\000\115\000\117\000\125\000\071\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\116\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\129\000\115\000\000\000\000\000\076\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\118\000\118\000\136\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\000\000\
    \000\000\082\000\142\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\135\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\088\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\141\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\133\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\139\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\037\000\255\255\255\255\255\255\037\000\255\255\036\000\
    \036\000\041\000\028\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \037\000\255\255\000\000\002\000\005\000\255\255\036\000\255\255\
    \120\000\255\255\255\255\255\255\255\255\255\255\255\255\019\000\
    \023\000\005\000\028\000\032\000\255\255\036\000\255\255\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\039\000\255\255\255\255\255\255\255\255\039\000\
    \255\255\008\000\008\000\008\000\008\000\008\000\008\000\062\000\
    \090\000\095\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\000\000\002\000\005\000\255\255\
    \255\255\023\000\053\000\094\000\005\000\056\000\060\000\090\000\
    \005\000\008\000\008\000\008\000\008\000\008\000\008\000\047\000\
    \005\000\048\000\054\000\058\000\005\000\046\000\005\000\005\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\043\000\055\000\059\000\093\000\097\000\090\000\
    \098\000\015\000\015\000\015\000\015\000\015\000\015\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\050\000\099\000\100\000\101\000\102\000\104\000\105\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\106\000\050\000\
    \107\000\015\000\015\000\015\000\015\000\015\000\015\000\108\000\
    \109\000\110\000\255\255\255\255\255\255\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\255\255\
    \016\000\016\000\016\000\016\000\016\000\016\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\065\000\065\000\065\000\065\000\
    \065\000\065\000\065\000\065\000\065\000\065\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\017\000\017\000\
    \017\000\017\000\017\000\017\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \000\000\002\000\005\000\019\000\023\000\034\000\120\000\041\000\
    \028\000\074\000\074\000\074\000\074\000\074\000\074\000\074\000\
    \074\000\074\000\074\000\075\000\043\000\255\255\075\000\075\000\
    \075\000\075\000\075\000\075\000\075\000\075\000\075\000\075\000\
    \124\000\255\255\062\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\050\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\081\000\128\000\
    \255\255\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\087\000\
    \255\255\255\255\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\091\000\255\255\091\000\091\000\
    \091\000\091\000\091\000\091\000\091\000\091\000\091\000\091\000\
    \255\255\255\255\255\255\087\000\255\255\255\255\255\255\255\255\
    \087\000\255\255\255\255\091\000\092\000\092\000\092\000\092\000\
    \092\000\092\000\092\000\092\000\092\000\092\000\112\000\255\255\
    \112\000\255\255\255\255\112\000\112\000\112\000\112\000\112\000\
    \112\000\112\000\112\000\112\000\112\000\092\000\255\255\255\255\
    \255\255\255\255\255\255\091\000\113\000\113\000\113\000\113\000\
    \113\000\113\000\113\000\113\000\113\000\113\000\114\000\114\000\
    \114\000\114\000\114\000\114\000\114\000\114\000\114\000\114\000\
    \116\000\116\000\116\000\116\000\116\000\116\000\116\000\116\000\
    \116\000\116\000\115\000\114\000\115\000\124\000\070\000\115\000\
    \115\000\115\000\115\000\115\000\115\000\115\000\115\000\115\000\
    \115\000\117\000\117\000\117\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\117\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\128\000\114\000\255\255\255\255\075\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\119\000\119\000\119\000\119\000\119\000\119\000\119\000\
    \119\000\119\000\119\000\132\000\132\000\132\000\132\000\132\000\
    \132\000\132\000\132\000\132\000\132\000\137\000\137\000\137\000\
    \137\000\137\000\137\000\137\000\137\000\137\000\137\000\255\255\
    \255\255\081\000\138\000\138\000\138\000\138\000\138\000\138\000\
    \138\000\138\000\138\000\138\000\143\000\143\000\143\000\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\132\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\087\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\138\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\132\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\138\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec finish_string v lexbuf =
  __ocaml_lex_finish_string_rec v lexbuf 0
and __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 197 "deriving_json/deriving_Json_lexer.mll"
                  ( Deriving_bi_outbuf.contents v.buf )
# 425 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 198 "deriving_json/deriving_Json_lexer.mll"
                  ( finish_escaped_char v lexbuf;
		    finish_string v lexbuf )
# 431 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 200 "deriving_json/deriving_Json_lexer.mll"
                  ( add_lexeme v.buf lexbuf;
		    finish_string v lexbuf )
# 437 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 202 "deriving_json/deriving_Json_lexer.mll"
                  ( custom_error "Unexpected end of input" v lexbuf )
# 442 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_finish_string_rec v lexbuf __ocaml_lex_state

and finish_escaped_char v lexbuf =
  __ocaml_lex_finish_escaped_char_rec v lexbuf 5
and __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 207 "deriving_json/deriving_Json_lexer.mll"
           c
# 454 "deriving_json/deriving_Json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 207 "deriving_json/deriving_Json_lexer.mll"
             ( Deriving_bi_outbuf.add_char v.buf c )
# 458 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 208 "deriving_json/deriving_Json_lexer.mll"
         ( Deriving_bi_outbuf.add_char v.buf '\b' )
# 463 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 209 "deriving_json/deriving_Json_lexer.mll"
         ( Deriving_bi_outbuf.add_char v.buf '\012' )
# 468 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 210 "deriving_json/deriving_Json_lexer.mll"
         ( Deriving_bi_outbuf.add_char v.buf '\n' )
# 473 "deriving_json/deriving_Json_lexer.ml"

  | 4 ->
# 211 "deriving_json/deriving_Json_lexer.mll"
         ( Deriving_bi_outbuf.add_char v.buf '\r' )
# 478 "deriving_json/deriving_Json_lexer.ml"

  | 5 ->
# 212 "deriving_json/deriving_Json_lexer.mll"
         ( Deriving_bi_outbuf.add_char v.buf '\t' )
# 483 "deriving_json/deriving_Json_lexer.ml"

  | 6 ->
let
# 213 "deriving_json/deriving_Json_lexer.mll"
                a
# 489 "deriving_json/deriving_Json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1)
and
# 213 "deriving_json/deriving_Json_lexer.mll"
                           b
# 494 "deriving_json/deriving_Json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 213 "deriving_json/deriving_Json_lexer.mll"
                                      c
# 499 "deriving_json/deriving_Json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3)
and
# 213 "deriving_json/deriving_Json_lexer.mll"
                                                 d
# 504 "deriving_json/deriving_Json_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 4) in
# 214 "deriving_json/deriving_Json_lexer.mll"
         ( utf8_of_bytes v.buf (hex a) (hex b) (hex c) (hex d) )
# 508 "deriving_json/deriving_Json_lexer.ml"

  | 7 ->
# 215 "deriving_json/deriving_Json_lexer.mll"
         ( lexer_error "Invalid escape sequence" v lexbuf )
# 513 "deriving_json/deriving_Json_lexer.ml"

  | 8 ->
# 216 "deriving_json/deriving_Json_lexer.mll"
         ( custom_error "Unexpected end of input" v lexbuf )
# 518 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_finish_escaped_char_rec v lexbuf __ocaml_lex_state

and read_comma v lexbuf =
  __ocaml_lex_read_comma_rec v lexbuf 19
and __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 219 "deriving_json/deriving_Json_lexer.mll"
          ( () )
# 529 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 220 "deriving_json/deriving_Json_lexer.mll"
          ( lexer_error "Expected ',' but found" v lexbuf )
# 534 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 221 "deriving_json/deriving_Json_lexer.mll"
          ( custom_error "Unexpected end of input" v lexbuf )
# 539 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_comma_rec v lexbuf __ocaml_lex_state

and read_comma_or_rbracket v lexbuf =
  __ocaml_lex_read_comma_or_rbracket_rec v lexbuf 23
and __ocaml_lex_read_comma_or_rbracket_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 224 "deriving_json/deriving_Json_lexer.mll"
          ( `Comma )
# 550 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 225 "deriving_json/deriving_Json_lexer.mll"
          ( `RBracket )
# 555 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 226 "deriving_json/deriving_Json_lexer.mll"
          ( lexer_error "Expected ',' or ']' but found" v lexbuf )
# 560 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 227 "deriving_json/deriving_Json_lexer.mll"
          ( custom_error "Unexpected end of input" v lexbuf )
# 565 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_comma_or_rbracket_rec v lexbuf __ocaml_lex_state

and finish_comment v lexbuf =
  __ocaml_lex_finish_comment_rec v lexbuf 28
and __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 230 "deriving_json/deriving_Json_lexer.mll"
         ( () )
# 576 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 231 "deriving_json/deriving_Json_lexer.mll"
         ( lexer_error "Unterminated comment" v lexbuf )
# 581 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 232 "deriving_json/deriving_Json_lexer.mll"
         ( newline v lexbuf; finish_comment v lexbuf )
# 586 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 233 "deriving_json/deriving_Json_lexer.mll"
         ( finish_comment v lexbuf )
# 591 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_finish_comment_rec v lexbuf __ocaml_lex_state

and read_eof lexbuf =
  __ocaml_lex_read_eof_rec lexbuf 34
and __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 238 "deriving_json/deriving_Json_lexer.mll"
              ( true )
# 602 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 239 "deriving_json/deriving_Json_lexer.mll"
              ( false )
# 607 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_eof_rec lexbuf __ocaml_lex_state

and read_space v lexbuf =
  __ocaml_lex_read_space_rec v lexbuf 36
and __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 242 "deriving_json/deriving_Json_lexer.mll"
                             ( newline v lexbuf; read_space v lexbuf )
# 618 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 243 "deriving_json/deriving_Json_lexer.mll"
                             ( finish_comment v lexbuf; read_space v lexbuf )
# 623 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 244 "deriving_json/deriving_Json_lexer.mll"
                             ( newline v lexbuf; read_space v lexbuf )
# 628 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 245 "deriving_json/deriving_Json_lexer.mll"
                             ( read_space v lexbuf )
# 633 "deriving_json/deriving_Json_lexer.ml"

  | 4 ->
# 246 "deriving_json/deriving_Json_lexer.mll"
                             ( () )
# 638 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_space_rec v lexbuf __ocaml_lex_state

and read_null v lexbuf =
  __ocaml_lex_read_null_rec v lexbuf 43
and __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 249 "deriving_json/deriving_Json_lexer.mll"
              ( () )
# 649 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 250 "deriving_json/deriving_Json_lexer.mll"
              ( lexer_error "Expected 'null' but found" v lexbuf )
# 654 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 251 "deriving_json/deriving_Json_lexer.mll"
              ( custom_error "Unexpected end of input" v lexbuf )
# 659 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_null_rec v lexbuf __ocaml_lex_state

and read_bool v lexbuf =
  __ocaml_lex_read_bool_rec v lexbuf 50
and __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 254 "deriving_json/deriving_Json_lexer.mll"
              ( true )
# 670 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 255 "deriving_json/deriving_Json_lexer.mll"
              ( false )
# 675 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 256 "deriving_json/deriving_Json_lexer.mll"
              ( lexer_error "Expected 'true' or 'false' but found" v lexbuf )
# 680 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 257 "deriving_json/deriving_Json_lexer.mll"
              ( custom_error "Unexpected end of input" v lexbuf )
# 685 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_bool_rec v lexbuf __ocaml_lex_state

and read_int v lexbuf =
  __ocaml_lex_read_int_rec v lexbuf 62
and __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 260 "deriving_json/deriving_Json_lexer.mll"
                         ( try extract_positive_int lexbuf
			   with Int_overflow ->
			     lexer_error "Int overflow" v lexbuf )
# 698 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 263 "deriving_json/deriving_Json_lexer.mll"
                         ( try extract_negative_int lexbuf
			   with Int_overflow ->
			     lexer_error "Int overflow" v lexbuf )
# 705 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 266 "deriving_json/deriving_Json_lexer.mll"
                         ( lexer_error "Expected integer but found" v lexbuf )
# 710 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 267 "deriving_json/deriving_Json_lexer.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 715 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_int_rec v lexbuf __ocaml_lex_state

and read_positive_int v lexbuf =
  __ocaml_lex_read_positive_int_rec v lexbuf 70
and __ocaml_lex_read_positive_int_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 270 "deriving_json/deriving_Json_lexer.mll"
                         ( try extract_positive_int lexbuf
			   with Int_overflow ->
			     lexer_error "Int overflow" v lexbuf )
# 728 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 273 "deriving_json/deriving_Json_lexer.mll"
                         ( lexer_error "Expected integer but found" v lexbuf )
# 733 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 274 "deriving_json/deriving_Json_lexer.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 738 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_positive_int_rec v lexbuf __ocaml_lex_state

and read_int32 v lexbuf =
  __ocaml_lex_read_int32_rec v lexbuf 75
and __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 277 "deriving_json/deriving_Json_lexer.mll"
                         ( try Int32.of_string (Lexing.lexeme lexbuf)
			   with _ ->
			     lexer_error "Int32 overflow" v lexbuf )
# 751 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 280 "deriving_json/deriving_Json_lexer.mll"
                         ( lexer_error "Expected int32 but found" v lexbuf )
# 756 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 281 "deriving_json/deriving_Json_lexer.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 761 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_int32_rec v lexbuf __ocaml_lex_state

and read_int64 v lexbuf =
  __ocaml_lex_read_int64_rec v lexbuf 81
and __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 284 "deriving_json/deriving_Json_lexer.mll"
                         ( try Int64.of_string (Lexing.lexeme lexbuf)
			   with _ ->
			     lexer_error "Int32 overflow" v lexbuf )
# 774 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 287 "deriving_json/deriving_Json_lexer.mll"
                         ( lexer_error "Expected int64 but found" v lexbuf )
# 779 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 288 "deriving_json/deriving_Json_lexer.mll"
                         ( custom_error "Unexpected end of input" v lexbuf )
# 784 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_int64_rec v lexbuf __ocaml_lex_state

and read_number v lexbuf =
  __ocaml_lex_read_number_rec v lexbuf 87
and __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 291 "deriving_json/deriving_Json_lexer.mll"
                ( nan )
# 795 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 292 "deriving_json/deriving_Json_lexer.mll"
                ( infinity )
# 800 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 293 "deriving_json/deriving_Json_lexer.mll"
                ( neg_infinity )
# 805 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 294 "deriving_json/deriving_Json_lexer.mll"
                ( float_of_string (lexeme lexbuf) )
# 810 "deriving_json/deriving_Json_lexer.ml"

  | 4 ->
# 295 "deriving_json/deriving_Json_lexer.mll"
                ( lexer_error "Expected number but found" v lexbuf )
# 815 "deriving_json/deriving_Json_lexer.ml"

  | 5 ->
# 296 "deriving_json/deriving_Json_lexer.mll"
                ( custom_error "Unexpected end of input" v lexbuf )
# 820 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_number_rec v lexbuf __ocaml_lex_state

and read_string v lexbuf =
  __ocaml_lex_read_string_rec v lexbuf 120
and __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 299 "deriving_json/deriving_Json_lexer.mll"
             ( Deriving_bi_outbuf.clear v.buf;
	       finish_string v lexbuf )
# 832 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 301 "deriving_json/deriving_Json_lexer.mll"
             ( lexer_error "Expected '\"' but found" v lexbuf )
# 837 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 302 "deriving_json/deriving_Json_lexer.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 842 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_string_rec v lexbuf __ocaml_lex_state

and read_lbracket v lexbuf =
  __ocaml_lex_read_lbracket_rec v lexbuf 124
and __ocaml_lex_read_lbracket_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 305 "deriving_json/deriving_Json_lexer.mll"
             ( () )
# 853 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 306 "deriving_json/deriving_Json_lexer.mll"
             ( lexer_error "Expected '[' but found" v lexbuf )
# 858 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 307 "deriving_json/deriving_Json_lexer.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 863 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_lbracket_rec v lexbuf __ocaml_lex_state

and read_rbracket v lexbuf =
  __ocaml_lex_read_rbracket_rec v lexbuf 128
and __ocaml_lex_read_rbracket_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 310 "deriving_json/deriving_Json_lexer.mll"
             ( () )
# 874 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 311 "deriving_json/deriving_Json_lexer.mll"
             ( lexer_error "Expected ']' but found" v lexbuf )
# 879 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 312 "deriving_json/deriving_Json_lexer.mll"
             ( custom_error "Unexpected end of input" v lexbuf )
# 884 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_rbracket_rec v lexbuf __ocaml_lex_state

and read_case v lexbuf =
  __ocaml_lex_read_case_rec v lexbuf 132
and __ocaml_lex_read_case_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 315 "deriving_json/deriving_Json_lexer.mll"
                 ( try `Cst (extract_positive_int lexbuf)
                       with Int_overflow -> lexer_error "Int overflow" v lexbuf )
# 896 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 317 "deriving_json/deriving_Json_lexer.mll"
                 ( read_space v lexbuf;
		   `NCst (read_positive_int v lexbuf) )
# 902 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 319 "deriving_json/deriving_Json_lexer.mll"
                 ( lexer_error "Expected positive integer or '[' but found" v lexbuf )
# 907 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 320 "deriving_json/deriving_Json_lexer.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )
# 912 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_case_rec v lexbuf __ocaml_lex_state

and read_vcase v lexbuf =
  __ocaml_lex_read_vcase_rec v lexbuf 138
and __ocaml_lex_read_vcase_rec v lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 323 "deriving_json/deriving_Json_lexer.mll"
                 ( try `Cst (extract_positive_int lexbuf)
                       with Int_overflow -> lexer_error "Int overflow" v lexbuf )
# 924 "deriving_json/deriving_Json_lexer.ml"

  | 1 ->
# 325 "deriving_json/deriving_Json_lexer.mll"
                 ( read_space v lexbuf;
		   let zero = read_positive_int v lexbuf in
		   if (zero <> 0) then
		     lexer_error
		       (Printf.sprintf "Expected 0 but found %d" zero) v lexbuf;
		   read_space v lexbuf;
		   read_comma v lexbuf;
		   read_space v lexbuf;
		   `NCst (read_int v lexbuf) )
# 937 "deriving_json/deriving_Json_lexer.ml"

  | 2 ->
# 334 "deriving_json/deriving_Json_lexer.mll"
                 ( lexer_error "Expected positive integer or '[' but found" v lexbuf )
# 942 "deriving_json/deriving_Json_lexer.ml"

  | 3 ->
# 335 "deriving_json/deriving_Json_lexer.mll"
                 ( custom_error "Unexpected end of input" v lexbuf )
# 947 "deriving_json/deriving_Json_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_read_vcase_rec v lexbuf __ocaml_lex_state

;;

# 337 "deriving_json/deriving_Json_lexer.mll"
 

  let init_lexer ?buf lexbuf =
    let buf =
      match buf with
	  None -> Deriving_bi_outbuf.create 256
	| Some buf -> buf
    in
    {
      buf = buf;
      lnum = 1;
      bol = 0;
      lexbuf = lexbuf;
    }

  let read_bounded_int min max v lexbuf =
    let n = read_int v lexbuf in
    if n < min || n > max then
      lexer_error "Int outside of bounds" v lexbuf
    else
      n

  let read_bool v = read_space v v.lexbuf; read_bool v v.lexbuf
  let read_int v = read_space v v.lexbuf; read_int v v.lexbuf
  let read_bounded_int ?(min = 0) ~max v =
    read_space v v.lexbuf; read_bounded_int min max v v.lexbuf
  let read_int32 v = read_space v v.lexbuf; read_int32 v v.lexbuf
  let read_int64 v = read_space v v.lexbuf; read_int64 v v.lexbuf
  let read_number v = read_space v v.lexbuf; read_number v v.lexbuf
  let read_string v = read_space v v.lexbuf; read_string v v.lexbuf

  let read_case v = read_space v v.lexbuf; read_case v v.lexbuf
  let read_vcase v = read_space v v.lexbuf; read_vcase v v.lexbuf

  let read_lbracket v = read_space v v.lexbuf; read_lbracket v v.lexbuf
  let read_rbracket v = read_space v v.lexbuf; read_rbracket v v.lexbuf
  let read_comma v = read_space v v.lexbuf; read_comma v v.lexbuf
  let read_comma_or_rbracket v =
    read_space v v.lexbuf; read_comma_or_rbracket v v.lexbuf


# 995 "deriving_json/deriving_Json_lexer.ml"
