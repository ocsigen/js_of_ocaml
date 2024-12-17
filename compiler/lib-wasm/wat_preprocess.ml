exception Syntax_error of (Lexing.position * Lexing.position) * string

let sign = [%sedlex.regexp? Opt ('+' | '-')]

let digit = [%sedlex.regexp? '0' .. '9']

let hexdigit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']

let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]

let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]

let uN = [%sedlex.regexp? num | "0x", hexnum]

let sN = [%sedlex.regexp? sign, uN]

let iN = [%sedlex.regexp? uN | sN]

let float = [%sedlex.regexp? num, Opt ('.', Opt num), Opt (('e' | 'E'), sign, num)]

let hexfloat =
  [%sedlex.regexp? "0x", hexnum, Opt ('.', Opt hexnum), Opt (('p' | 'P'), sign, num)]

let fN = [%sedlex.regexp? sign, (float | hexfloat | "inf" | "nan" | "nan:", hexnum)]

let idchar =
  [%sedlex.regexp?
    ( '0' .. '9'
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '/'
    | ':'
    | '<'
    | '='
    | '>'
    | '?'
    | '@'
    | '\\'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' )]

let id = [%sedlex.regexp? '$', Plus idchar]

let linechar = [%sedlex.regexp? Sub (any, (10 | 13))]

let newline = [%sedlex.regexp? 10 | 13 | 13, 10]

let linecomment = [%sedlex.regexp? ";;", Star linechar, (newline | eof)]

let format = [%sedlex.regexp? '\n' | 9]

(*
let space = [%sedlex.regexp? ' ' | format | comment]
*)
let keyword = [%sedlex.regexp? Plus idchar]

let rec comment lexbuf =
  match%sedlex lexbuf with
  | ";)" -> ()
  | "(;" ->
      comment lexbuf;
      comment lexbuf
  | ';' | '(' | Plus (Sub (any, (';' | '('))) -> comment lexbuf
  | _ ->
      raise
        (Syntax_error
           (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Malformed comment.\n"))

let string_buffer = Buffer.create 256

let rec string lexbuf =
  match%sedlex lexbuf with
  | '"' ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      s
  | Plus (Sub (any, (0 .. 31 | 0x7f | '"' | '\\')))
  | "\\t" | "\\n" | "\\r" | "\\'" | "\\\"" | "\\\\"
  | '\\', hexdigit, hexdigit
  | "\\u{", hexnum, "}" ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      string lexbuf
  | _ ->
      raise
        (Syntax_error
           (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Malformed string.\n"))

type pos =
  { loc : Lexing.position
  ; byte_loc : int
  }

type token =
  | LPAREN
  | RPAREN
  | ATOM of string
  | EOF

let locs lexbuf =
  let loc, loc' = Sedlexing.lexing_positions lexbuf in
  let byte_loc, byte_loc' = Sedlexing.bytes_loc lexbuf in
  { loc; byte_loc }, { loc = loc'; byte_loc = byte_loc' }

let rec token lexbuf =
  match%sedlex lexbuf with
  | '(' -> LPAREN, locs lexbuf
  | ')' -> RPAREN, locs lexbuf
  | uN | sN | fN | keyword -> ATOM (Sedlexing.Utf8.lexeme lexbuf), locs lexbuf
  | '"' ->
      let string_start =
        { loc = Sedlexing.lexing_position_start lexbuf
        ; byte_loc = Sedlexing.lexeme_bytes_start lexbuf
        }
      in
      Buffer.add_char string_buffer '"';
      let str = string lexbuf in
      ( ATOM str
      , ( string_start
        , { loc = Sedlexing.lexing_position_curr lexbuf
          ; byte_loc = Sedlexing.lexeme_bytes_end lexbuf
          } ) )
  | newline | linecomment -> token lexbuf
  | Plus (' ' | '\t') -> token lexbuf
  | "(;" ->
      comment lexbuf;
      token lexbuf
  | eof -> EOF, locs lexbuf
  | _ ->
      raise
        (Syntax_error (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Syntax error.\n"))

type t =
  { loc : pos * pos
  ; desc : desc
  }

and desc =
  | Atom of string
  | List of t list

let rec parse_list lexbuf toplevel start_loc acc =
  let tok, (loc, loc') = token lexbuf in
  match tok with
  | LPAREN ->
      let lst, loc = parse_list lexbuf false loc [] in
      parse_list lexbuf toplevel start_loc ({ desc = List lst; loc } :: acc)
  | RPAREN ->
      if toplevel
      then
        raise
          (Syntax_error
             ( Sedlexing.lexing_positions lexbuf
             , Printf.sprintf "Missing closing parenthesis.\n" ));
      List.rev acc, (start_loc, loc')
  | EOF ->
      if not toplevel
      then
        raise
          (Syntax_error
             ( Sedlexing.lexing_positions lexbuf
             , Printf.sprintf "Unexpected end of file.\n" ));
      List.rev acc, (start_loc, loc')
  | ATOM s ->
      parse_list lexbuf toplevel start_loc ({ loc = loc, loc'; desc = Atom s } :: acc)

let parse lexbuf =
  parse_list
    lexbuf
    true
    { loc = Sedlexing.lexing_position_start lexbuf
    ; byte_loc = Sedlexing.lexeme_bytes_end lexbuf
    }
    []

let report_syntax_error loc msg =
  let location = MenhirLib.LexerUtil.range loc in
  Format.eprintf "%s%s%!" location msg;
  exit 1

module StringMap = Map.Make (String)

type st =
  { text : string
  ; mutable pos : pos
  ; variables : bool StringMap.t
  ; buf : Buffer.t
  ; mutable head : int
  ; head_buf : Buffer.t
  }

let write st pos' =
  Buffer.add_substring st.buf st.text st.pos.byte_loc (pos'.byte_loc - st.pos.byte_loc);
  st.pos <- pos'

let skip st (pos' : pos) =
  let lines = pos'.loc.pos_lnum - st.pos.loc.pos_lnum in
  let cols =
    pos'.loc.pos_cnum
    - pos'.loc.pos_bol
    - if lines > 0 then 0 else st.pos.loc.pos_cnum - st.pos.loc.pos_cnum
  in
  Buffer.add_string st.buf (String.make lines '\n');
  Buffer.add_string st.buf (String.make cols ' ');
  st.pos <- pos'

let pred_position { loc; byte_loc } =
  { loc = { loc with pos_cnum = loc.pos_cnum - 1 }; byte_loc = byte_loc - 1 }

let eval st cond =
  match cond with
  | { desc = Atom s; loc = pos, pos' } ->
      if not (StringMap.mem s st.variables)
      then
        raise
          (Syntax_error ((pos.loc, pos'.loc), Printf.sprintf "Unknown variable '%s'.\n" s));
      StringMap.find s st.variables
  | { loc = pos, pos'; _ } ->
      raise (Syntax_error ((pos.loc, pos'.loc), Printf.sprintf "Syntax error.\n"))

let rec rewrite_list st l = List.iter (rewrite st) l

and rewrite st elt =
  match elt with
  | { desc =
        List
          ({ desc = Atom "try"; _ }
          :: ( { desc = List ({ desc = Atom "result"; _ } :: _); _ }
               :: { desc = List ({ desc = Atom "do"; loc = _, pos_after_do } :: body)
                  ; loc = _, pos_after_body
                  }
               :: _
             | { desc = List ({ desc = Atom "do"; loc = _, pos_after_do } :: body)
               ; loc = _, pos_after_body
               }
               :: _ ))
    ; loc = pos, pos'
    }
    when StringMap.find "trap-on-exception" st.variables ->
      write st pos;
      Buffer.add_string st.buf "(block";
      skip st pos_after_do;
      rewrite_list st body;
      write st pos_after_body;
      skip st pos'
  | { desc = List ({ desc = Atom "throw"; _ } :: _); loc = pos, pos' }
    when StringMap.find "trap-on-exception" st.variables ->
      write st pos;
      Buffer.add_string st.buf "(unreachable)";
      skip st pos'
  | { desc =
        List
          [ { desc = Atom "#if"; _ }
          ; cond
          ; { desc = List ({ desc = Atom "#then"; loc = _, pos_after_then } :: then_body)
            ; loc = _, pos_after_then_body
            }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      if eval st cond
      then (
        skip st pos_after_then;
        rewrite_list st then_body;
        write st (pred_position pos_after_then_body);
        skip st pos')
      else skip st pos'
  | { desc =
        List
          [ { desc = Atom "#if"; _ }
          ; cond
          ; { desc = List ({ desc = Atom "#then"; loc = _, pos_after_then } :: then_body)
            ; loc = _, pos_after_then_body
            }
          ; { desc = List ({ desc = Atom "#else"; loc = _, pos_after_else } :: else_body)
            ; loc = _, pos_after_else_body
            }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      if eval st cond
      then (
        skip st pos_after_then;
        rewrite_list st then_body;
        write st (pred_position pos_after_then_body);
        skip st pos')
      else (
        skip st pos_after_else;
        rewrite_list st else_body;
        write st (pred_position pos_after_else_body);
        skip st pos')
  | { desc =
        List
          [ { desc = Atom "#string"; _ }
          ; { desc = Atom name; _ }
          ; { desc = Atom value; _ }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      (if
         StringMap.mem "use-js-string" st.variables
         && StringMap.find "use-js-string" st.variables
       then (
         Printf.bprintf
           st.head_buf
           "(import \"\" %s (global %s$string externref)) "
           value
           name;
         Printf.bprintf
           st.buf
           "(global %s (ref eq) (struct.new $string (any.convert_extern (global.get \
            %s$string))))"
           name
           name)
       else
         let s = String.sub value 1 (String.length value - 2) in
         Printf.bprintf
           st.buf
           "(global %s (ref eq) (array.new_fixed $bytes %d%a))"
           name
           (String.length s)
           (fun f s ->
             String.iter (fun c -> Printf.bprintf f " (i32.const %d)" (Char.code c)) s)
           s);
      skip st pos'
  | { desc =
        List
          [ { desc = Atom "#jsstring"; _ }
          ; { desc = Atom name; _ }
          ; { desc = Atom value; _ }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      Printf.bprintf
        st.head_buf
        "(import \"\" %s (global %s$string externref)) "
        value
        name;
      Printf.bprintf
        st.buf
        "(global %s (ref eq) (struct.new $js (any.convert_extern (global.get \
         %s$string))))"
        name
        name;
      skip st pos'
  | { desc = List ({ desc = Atom "module"; loc = _, pos } :: _ as l); _ } ->
      st.head <- pos.byte_loc;
      rewrite_list st l
  | { desc = List l; _ } -> rewrite_list st l
  | _ -> ()

let f ~variables ~filename ~contents:text =
  let variables = ("trap-on-exception", false) :: variables in
  let variables =
    List.fold_left (fun m (k, v) -> StringMap.add k v m) StringMap.empty variables
  in
  let lexbuf = Sedlexing.Utf8.from_string text in
  Sedlexing.set_filename lexbuf filename;
  try
    let t, (pos, end_pos) = parse lexbuf in
    let st =
      { text
      ; pos
      ; variables
      ; buf = Buffer.create (String.length text)
      ; head_buf = Buffer.create 128
      ; head = 0
      }
    in
    rewrite_list st t;
    write st end_pos;
    let head = Buffer.contents st.head_buf in
    let contents = Buffer.contents st.buf in
    String.sub contents 0 st.head
    ^ head
    ^ String.sub contents st.head (String.length contents - st.head)
  with Syntax_error (loc, msg) -> report_syntax_error loc msg
