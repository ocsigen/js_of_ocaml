open Stdlib

exception Error of (Lexing.position * Lexing.position) * string

let report_error loc msg =
  let location = MenhirLib.LexerUtil.range loc in
  Format.eprintf "%s%s%!" location msg;
  exit 1

(****)

(*
See the WebAssembly Text Format Specification:
https://webassembly.github.io/spec/core/text/index.html

We use custom annotations to extend the syntax
(https://github.com/WebAssembly/annotations).
*)

let digit = [%sedlex.regexp? '0' .. '9']

let hexdigit = [%sedlex.regexp? '0' .. '9' | 'a' .. 'f' | 'A' .. 'F']

let num = [%sedlex.regexp? digit, Star (Opt '_', digit)]

let hexnum = [%sedlex.regexp? hexdigit, Star (Opt '_', hexdigit)]

let uN = [%sedlex.regexp? num | "0x", hexnum]

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

let keyword = [%sedlex.regexp? Plus idchar]

let rec comment start_pos lexbuf =
  match%sedlex lexbuf with
  | ";)" -> ()
  | "(;" ->
      comment (Sedlexing.lexing_positions lexbuf) lexbuf;
      comment start_pos lexbuf
  | ';' | '(' | Plus (Sub (any, (';' | '('))) -> comment start_pos lexbuf
  | _ -> raise (Error (start_pos, Printf.sprintf "Unclosed comment.\n"))

let string_buffer = Buffer.create 256

let rec string lexbuf =
  match%sedlex lexbuf with
  | '"' ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      s
  | Plus
      ( Sub (any, (0 .. 31 | 0x7f | '"' | '\\'))
      | "\\t" | "\\n" | "\\r" | "\\'" | "\\\"" | "\\\\"
      | '\\', hexdigit, hexdigit
      | "\\u{", hexnum, "}" ) ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      string lexbuf
  | _ ->
      raise
        (Error (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Malformed string.\n"))

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

let position_of_loc (pos, pos') = pos.loc, pos'.loc

let rec token lexbuf =
  match%sedlex lexbuf with
  | '(' -> LPAREN, locs lexbuf
  | ')' -> RPAREN, locs lexbuf
  | keyword -> ATOM (Sedlexing.Utf8.lexeme lexbuf), locs lexbuf
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
  | Plus (' ' | '\t' | newline | linecomment) -> token lexbuf
  | "(;" ->
      comment (Sedlexing.lexing_positions lexbuf) lexbuf;
      token lexbuf
  | ";)" ->
      raise
        (Error
           ( Sedlexing.lexing_positions lexbuf
           , Printf.sprintf "Unmatched closing comment.\n" ))
  | eof -> EOF, locs lexbuf
  | _ ->
      raise (Error (Sedlexing.lexing_positions lexbuf, Printf.sprintf "Syntax error.\n"))

type t =
  { loc : pos * pos
  ; desc : desc
  }

and desc =
  | Atom of string
  | List of t list

let rec parse_list lexbuf toplevel start_loc acc =
  let tok, loc = token lexbuf in
  match tok with
  | LPAREN ->
      let lst, loc = parse_list lexbuf false loc [] in
      parse_list lexbuf toplevel start_loc ({ desc = List lst; loc } :: acc)
  | RPAREN ->
      if toplevel
      then
        raise
          (Error
             ( Sedlexing.lexing_positions lexbuf
             , Printf.sprintf "Unexpected closing parenthesis.\n" ));
      List.rev acc, (fst start_loc, snd loc)
  | EOF ->
      if not toplevel
      then
        raise
          (Error (position_of_loc start_loc, Printf.sprintf "Unclosed parenthesis.\n"));
      List.rev acc, (fst start_loc, snd loc)
  | ATOM s -> parse_list lexbuf toplevel start_loc ({ loc; desc = Atom s } :: acc)

let parse lexbuf =
  let pos =
    { loc = Sedlexing.lexing_position_start lexbuf
    ; byte_loc = Sedlexing.lexeme_bytes_end lexbuf
    }
  in
  parse_list lexbuf true (pos, pos) []

let is_unsigned_integer s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  match%sedlex lexbuf with
  | uN, eof -> true
  | _ -> false

let hexdigit c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - (Char.code 'a' - 10)
  | 'A' .. 'F' -> Char.code c - (Char.code 'A' - 10)
  | _ -> assert false

let rec parse_string_contents loc lexbuf =
  match%sedlex lexbuf with
  | eof ->
      let s = Buffer.contents string_buffer in
      Buffer.clear string_buffer;
      s
  | Plus (Sub (any, (0 .. 31 | 0x7f | '"' | '\\'))) ->
      Buffer.add_string string_buffer (Sedlexing.Utf8.lexeme lexbuf);
      parse_string_contents loc lexbuf
  | "\\t" ->
      Buffer.add_char string_buffer '\t';
      parse_string_contents loc lexbuf
  | "\\n" ->
      Buffer.add_char string_buffer '\n';
      parse_string_contents loc lexbuf
  | "\\r" ->
      Buffer.add_char string_buffer '\r';
      parse_string_contents loc lexbuf
  | "\\'" ->
      Buffer.add_char string_buffer '\'';
      parse_string_contents loc lexbuf
  | "\\\"" ->
      Buffer.add_char string_buffer '"';
      parse_string_contents loc lexbuf
  | "\\\\" ->
      Buffer.add_char string_buffer '\\';
      parse_string_contents loc lexbuf
  | '\\', hexdigit, hexdigit ->
      let s = Sedlexing.Utf8.lexeme lexbuf in
      assert (String.length s = 3);
      Buffer.add_char string_buffer (Char.chr ((hexdigit s.[1] * 16) + hexdigit s.[2]));
      parse_string_contents loc lexbuf
  | "\\u{", hexnum, "}" -> (
      match
        let s = Sedlexing.Utf8.lexeme lexbuf in
        int_of_string ("0x" ^ String.sub s ~pos:3 ~len:(String.length s - 4))
      with
      | c when Uchar.is_valid c ->
          Buffer.add_utf_8_uchar string_buffer (Uchar.of_int c);
          parse_string_contents loc lexbuf
      | _ | (exception Failure _) ->
          Buffer.clear string_buffer;
          raise
            (Error
               (position_of_loc loc, Printf.sprintf "Invalid Unicode escape sequences.\n"))
      )
  | _ -> assert false

let parse_string loc s =
  parse_string_contents
    loc
    (Sedlexing.Utf8.from_string (String.sub s ~pos:1 ~len:(String.length s - 2)))

let is_string s = String.length s > 0 && Char.equal s.[0] '"'

let is_keyword s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  match%sedlex lexbuf with
  | keyword, eof -> true
  | _ -> false

let is_id s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  match%sedlex lexbuf with
  | id, eof -> true
  | _ -> false

(****)

module StringMap = Map.Make (String)

type typ =
  | Bool
  | String
  | Version

type value =
  | Bool of bool
  | String of string
  | Version of int * int * int

let value_equal (a : value) b = Poly.equal a b

let value_compare (a : value) b = Poly.compare a b

type st =
  { text : string
  ; mutable pos : pos
  ; variables : value StringMap.t
  ; buf : Buffer.t
  }

let value_type v : typ =
  match v with
  | Bool _ -> Bool
  | String _ -> String
  | Version _ -> Version

let type_name (t : typ) =
  match t with
  | Bool -> "boolean"
  | String -> "string"
  | Version -> "version"

let variable_is_set st nm =
  match StringMap.find_opt nm st.variables with
  | Some (Bool true) -> true
  | _ -> false

let check_type ?typ expr actual_typ =
  match typ with
  | None -> ()
  | Some typ ->
      if not (Poly.equal actual_typ typ)
      then
        raise
          (Error
             ( position_of_loc expr.loc
             , Printf.sprintf
                 "Expected a %s but this is a %s.\n"
                 (type_name typ)
                 (type_name actual_typ) ))

let rec eval ?typ st expr =
  match expr with
  | { desc = Atom s; loc } when is_string s ->
      check_type ?typ expr String;
      String (parse_string loc s)
  | { desc = Atom s; loc } when is_keyword s ->
      if not (StringMap.mem s st.variables)
      then
        raise (Error (position_of_loc loc, Printf.sprintf "Unknown variable '%s'.\n" s));
      let res = StringMap.find s st.variables in
      check_type ?typ expr (value_type res);
      res
  | { desc =
        List
          [ { desc = Atom major; _ }
          ; { desc = Atom minor; _ }
          ; { desc = Atom patchlevel; _ }
          ]
    ; _
    }
    when is_unsigned_integer major
         && is_unsigned_integer minor
         && is_unsigned_integer patchlevel ->
      check_type ?typ expr Version;
      Version (int_of_string major, int_of_string minor, int_of_string patchlevel)
  | { desc = List ({ desc = Atom "and"; _ } :: lst); _ } ->
      check_type ?typ expr Bool;
      Bool (List.for_all ~f:(fun expr' -> eval_bool st expr') lst)
  | { desc = List ({ desc = Atom "or"; _ } :: lst); _ } ->
      check_type ?typ expr Bool;
      Bool (List.exists ~f:(fun expr' -> eval_bool st expr') lst)
  | { desc = List [ { desc = Atom "not"; _ }; expr' ]; _ } ->
      check_type ?typ expr Bool;
      Bool (not (eval_bool st expr'))
  | { desc =
        List ({ desc = Atom (("=" | "<" | ">" | "<=" | ">=" | "<>") as op); _ } :: args)
    ; loc
    } -> bin_op st ?typ loc op args
  | { loc; _ } -> raise (Error (position_of_loc loc, Printf.sprintf "Syntax error.\n"))

and eval_bool st expr =
  match eval ~typ:Bool st expr with
  | Bool b -> b
  | _ -> assert false

and bin_op st ?typ loc op args =
  match args with
  | [ expr; expr' ] ->
      check_type ?typ expr Bool;
      let v = eval st expr in
      let v' = eval ~typ:(value_type v) st expr' in
      Bool
        (let op =
           match op with
           | "=" -> ( = )
           | "<" -> ( < )
           | ">" -> ( > )
           | "<=" -> ( <= )
           | ">=" -> ( >= )
           | "<>" -> ( <> )
           | _ -> assert false
         in
         op (value_compare v v') 0)
  | _ -> raise (Error (position_of_loc loc, Printf.sprintf "Syntax error.\n"))

(****)

let write st pos' =
  Buffer.add_substring st.buf st.text st.pos.byte_loc (pos'.byte_loc - st.pos.byte_loc);
  st.pos <- pos'

let skip st (pos' : pos) =
  let lines = pos'.loc.pos_lnum - st.pos.loc.pos_lnum in
  let cols =
    pos'.loc.pos_cnum
    - pos'.loc.pos_bol
    - if lines > 0 then 0 else st.pos.loc.pos_cnum - st.pos.loc.pos_bol
  in
  Buffer.add_string st.buf (String.make (max 0 lines) '\n');
  Buffer.add_string st.buf (String.make (max 0 cols) ' ');
  st.pos <- pos'

let insert st s =
  Buffer.add_string st.buf s;
  let n = String.length s in
  st.pos <-
    { loc = { st.pos.loc with pos_cnum = st.pos.loc.pos_cnum + n }
    ; byte_loc = st.pos.byte_loc - 1
    }

let pred_position { loc; byte_loc } =
  { loc = { loc with pos_cnum = loc.pos_cnum - 1 }; byte_loc = byte_loc - 1 }

let rec rewrite_list st l = List.iter ~f:(rewrite st) l

and rewrite st elt =
  match elt with
  | { desc =
        List
          [ { desc = Atom "@if"; _ }
          ; expr
          ; { desc = List ({ desc = Atom "@then"; loc = _, pos_after_then } :: then_body)
            ; loc = _, pos_after_then_body
            }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      if eval_bool st expr
      then (
        skip st pos_after_then;
        rewrite_list st then_body;
        write st (pred_position pos_after_then_body);
        skip st pos')
      else skip st pos'
  | { desc =
        List
          [ { desc = Atom "@if"; _ }
          ; expr
          ; { desc = List ({ desc = Atom "@then"; loc = _, pos_after_then } :: then_body)
            ; loc = _, pos_after_then_body
            }
          ; { desc = List ({ desc = Atom "@else"; loc = _, pos_after_else } :: else_body)
            ; loc = _, pos_after_else_body
            }
          ]
    ; loc = pos, pos'
    } ->
      write st pos;
      if eval_bool st expr
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
          ({ desc = Atom "@if"; _ }
          :: _
          :: { desc = List ({ desc = Atom "@then"; _ } :: _); _ }
          :: { desc = List ({ desc = Atom "@else"; _ } :: _); _ }
          :: { loc; _ }
          :: _)
    ; _
    } ->
      raise
        (Error (position_of_loc loc, Printf.sprintf "Expecting closing parenthesis.\n"))
  | { desc =
        List
          ({ desc = Atom "@if"; _ }
          :: _
          :: { desc = List ({ desc = Atom "@then"; _ } :: _); _ }
          :: { loc; _ }
          :: _)
    ; _
    } ->
      raise
        (Error
           ( position_of_loc loc
           , Printf.sprintf "Expecting @else clause or closing parenthesis.\n" ))
  | { desc = List ({ desc = Atom "@if"; _ } :: _ :: { loc = pos, pos'; _ } :: _); _ }
  | { desc = List [ { desc = Atom "@if"; _ }; { loc = _, pos; _ } ]; loc = _, pos' } ->
      raise (Error ((pos.loc, pos'.loc), Printf.sprintf "Expecting @then clause.\n"))
  | { desc = List [ { desc = Atom "@if"; loc = _, pos } ]; loc = _, pos' } ->
      raise (Error ((pos.loc, pos'.loc), Printf.sprintf "Expecting condition.\n"))
  | { desc = List ({ desc = Atom (("@then" | "@else") as nm); loc } :: _); _ } ->
      raise
        (Error
           ( position_of_loc loc
           , Printf.sprintf "Unexpected %s clause. Maybe you forgot a parenthesis.\n" nm
           ))
  | { desc =
        List
          [ { desc = Atom "@string"; _ }
          ; { desc = Atom name; loc = loc_name }
          ; { desc = Atom value; loc = loc_value }
          ]
    ; loc = pos, pos'
    } ->
      if not (is_id name) then raise (Error (position_of_loc loc_name, "Expecting an id"));
      if not (is_string value)
      then raise (Error (position_of_loc loc_value, "Expecting a string"));
      let s = parse_string loc_value value in
      write st pos;
      insert
        st
        (Format.asprintf
           "(global %s (ref eq) (array.new_fixed $bytes %d%a))"
           name
           (String.length s)
           (fun f s ->
             String.iter ~f:(fun c -> Format.fprintf f " (i32.const %d)" (Char.code c)) s)
           s);
      skip st pos'
  | { desc = List [ { desc = Atom "@string"; _ }; { desc = Atom value; loc = loc_value } ]
    ; loc = pos, pos'
    } ->
      if not (is_string value)
      then raise (Error (position_of_loc loc_value, "Expecting a string"));
      let s = parse_string loc_value value in
      write st pos;
      insert
        st
        (Format.asprintf
           "(array.new_fixed $bytes %d%a)"
           (String.length s)
           (fun f s ->
             String.iter ~f:(fun c -> Format.fprintf f " (i32.const %d)" (Char.code c)) s)
           s);
      skip st pos'
  | { desc = List [ { desc = Atom "@string"; loc = _, pos } ]; loc = _, pos' } ->
      raise (Error ((pos.loc, pos'.loc), Printf.sprintf "Expecting an id or a string.\n"))
  | { desc = List ({ desc = Atom "@string"; _ } :: _ :: _ :: { loc; _ } :: _); _ } ->
      raise
        (Error (position_of_loc loc, Printf.sprintf "Expecting a closing parenthesis.\n"))
  | { desc = List [ { desc = Atom "@char"; _ }; { desc = Atom value; loc = loc_value } ]
    ; loc = pos, pos'
    } ->
      if
        (not (is_string value))
        ||
        let s = parse_string loc_value value in
        String.length s <> 1 || Char.code s.[0] > 127
      then raise (Error (position_of_loc loc_value, "Expecting an ASCII character"));
      let s = parse_string loc_value value in
      write st pos;
      insert st (Format.asprintf "(i32.const %d)" (Char.code s.[0]));
      skip st pos'
  | { desc = List [ { desc = Atom "@char"; loc = _, pos } ]; loc = _, pos' } ->
      raise (Error ((pos.loc, pos'.loc), Printf.sprintf "Expecting a string.\n"))
  | { desc = List ({ desc = Atom "@char"; _ } :: _ :: _ :: { loc; _ } :: _); _ } ->
      raise
        (Error (position_of_loc loc, Printf.sprintf "Expecting a closing parenthesis.\n"))
  | { desc =
        List
          ({ desc = Atom "func"; loc = _, pos }
          :: { desc =
                 List
                   [ { desc = Atom "export"; _ }
                   ; { desc = Atom export_name; loc = export_loc }
                   ]
             ; loc = pos', _
             }
          :: l)
    ; _
    }
    when variable_is_set st "name-wasm-functions"
         && is_id ("$" ^ parse_string export_loc export_name) ->
      write st pos;
      insert st (Printf.sprintf " $%s " (parse_string export_loc export_name));
      skip st pos';
      rewrite_list st l
  | { desc = List l; _ } -> rewrite_list st l
  | _ -> ()

(****)

let ocaml_version =
  Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun major minor patchlevel ->
      Version (major, minor, patchlevel))

let default_settings = [ "name-wasm-functions", Bool true ]

let f ~variables ~filename ~contents:text =
  let variables =
    List.fold_left
      ~f:(fun m (k, v) -> StringMap.add k v m)
      ~init:StringMap.empty
      (default_settings @ variables)
  in
  let variables = StringMap.add "ocaml_version" ocaml_version variables in
  let lexbuf = Sedlexing.Utf8.from_string text in
  Sedlexing.set_filename lexbuf filename;
  try
    let t, (pos, end_pos) = parse lexbuf in
    let st = { text; pos; variables; buf = Buffer.create (String.length text) } in
    rewrite_list st t;
    write st end_pos;
    Buffer.contents st.buf
  with Error (loc, msg) -> report_error loc msg

type source =
  | Binary
  | File
  | Contents of string

type input =
  { module_name : string
  ; file : string
  ; source : source
  }

let with_preprocessed_files ~variables ~inputs action =
  List.fold_left
    ~f:(fun cont { module_name; file; source } inputs ->
      match
        match source with
        | Binary -> None
        | File ->
            if Link.Wasm_binary.check_file ~file then None else Some (Fs.read_file file)
        | Contents contents -> Some contents
      with
      | None -> cont ({ Binaryen.module_name; file; source_map_file = None } :: inputs)
      | Some contents ->
          let source_file = file in
          Fs.with_intermediate_file (Filename.temp_file module_name ".wat")
          @@ fun file ->
          Fs.write_file
            ~name:file
            ~contents:
              (if Link.Wasm_binary.check ~contents
               then contents
               else f ~variables ~filename:source_file ~contents);
          cont ({ Binaryen.module_name; file; source_map_file = None } :: inputs))
    ~init:action
    inputs
    []
