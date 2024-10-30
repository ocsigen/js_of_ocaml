open Js_of_ocaml_compiler
open Stdlib

let exe =
  match Sys.os_type with
  | "Cygwin" | "Win32" -> fun x -> x ^ ".exe"
  | "Unix" | _ -> fun x -> x

let node = try Sys.getenv "NODE" with Not_found -> exe "node"

let failure_expected = ref false

let progress = ref false

let verbose = ref false

let flags, files =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.partition ~f:(fun x -> Char.equal (String.get x 0) '-')

let rec ls_files path =
  match Unix.lstat path with
  | { st_kind = S_DIR; _ } ->
      Sys.readdir path
      |> Array.to_list
      |> List.concat_map ~f:(fun name -> ls_files (Filename.concat path name))
  | { st_kind = S_REG; _ } -> [ path ]
  | { st_kind = S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK; _ } -> []

let files =
  List.concat_map files ~f:(fun path ->
      let l = ls_files path in
      List.filter l ~f:(fun name -> Filename.check_suffix name ".js"))

let () = if !verbose then Printf.eprintf "Found %d files\n%!" (List.length files)

let () =
  List.iter flags ~f:(function
    | "--fail" -> failure_expected := true
    | "-p" | "--progress" -> progress := true
    | "-v" | "--verbose" -> verbose := true
    | f -> failwith ("unrecognised flag " ^ f))

type error =
  | Diff of Javascript.program * Javascript.program
  | Print_parse of Parse_info.t * string
  | Parse of Parse_info.t * string
  | Parse_warning of Parse_js.Lexer.error list
  | Tok_missmatch of string

let unsupported = ref []

let negative = ref []

let noStrict = ref []

let fail = ref []

let pass = ref []

let normalize_string s =
  let l = String.length s in
  let b = Buffer.create (String.length s + 2) in
  for i = 0 to l - 1 do
    let c = s.[i] in
    match c with
    | '\000' when i = l - 1 || not (Char.is_num s.[i + 1]) -> Buffer.add_string b "\\0"
    | '\b' -> Buffer.add_string b "\\b"
    | '\t' -> Buffer.add_string b "\\t"
    | '\n' -> Buffer.add_string b "\\n"
    (* This escape sequence is not supported by IE < 9
       | '\011' -> "\\v"
    *)
    | '\012' -> Buffer.add_string b "\\f"
    (* https://github.com/ocsigen/js_of_ocaml/issues/898 *)
    | '/' when i > 0 && Char.equal s.[i - 1] '<' -> Buffer.add_string b "\\/"
    | '\r' -> Buffer.add_string b "\\r"
    | '\000' .. '\031' | '\127' ->
        Buffer.add_string b "\\x";
        Buffer.add_char_hex b c
    | _ -> Buffer.add_char b c
  done;
  Buffer.contents b

class clean_loc =
  object
    inherit Js_traverse.map as super

    method! parse_info _ = Parse_info.zero

    method! loc _ = N

    method! expression e =
      match e with
      | EStr (Utf8 x) -> EStr (Utf8_string.of_string_exn (normalize_string x))
      | e -> super#expression e
  end

let clean_loc = new clean_loc

let clean_loc p = clean_loc#program p

let p_to_string p =
  let buffer = Buffer.create 100 in
  let pp = Pretty_print.to_buffer buffer in
  let _ = Js_output.program pp p in
  Buffer.contents buffer

let accepted_by_node file =
  let ((ic, oc, ec) as ic_oc) =
    Unix.open_process_full (Printf.sprintf "%s --check %s" node file) [||]
  in
  let pid = Unix.process_full_pid ic_oc in
  let _pid, status = Unix.waitpid [] pid in
  close_in ic;
  close_out oc;
  close_in ec;
  match status with
  | WEXITED 0 -> true
  | WEXITED _ -> false
  | WSIGNALED _ | WSTOPPED _ -> assert false

let should_ignore_feature = function
  | "import-assertions" | "import-attributes" | "decorators" -> true
  | _ -> false

let should_ignore_flag = function
  | "noStrict" -> true
  | _ -> false

let token_equal : Js_token.t -> Js_token.t -> bool =
 fun a b ->
  match a, b with
  | T_SEMICOLON, T_VIRTUAL_SEMICOLON | T_VIRTUAL_SEMICOLON, T_SEMICOLON -> true
  | T_DECR_NB, T_DECR | T_DECR, T_DECR_NB | T_INCR_NB, T_INCR | T_INCR, T_INCR_NB -> true
  | T_IDENTIFIER (Utf8 a, _), T_IDENTIFIER (Utf8 b, _) -> String.equal a b
  | T_STRING (Utf8 a, _), T_STRING (Utf8 b, _) ->
      String.equal a b || String.equal (normalize_string a) (normalize_string b)
  | a, T_IDENTIFIER (Utf8 b, _) when Poly.(Some a = Js_token.is_keyword b) -> true
  | T_IDENTIFIER (Utf8 a, _), b when Poly.(Some b = Js_token.is_keyword a) -> true
  | a, b -> Poly.(a = b)

let rec check_toks
    (a : (Js_token.t * _) list)
    (stack_a : Js_token.t list)
    (b : (Js_token.t * _) list)
    (stack_b : Js_token.t list) : _ =
  match a, b with
  | ( ( ( TComment _
        | T_EOF
        | T_LPAREN
        | T_LPAREN_ARROW
        | T_RPAREN
        | T_SEMICOLON
        | T_VIRTUAL_SEMICOLON
        | T_COMMA )
      , _ )
      :: a
    , b ) -> check_toks a stack_a b stack_b
  | ( a
    , ( ( TComment _
        | T_EOF
        | T_LPAREN
        | T_LPAREN_ARROW
        | T_RPAREN
        | T_SEMICOLON
        | T_VIRTUAL_SEMICOLON
        | T_COMMA )
      , _ )
      :: b ) -> check_toks a stack_a b stack_b
  | (ta, loc_a) :: ra, (tb, _loc_b) :: rb ->
      if token_equal ta tb
      then check_toks ra stack_a rb stack_b
      else
        Error
          (Printf.sprintf
             "token mismatch (%d:%d) %s <> %s\n"
             (Loc.line loc_a)
             (Loc.column loc_a)
             (Js_token.to_string ta)
             (Js_token.to_string tb))
  | [], [] -> Ok ()
  | [], (tb, _) :: _rb ->
      Error (Printf.sprintf "token mismatch <EOF> vs %s\n" (Js_token.to_string tb))
  | (ta, _) :: _ra, [] ->
      Error (Printf.sprintf "token mismatch %s vs <EOF>\n" (Js_token.to_string ta))

let to_file p =
  let s = p_to_string p in
  let f, oc = Filename.open_temp_file "as" "WE" in
  output_string oc s;
  output_string oc "\n";
  close_out oc;
  f

let patdiff p1 p2 =
  ignore (Sys.command (Printf.sprintf "patdiff %s %s" (to_file p1) (to_file p2)));
  flush_all ()

let print_loc_error (pi : Parse_info.t) c =
  List.iteri (String.split_on_char ~sep:'\n' c) ~f:(fun i c ->
      let diff = abs (i + 1 - pi.line) in
      if diff = 0
      then (
        let b = Buffer.create (String.length c) in
        String.fold_utf_8 c () ~f:(fun () i u ->
            if i = pi.col then Buffer.add_utf_8_uchar b (Uchar.of_int 0x274C);
            Buffer.add_utf_8_uchar b u);
        Printf.eprintf "%s\n" (Buffer.contents b))
      else if diff < 5
      then Printf.eprintf "%s\n" c
      else ());
  Printf.eprintf "\n"

let () =
  let negative_r = Str.regexp_string "negative:" in
  let features_r = Str.regexp {|features: ?\[\([a-zA-Z. ,-]*\)\]|} in
  let flags_r = Str.regexp {|flags: ?\[\([a-zA-Z. ,-]*\)\]|} in
  let total = List.length files in
  List.iteri files ~f:(fun i filename ->
      let () = if !progress then Printf.eprintf "%d/%d\r%!" i total in
      let ic = open_in_bin filename in
      let content = In_channel.input_all ic in
      let errors = ref [] in
      let add r = r := (filename, content) :: !r in
      close_in ic;
      let mode =
        match Str.search_forward negative_r content 0 with
        | _ -> `Negative
        | exception Not_found -> (
            let features =
              match Str.search_forward features_r content 0 with
              | _ ->
                  String.split_on_char ~sep:',' (Str.matched_group 1 content)
                  |> List.map ~f:String.trim
              | exception Not_found -> []
            in
            let flags =
              match Str.search_forward flags_r content 0 with
              | _ ->
                  String.split_on_char ~sep:',' (Str.matched_group 1 content)
                  |> List.map ~f:String.trim
              | exception Not_found -> []
            in
            match List.find_opt ~f:should_ignore_feature features with
            | Some f -> `Unsupported f
            | None -> (
                match List.find_opt ~f:should_ignore_flag flags with
                | Some f -> `Unsupported f
                | None -> `Ok))
      in
      match mode with
      | `Negative -> negative := filename :: !negative
      | `Unsupported "noStrict" -> noStrict := filename :: !noStrict
      | `Unsupported _ -> unsupported := (filename, content) :: !unsupported
      | `Ok -> (
          try
            let p1, toks1 =
              Parse_js.Lexer.of_string
                ~report_error:(fun e -> errors := e :: !errors)
                ~filename
                content
              |> Parse_js.parse'
            in
            let p1 = List.concat_map p1 ~f:snd in
            match List.rev !errors with
            | [] -> (
                let s = p_to_string p1 in
                try
                  let p2, toks2 =
                    Parse_js.Lexer.of_string
                      ~report_error:(fun e -> errors := e :: !errors)
                      ~filename
                      s
                    |> Parse_js.parse'
                  in
                  let p2 = List.concat_map p2 ~f:snd in
                  match
                    Poly.(clean_loc p1 = clean_loc p2), check_toks toks1 [] toks2 []
                  with
                  | true, Error s when false ->
                      fail := (Tok_missmatch s, filename) :: !fail
                  | true, _ -> add pass
                  | false, _ -> fail := (Diff (p1, p2), filename) :: !fail
                with Parse_js.Parsing_error loc ->
                  if not (accepted_by_node filename)
                  then add unsupported
                  else fail := (Print_parse (loc, s), filename) :: !fail)
            | l ->
                if accepted_by_node filename
                then fail := (Parse_warning l, filename) :: !fail
          with
          | Parse_js.Parsing_error loc ->
              if not (accepted_by_node filename)
              then add unsupported
              else fail := (Parse (loc, content), filename) :: !fail
          | e ->
              Printf.eprintf "Unexpected error %s\n%s\n" filename (Printexc.to_string e)));
  Printf.printf "Summary:\n";
  Printf.printf "  invalid    : %d\n" (List.length !negative);
  Printf.printf "  not scrict : %d\n" (List.length !noStrict);
  Printf.printf "  skip       : %d\n" (List.length !unsupported);
  Printf.printf "  fail       : %d\n" (List.length !fail);
  Printf.printf "  pass       : %d\n" (List.length !pass);
  flush_all ();
  let l = !fail in
  if !failure_expected
  then (
    List.iter !pass ~f:(fun (f, c) ->
        Printf.printf "succeded to parse %s\n" f;
        Printf.printf "%s\n\n" c);
    match !pass with
    | [] -> exit 0
    | _ -> exit 1)
  else (
    List.iter l ~f:(fun (reason, f) ->
        match reason with
        | Parse (pi, content) ->
            Printf.eprintf "<ERROR>: Parse_error: %s:%d:%d\n" f pi.line pi.col;
            print_loc_error pi content
        | Print_parse (pi, content) ->
            Printf.eprintf "<ERROR>: Parse_error(roundtrip): %s:%d:%d\n" f pi.line pi.col;
            print_loc_error pi content
        | Diff (p1, p2) ->
            Printf.eprintf "<ERROR>: Diff: %s\n%!" f;
            patdiff p1 p2
        | Parse_warning l ->
            Printf.eprintf "<ERROR>: Lexer warning: %s\n" f;
            List.iter l ~f:Parse_js.Lexer.print_error
        | Tok_missmatch s ->
            Printf.eprintf "<ERROR>: Tok mismatch: %s\n" f;
            Printf.eprintf "%s\n" s);
    match !fail with
    | [] -> exit 0
    | _ -> exit 1)
