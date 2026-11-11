(* Dump every token with its location, to compare lexer versions. *)
open Js_of_ocaml_compiler
open Stdlib

let read_file f =
  let ic = open_in_bin f in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let () =
  Config.Flag.enable "debuginfo";
  List.iter
    (List.tl (Array.to_list Sys.argv))
    ~f:(fun file ->
      let src = read_file file in
      let errors = ref [] in
      let report_error e = errors := e :: !errors in
      let run kind =
        Parse_js.parse' kind (Parse_js.Lexer.of_string ~report_error ~filename:file src)
      in
      Printf.printf "== %s\n" file;
      match
        try Ok (run `Script)
        with Parse_js.Parsing_error _ -> (
          errors := [];
          try Ok (run `Module) with Parse_js.Parsing_error (pi, _) -> Error pi)
      with
      | Error pi -> Printf.printf "PARSE ERROR %d:%d\n" pi.Parse_info.line pi.col
      | Ok (_, toks) ->
          List.iter toks ~f:(fun (tok, loc) ->
              let p2 = Loc.p2 loc in
              Printf.printf
                "%d:%d-%d:%d %d %s\n"
                (Loc.line loc)
                (Loc.column loc)
                p2.Lexing.pos_lnum
                (p2.Lexing.pos_cnum - p2.Lexing.pos_bol)
                (Loc.cnum loc)
                (Js_token.to_string_extra tok));
          List.iter (List.rev !errors) ~f:(fun e ->
              Printf.printf "LEXER ERROR ";
              Parse_js.Lexer.print_error e))
