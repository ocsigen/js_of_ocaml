(* Lexer-only benchmark: best of N runs over a file *)
open Js_of_ocaml_compiler
open Stdlib

let () =
  let n = int_of_string Sys.argv.(1) and file = Sys.argv.(2) in
  let ic = open_in_bin file in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let best = ref infinity in
  for _ = 1 to n do
    Gc.full_major ();
    let t0 = Sys.time () in
    let env = Flow_lexer.Lex_env.create (Sedlexing.Utf8.from_string src) in
    let rec loop () =
      match Flow_lexer.lex env with
      | Js_token.T_EOF, _ -> ()
      | _ -> loop ()
    in
    loop ();
    best := Float.min !best (Sys.time () -. t0)
  done;
  Printf.printf "lexer best of %d: %.3fs\n" n !best
