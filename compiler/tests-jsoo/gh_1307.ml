let test content =
  Printf.printf "input: %S\n" content;
  flush_all ();
  match Parser_1307.root Lexer_1307.token (Lexing.from_string content) with
  | n ->
      Printf.printf "%d\n" n;
      print_endline "success"
  | exception e ->
      print_endline (Printexc.to_string e);
      print_endline "failure"

let%expect_test "parsing" =
  (* use [Parsing.set_trace true] once https://github.com/janestreet/ppx_expect/issues/43 is fixed *)
  let (old : bool) = Parsing.set_trace false in
  test "a";
  [%expect {|
    input: "a"
    Stdlib.Parsing.Parse_error
    failure |}];
  test "aa";
  [%expect {|
    input: "aa"
    0
    success |}];
  test "aaa";
  [%expect {|
    input: "aaa"
    Stdlib.Parsing.Parse_error
    failure |}];
  let (_ : bool) = Parsing.set_trace old in
  ()
