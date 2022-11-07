let () =
  let content = {|
let () = print_endline "hello";;
1+;;|} in
  Topdirs.dir_directory "/static/cmis";
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  let lexbuf = Lexing.from_string content in
  let phr = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase true Format.std_formatter phr)
