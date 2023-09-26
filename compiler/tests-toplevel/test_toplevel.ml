let () =
  let content = {|
let () = print_endline "hello";;
1+;;
Missing_module.f;;
|} in
  Topdirs.dir_directory "/static/cmis";
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  let lexbuf = Lexing.from_string content in
  while true do
    try
      Location.reset ();
      let phr = !Toploop.parse_toplevel_phrase lexbuf in
      if not (Toploop.execute_phrase true Format.std_formatter phr) then raise Exit
    with
    | End_of_file ->
        flush_all ();
        exit 0
    | x -> Location.report_exception Format.std_formatter x
  done
