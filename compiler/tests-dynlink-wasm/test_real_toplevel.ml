let () =
  Topdirs.dir_directory "/static/cmis";
  Toploop.initialize_toplevel_env ();
  let lb = Lexing.from_string "let x = 1 + 2;;" in
  let phr = !Toploop.parse_toplevel_phrase lb in
  ignore (Toploop.execute_phrase true Format.std_formatter phr)
