let () =
  let content =
    {|
let () = print_endline "hello";;
1+1;;
1+;;
Missing_module.f;;
let y = float 1 /. float 3;;
external pure_js_expr : string -> 'a = "caml_pure_js_expr";;
external js_to_string : 'a -> string = "caml_js_to_string";;
let () = print_endline (js_to_string (pure_js_expr "'hello from JS'"));;
|}
  in
  Topdirs.dir_directory "/static/cmis";
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  let lexbuf = Lexing.from_string content in
  while true do
    try
      Location.reset ();
      let phr = !Toploop.parse_toplevel_phrase lexbuf in
      let res = Toploop.execute_phrase true Format.std_formatter phr in
      flush_all ();
      if not res then raise Exit
    with
    | End_of_file -> exit 0
    | x -> Location.report_exception Format.std_formatter x
  done
