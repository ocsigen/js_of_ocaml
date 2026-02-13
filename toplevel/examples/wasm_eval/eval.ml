open Js_of_ocaml

(* Evaluate OCaml code, return toplevel formatter output as a JS string *)
let execute (code_js : Js.js_string Js.t) : Js.js_string Js.t =
  let code = Js.to_string code_js in
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  let lexbuf = Lexing.from_string code in
  (try
     while true do
       try
         Location.reset ();
         let phr = !Toploop.parse_toplevel_phrase lexbuf in
         ignore (Toploop.execute_phrase true ppf phr : bool);
         flush_all ()
       with
       | End_of_file -> raise End_of_file
       | x -> Location.report_exception ppf x
     done
   with End_of_file -> ());
  Format.pp_print_flush ppf ();
  Js.string (Buffer.contents buffer)

let () =
  Topdirs.dir_directory "/static/cmis";
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  (* Export execute function to globalThis.wasmExecute *)
  Js.Unsafe.set Js.Unsafe.global (Js.string "wasmExecute") (Js.wrap_callback execute)
