open Js_of_ocaml
open Js_of_ocaml_wscript

let () =
  print_endline "starting up Excel...";
  let app = Excel.create () in
  app##.visible := Js._true;
  print_endline "press enter to close it";
  ignore Wscript.stdin##readLine
