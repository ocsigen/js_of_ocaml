open Js_of_ocaml
open Js_of_ocaml_wscript

let () =
  let conn = Js.Unsafe.coerce (new%js Wscript.obj (Js.string "ADODB.Connection")) in
  conn##_open (Js.Unsafe.global##._WScript##.arguments##.unnamed##item 0);
  let cmd = Js.Unsafe.coerce (new%js Wscript.obj (Js.string "ADODB.Command")) in
  cmd##.activeConnection := conn;
  cmd##.parameters##append (cmd##createParameter (Js.string "p") 8 1 0 (Js.string "x"));
  Wscript.set_prop cmd "parameters" [|Js.(Unsafe.inject (string "p"))|] (Js.string "it works!");
  cmd##.commandText := Js.string "SELECT ? f";
  let rs = cmd##execute in
  while not (Js.to_bool rs##.eof) do
    print_endline (Js.to_string (rs##fields (Js.string "f"))##.value);
    rs##moveNext
  done
