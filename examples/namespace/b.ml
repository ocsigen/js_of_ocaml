let () = print_endline "B"

exception Exn

let try_with f = try f () with Exn -> ()

let raise_ () = raise Exn

let () = Js_of_ocaml.Js.export "tryWith" try_with

let () = Js_of_ocaml.Js.export "raise" raise_
