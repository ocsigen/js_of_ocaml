open Js_of_ocaml_toplevel

let () = JsooTop.initialize ()

let fmt = Format.std_formatter

let () =
  JsooTop.execute
    true
    ~pp_code:fmt
    ~highlight_location:(fun _ -> ())
    fmt
    {|let () = print_endline "hello";;|}
