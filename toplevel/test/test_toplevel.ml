let () = Js_of_ocaml_toplevel.JsooTop.initialize ()

let fmt = Format.std_formatter

let () =
  Js_of_ocaml.Sys_js.set_channel_flusher stderr (fun str -> Printf.printf "<ERR>: %s" str)

let () =
  Js_of_ocaml_toplevel.JsooTop.execute
    true
    ~pp_code:fmt
    ~highlight_location:(fun _ -> ())
    fmt
    {|
external parseInt : float -> int = "parseInt"
let f = 3.14
let () = Printf.printf "parseInt(%f) = %d\n" f (parseInt f);;
|}

let () =
  Js_of_ocaml_toplevel.JsooTop.execute
    true
    ~pp_code:fmt
    ~highlight_location:(fun _ -> ())
    fmt
    {|
let () = print_endline "hello";;
1+;;|}
