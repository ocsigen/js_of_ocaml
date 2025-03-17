let () = Js_of_ocaml_toplevel.JsooTop.initialize ()

let fmt = Format.std_formatter

let () =
  Js_of_ocaml.Sys_js.set_channel_flusher stderr (fun str ->
      let rec iter = function
        | [] -> ()
        | [ "" ] -> ()
        | [ x ] -> Printf.printf "<ERR>: %s" x
        | x :: xs ->
            Printf.printf "<ERR>: %s\n" x;
            iter xs
      in
      iter (String.split_on_char '\n' str))

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

let () =
  let open Js_of_ocaml.Js in
  Unsafe.fun_call (Unsafe.js_expr "require") [| Unsafe.coerce (string "./re.cmis.js") |]

let () =
  Js_of_ocaml_toplevel.JsooTop.execute
    true
    ~pp_code:fmt
    ~highlight_location:(fun _ -> ())
    fmt
    {|
let () =
  let regex = Re.compile Re.(seq [str "//"; rep print ]) in
  Printf.printf "%b\n%!" (Re.execp regex "// a C comment");
  Printf.printf "%b\n%!" (Re.execp ~pos:1 regex "// a C comment");;
|}
