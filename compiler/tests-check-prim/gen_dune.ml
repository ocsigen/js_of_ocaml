(** *)

type version =
  [ `V4_13
  | `V4_14
  | `V5_0
  | `V5_1
  | `V5_2
  | `V5_3
  | `V5_4
  ]

let string_of_version : version -> string = function
  | `V4_13 -> "4.13"
  | `V4_14 -> "4.14"
  | `V5_0 -> "5.0"
  | `V5_1 -> "5.1"
  | `V5_2 -> "5.2"
  | `V5_3 -> "5.3"
  | `V5_4 -> "5.4"

let next_version : version -> version option = function
  | `V4_13 -> Some `V4_14
  | `V4_14 -> Some `V5_0
  | `V5_0 -> Some `V5_1
  | `V5_1 -> Some `V5_2
  | `V5_2 -> Some `V5_3
  | `V5_3 -> Some `V5_4
  | `V5_4 -> None

type os_type =
  | Unix
  | Win32

let string_of_os_type = function
  | Unix -> "Unix"
  | Win32 -> "Win32"

let rule bc ocaml_version os_type =
  let vl =
    [ Printf.sprintf "(>= %%{ocaml_version} %s)" (string_of_version ocaml_version) ]
  in
  let vu =
    match next_version ocaml_version with
    | None -> []
    | Some up -> [ Printf.sprintf "(< %%{ocaml_version} %s)" (string_of_version up) ]
  in
  let os =
    match os_type with
    | None -> []
    | Some os_type -> [ Printf.sprintf "(= %%{os_type} %s)" (string_of_os_type os_type) ]
  in
  let enabled_if = Printf.sprintf "(and %s)" (String.concat "" (vl @ vu @ os)) in

  let target =
    Filename.chop_extension bc
    ^ (match os_type with
      | None -> ""
      | Some os_type -> "-" ^ string_of_os_type os_type)
    ^ "."
    ^ string_of_version ocaml_version
    ^ ".output"
  in
  Printf.sprintf
    {|(rule
 (targets %s)
 (mode
  (promote (until-clean)))
 (enabled_if %s)
 (action
  (with-stdout-to
   %%{targets}
   (run
    %%{bin:js_of_ocaml}
    check-runtime
    +dynlink.js
    +toplevel.js
    %%{dep:%s}))))
|}
    target
    enabled_if
    bc

let () =
  let versions : version list = [ `V4_14; `V5_2; `V5_3 ] in
  List.iter
    (fun ocaml_version ->
      List.iter
        (fun (bc, os_type) -> print_endline (rule bc ocaml_version os_type))
        [ "main.bc", None; "unix.bc", Some Win32; "unix.bc", Some Unix ])
    versions
