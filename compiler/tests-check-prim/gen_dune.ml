(** *)

type version =
  [ `V4_13
  | `V4_14
  | `V5_0
  | `V5_1
  | `V5_2
  | `V5_3
  | `V5_4
  | `V5_5
  ]

type variant =
  [ `Mainstream
  | `OxCaml
  ]

let string_of_version : version -> string = function
  | `V4_13 -> "4.13"
  | `V4_14 -> "4.14"
  | `V5_0 -> "5.0"
  | `V5_1 -> "5.1"
  | `V5_2 -> "5.2"
  | `V5_3 -> "5.3"
  | `V5_4 -> "5.4"
  | `V5_5 -> "5.5"

let next_version : version -> version option = function
  | `V4_13 -> Some `V4_14
  | `V4_14 -> Some `V5_0
  | `V5_0 -> Some `V5_1
  | `V5_1 -> Some `V5_2
  | `V5_2 -> Some `V5_3
  | `V5_3 -> Some `V5_4
  | `V5_4 -> Some `V5_5
  | `V5_5 -> None

type os_type =
  | Unix
  | Win32

let string_of_os_type = function
  | Unix -> "Unix"
  | Win32 -> "Win32"

let rule bc ocaml_version os_type ocaml_variant =
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
  let ox =
    [ (match ocaml_variant with
      | `Mainstream -> "(not %{oxcaml_supported})"
      | `OxCaml -> "%{oxcaml_supported}")
    ]
  in
  let enabled_if = Printf.sprintf "(and %s)" (String.concat "" (vl @ vu @ os @ ox)) in

  let target =
    Filename.chop_extension bc
    ^ (match os_type with
      | None -> ""
      | Some os_type -> "-" ^ string_of_os_type os_type)
    ^ "."
    ^ string_of_version ocaml_version
    ^ (match ocaml_variant with
      | `Mainstream -> ""
      | `OxCaml -> "+ox")
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
  let versions : (version * variant) list =
    [ `V4_14, `Mainstream; `V5_4, `Mainstream; `V5_2, `OxCaml ]
  in
  set_binary_mode_out stdout true;
  List.iter
    (fun (ocaml_version, ocaml_variant) ->
      List.iter
        (fun (bc, os_type) -> print_endline (rule bc ocaml_version os_type ocaml_variant))
        [ "main.bc", None; "unix.bc", Some Win32; "unix.bc", Some Unix ])
    versions
