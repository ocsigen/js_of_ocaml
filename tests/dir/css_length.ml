(* CSS.Length testing *)
open Common
let log_stop = log_start "CSS.Length test suite"

let () =
  let ls =
    [ CSS.Length.Em  0.1
    ; CSS.Length.Ex  0.12
    ; CSS.Length.Px  5.4
    ; CSS.Length.Gd  0.
    ; CSS.Length.Rem 10.0
    ; CSS.Length.Vw  0.10
    ; CSS.Length.Vh  0.1
    ; CSS.Length.Vm  0.5
    ; CSS.Length.Ch  20.6
    ; CSS.Length.Zero
    ; CSS.Length.Mm 100.
    ; CSS.Length.Cm 10.1
    ; CSS.Length.In 0.1
    ; CSS.Length.Pt 10.2
    ; CSS.Length.Pc 103.1
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Length.js c  in
        let ml = CSS.Length.ml js in
        if c = ml then
          log_success ()
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Length.string_of_t c)
            (CSS.Length.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    ls


let () = log_stop ()
