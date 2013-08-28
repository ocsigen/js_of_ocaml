(*CSS.Angle test suite*)
open Common

let log_stop = log_start "CSS.Angle test suite"

let () =
  let a =
    [ CSS.Angle.Rad  0.1
    ; CSS.Angle.Turns  0.12
    ; CSS.Angle.Deg  5.4
    ; CSS.Angle.Turns  0.
    ; CSS.Angle.Grad 10.0
    ; CSS.Angle.Grad  0.10
    ]
  in
  List.iter
    (fun c ->
      try
        let js = CSS.Angle.js c  in
        let ml = CSS.Angle.ml js in
        if c = ml then
          log_success ()
        else
          log_failure (Printf.sprintf "%s   %s"
            (CSS.Angle.string_of_t c)
            (CSS.Angle.string_of_t ml)
            )
      with
        | Invalid_argument s -> log_failure s
        | Failure s -> log_failure s
    )
    a

let () = log_stop ()
