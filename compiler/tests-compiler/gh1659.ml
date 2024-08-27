let%expect_test _ =
  let prog =
    {|
let f a b = a = b
let () = Printf.printf "(0., 0.) = (-0., 0.) => %B\n" (f (0., 0.) (-0., 0.))
let f a b = a = b
let () = Printf.printf "0. = -0. => %B\n" (f 0. (-0.));;
let f a b = a = b
let nan1 = 0. /. 0.
let nan2 = 0. /. 0.
let () = Printf.printf "nan = nan => %B\n" (f nan1 nan2);;
  |}
  in
  Util.compile_and_run prog;
  [%expect
    {|
    (0., 0.) = (-0., 0.) => true
    0. = -0. => true
    nan = nan => false
    |}]

let%expect_test _ =
  let prog =
    {|
external equals : 'a -> 'a -> bool = "caml_js_equals";;
let () = Printf.printf "x = (0., 0.); x = x => %B\n" (let x = (0., 0.) in equals x x)
let () = Printf.printf "(0., 0.) = (0., 0.) => %B\n" (equals (0., 0.) (0., 0.))
let () = Printf.printf "0. = -0. => %B\n" (equals 0. (-0.));;
let nan1 = 0. /. 0.
let nan2 = 0. /. 0.
let () = Printf.printf "nan = nan => %B\n" (equals nan1 nan2);;
  |}
  in
  Util.compile_and_run prog;
  [%expect
    {|
    x = (0., 0.); x = x => true
    (0., 0.) = (0., 0.) => false
    0. = -0. => true
    nan = nan => false
    |}]

let%expect_test _ =
  let prog =
    {|
external equals : 'a -> 'a -> bool = "caml_js_strict_equals";;
let () = Printf.printf "x = (0., 0.); x = x => %B\n" (let x = (0., 0.) in equals x x)
let () = Printf.printf "(0., 0.) = (0., 0.) => %B\n" (equals (0., 0.) (0., 0.))
let () = Printf.printf "0. = -0. => %B\n" (equals 0. (-0.));;
let nan1 = 0. /. 0.
let nan2 = 0. /. 0.
let () = Printf.printf "nan = nan => %B\n" (equals nan1 nan2);;
  |}
  in
  Util.compile_and_run prog;
  [%expect
    {|
    x = (0., 0.); x = x => true
    (0., 0.) = (0., 0.) => false
    0. = -0. => true
    nan = nan => false
    |}]
