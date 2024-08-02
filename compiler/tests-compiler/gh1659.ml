let%expect_test _ =
  let prog = {|
let x = (0., 0.) = (-0., 0.);;

Printf.printf "%B\n" x;;
  |} in
  Util.compile_and_run prog;
  [%expect {|
    true |}]

let%expect_test _ =
  let prog = {|
external equals : 'a -> 'a -> bool = "caml_js_equals";;
let x = equals (0, 0) (0, 0);;
Printf.printf "%B\n" x;;
  |} in
  Util.compile_and_run prog;
  [%expect {|
    false |}]