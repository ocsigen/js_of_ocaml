let%expect_test _ =
  let prog =
    {|
type t =
  | A
  | B
  | C

let f x =
  while
    match x with
    | A -> true
    | B -> true
    | C -> true
  do
    ()
  done
  |}
  in
  let js = Util.compile_and_parse prog in
  Util.print_program js;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function f(x){for(;;) ;}
       var Test = [0, f];
       runtime.caml_register_global(0, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
