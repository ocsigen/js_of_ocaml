let%expect_test _ =
  let prog =
    {|
let f x =
  let g y =
    let h z = x + y + z in
    h 7
  in
  g
    |}
  in
  let flags = [ "--enable"; "es6" ] in
  let program = Util.compile_and_parse ~effects:false ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        f =
          function(x){var g = function(y){return (x + y | 0) + 7 | 0;}; return g;},
        Test = [0, f];
       runtime.caml_register_global(0, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]
