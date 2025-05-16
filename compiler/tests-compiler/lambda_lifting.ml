let%expect_test _ =
  let prog =
    {|
let f x =
  let g y =
    let h z = x + y + z in
    h 7
  in
  g 5
in
Printf.printf "%d\n" (f 3)
    |}
  in
  let flags =
    [ "--no-inline"; "--set=lifting-threshold=1"; "--set=lifting-baseline=0" ]
  in
  Util.compile_and_run ~effects:`Cps ~flags prog;
  [%expect {|15 |}];
  let program = Util.compile_and_parse ~effects:`Cps ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        global_data = runtime.caml_get_global_data(),
        Stdlib_Printf = global_data.Stdlib__Printf;
       function h(x, y){function h(z){return (x + y | 0) + z | 0;} return h;}
       function g(x){function g(y){var h$0 = h(x, y); return h$0(7);} return g;}
       function f(x){var g$0 = g(x); return g$0(5);}
       var _a_ = f(3);
       runtime.caml_callback
        (Stdlib_Printf[2],
         [[0, [4, 0, 0, 0, [12, 10, 0]], runtime.caml_string_of_jsbytes("%d\n")],
          _a_]);
       var Test = [0];
       runtime.caml_register_global(2, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
