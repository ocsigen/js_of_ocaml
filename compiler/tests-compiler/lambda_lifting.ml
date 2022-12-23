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
  let flags = [ "--no-inline"; "--set=lifting-threshold=1" ] in
  Util.compile_and_run ~effects:true ~flags prog;
  [%expect {|15 |}];
  let program = Util.compile_and_parse ~effects:true ~flags prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis)
       {"use strict";
        var runtime=globalThis.jsoo_runtime,caml_callback=runtime.caml_callback;
        function caml_cps_exact_call1(f,a0)
         {return runtime.caml_stack_check_depth()
                  ?f(a0)
                  :runtime.caml_trampoline_return(f,[a0])}
        function caml_cps_exact_call2(f,a0,a1)
         {return runtime.caml_stack_check_depth()
                  ?f(a0,a1)
                  :runtime.caml_trampoline_return(f,[a0,a1])}
        return caml_callback
                (function(cont)
                  {var
                    global_data=runtime.caml_get_global_data(),
                    Stdlib_Printf=global_data.Stdlib__Printf,
                    _c_=
                     [0,
                      [4,0,0,0,[12,10,0]],
                      runtime.caml_string_of_jsbytes("%d\n")];
                   function f(x,cont)
                    {var g$0=g(x);return caml_cps_exact_call2(g$0,5,cont)}
                   function h(x,y)
                    {function h(z,cont)
                      {return caml_cps_exact_call1(cont,(x + y | 0) + z | 0)}
                     return h}
                   function g(x)
                    {function g(y,cont)
                      {var h$0=h(x,y);return caml_cps_exact_call2(h$0,7,cont)}
                     return g}
                   var _a_=3,_b_=caml_callback(f,[_a_]),_d_=Stdlib_Printf[2];
                   caml_callback(_d_,[_c_,_b_]);
                   var Test=[0];
                   runtime.caml_register_global(2,Test,"Test");
                   return},
                 [])}
      (globalThis));
    //end |}]
