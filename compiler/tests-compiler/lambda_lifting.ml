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
        var runtime=globalThis.jsoo_runtime;
        function caml_cps_exact_call0(f)
         {return runtime.caml_stack_check_depth()
                  ?f()
                  :runtime.caml_trampoline_return(f,[])}
        function caml_cps_exact_call1(f,a0)
         {return runtime.caml_stack_check_depth()
                  ?f(a0)
                  :runtime.caml_trampoline_return(f,[a0])}
        function caml_cps_exact_call2(f,a0,a1)
         {return runtime.caml_stack_check_depth()
                  ?f(a0,a1)
                  :runtime.caml_trampoline_return(f,[a0,a1])}
        function caml_cps_call3(f,a0,a1,a2)
         {return runtime.caml_stack_check_depth()
                  ?f.length == 3?f(a0,a1,a2):runtime.caml_call_gen(f,[a0,a1,a2])
                  :runtime.caml_trampoline_return(f,[a0,a1,a2])}
        return runtime.caml_callback
                (function(cont)
                  {var
                    global_data=runtime.caml_get_global_data(),
                    Stdlib_Printf=global_data.Stdlib__Printf,
                    _a_=
                     [0,
                      [4,0,0,0,[12,10,0]],
                      runtime.caml_string_of_jsbytes("%d\n")];
                   function _b_()
                    {function f(x,cont)
                      {function g(y,cont)
                        {function h(z,cont)
                          {return caml_cps_exact_call1(cont,(x + y | 0) + z | 0)}
                         return caml_cps_exact_call0
                                 (function(){return caml_cps_exact_call0(_c_(h,7,cont))})}
                       return caml_cps_exact_call0
                               (function(){return caml_cps_exact_call0(_d_(g,5,cont))})}
                     return caml_cps_exact_call0
                             (function(){return caml_cps_exact_call0(_m_(f,3))})}
                   function _c_(h,_v_,cont)
                    {return function(){return caml_cps_exact_call2(h,_v_,cont)}}
                   function _d_(g,_u_,cont)
                    {return function(){return caml_cps_exact_call2(g,_u_,cont)}}
                   function _e_(Test)
                    {return function()
                      {runtime.caml_register_global(2,Test,"Test");return}}
                   function _f_()
                    {return function()
                      {var Test=[0];return caml_cps_exact_call0(_e_(Test))}}
                   function _g_()
                    {return function(){return caml_cps_exact_call0(_f_())}}
                   function _h_()
                    {return function(_t_){return caml_cps_exact_call0(_g_())}}
                   function _i_(_r_,_s_)
                    {return function(){return caml_cps_call3(_s_,_a_,_r_,_h_())}}
                   function _j_(_q_)
                    {return function()
                      {return caml_cps_exact_call0(_i_(_q_,Stdlib_Printf[2]))}}
                   function _k_()
                    {return function(_p_){return caml_cps_exact_call0(_j_(_p_))}}
                   function _l_(f,_o_)
                    {return function(){return caml_cps_exact_call2(f,_o_,_k_())}}
                   function _m_(f,_n_)
                    {return function(){return caml_cps_exact_call0(_l_(f,_n_))}}
                   return caml_cps_exact_call0(_b_)},
                 [])}
      (globalThis));
    //end |}]
