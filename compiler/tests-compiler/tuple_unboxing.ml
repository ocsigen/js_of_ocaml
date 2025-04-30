open Util

let%expect_test _ =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      {|
      let f (x, y) = x + y
      let x = f(1, 2)
    |}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime, _a_ = [0, 1, 2];
       function f(param){var y = param[2], x = param[1]; return x + y | 0;}
       var x = f(_a_), Test = [0, f, x];
       runtime.caml_register_global(1, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]

let%expect_test _ =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      {|
      type t = {x : int; y : int}
      let f b y t = let {x; _} = if b then {x=1; y} else t in x
      let g b t = let {x; _} = if b then {x=1; y=1} else t in x
    |}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime, _a_ = [0, 1, 1];
       function f(b, y, t){var match = b ? [0, 1] : t, x = match[1]; return x;}
       function g(b, t){var match = b ? _a_ : t, x = match[1]; return x;}
       var Test = [0, f, g];
       runtime.caml_register_global(1, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]

let%expect_test _ =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      ~debug:false
      {|
        type t = C | D | E
        type s = A of int | B of int
        let foo c a b =
          let m =
            match c with
            | C -> A a
            | D -> B b
            | E -> B (b + 1)
          in
          match m with
          | A x -> x
          | B y -> y
    |}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        Test =
          [0,
           function(_c_, _b_, _a_){
            switch(_c_){
              case 0:
               var _d_ = [0, _b_]; break;
              case 1:
               var _d_ = [1, _a_]; break;
              default: var _d_ = [1, _a_ + 1 | 0];
            }
            return _d_[1];
           }];
       runtime.caml_register_global(0, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]
