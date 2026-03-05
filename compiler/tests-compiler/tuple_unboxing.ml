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
       var runtime = globalThis.jsoo_runtime;
       function f$0(x, y){return x + y | 0;}
       function f(_a_){return f$0(_a_[1], _a_[2]);}
       var _a_ = [0, 1, 2], x = f$0(_a_[1], _a_[2]);
       runtime.caml_register_global(1, [0, f, x], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

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
       var runtime = globalThis.jsoo_runtime;
       function f(b, y, t){var x = b ? 1 : t[1]; return x;}
       var _a_ = [0, 1, 1];
       function g(b, t){var x = b ? _a_[1] : t[1]; return x;}
       runtime.caml_register_global(1, [0, f, g], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

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
           function(_d_, _c_, _b_){
            switch(_d_){
              case 2:
               var _a_ = _b_ + 1 | 0; break;
              case 1:
               var _a_ = _b_; break;
              default: var _a_ = _c_;
            }
            return _a_;
           }];
       runtime.caml_register_global(0, Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]
