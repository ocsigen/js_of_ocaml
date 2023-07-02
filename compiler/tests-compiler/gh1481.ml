let%expect_test _ =
  let prog =
    {|
type test = [ `B | `C | `D | `A ]

let to_string (tag : test) =
  match tag with
  | `A -> ("`A")
  | `B -> ("`B")
  | `C -> ("`C")
  | `D -> ("`D")

let correct x y =
  let z =
    match x, y with
    | (`A, v) | (v, `A) -> v
    | `B, _ | _, `B -> `B
    | `C, _ | _, `C -> `C
    | `D, `D -> `D
  in
  z

let incorrect x y =
  match x, y with
  | (`A, v) | (v, `A) -> v
  | `B, _ | _, `B -> `B
  | `C, _ | _, `C -> `C
  | `D, `D -> `D

let () =
  let a = `C in
  Printf.printf "[a] is: %s\n" (to_string a);

  let b = `A in
  Printf.printf "[b] is: %s\n" (to_string b);

  let c = correct a b in
  Printf.printf "[correct a b] is: %s\n" (to_string c);

  let d = incorrect a b in
  Printf.printf "[incorrect a b] is: %s\n" (to_string d);

  |}
  in
  let program = Util.compile_and_parse ~debug:false prog in
  Util.print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
       function caml_call2(f, a0, a1){
        return (f.l >= 0 ? f.l : f.l = f.length) == 2
                ? f(a0, a1)
                : runtime.caml_call_gen(f, [a0, a1]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib_Printf = global_data.Stdlib__Printf,
        cst_D = caml_string_of_jsbytes("`D"),
        cst_C = caml_string_of_jsbytes("`C"),
        cst_B = caml_string_of_jsbytes("`B"),
        cst_A = caml_string_of_jsbytes("`A"),
        _e_ =
          [0,
           [11, caml_string_of_jsbytes("[a] is: "), [2, 0, [12, 10, 0]]],
           caml_string_of_jsbytes("[a] is: %s\n")],
        _g_ =
          [0,
           [11, caml_string_of_jsbytes("[b] is: "), [2, 0, [12, 10, 0]]],
           caml_string_of_jsbytes("[b] is: %s\n")],
        _i_ =
          [0,
           [11, caml_string_of_jsbytes("[correct a b] is: "), [2, 0, [12, 10, 0]]],
           caml_string_of_jsbytes("[correct a b] is: %s\n")],
        _k_ =
          [0,
           [11,
            caml_string_of_jsbytes("[incorrect a b] is: "),
            [2, 0, [12, 10, 0]]],
           caml_string_of_jsbytes("[incorrect a b] is: %s\n")];
       function _a_(_r_){
        return 67 <= _r_ ? 68 <= _r_ ? cst_D : cst_C : 66 <= _r_ ? cst_B : cst_A;
       }
       function _b_(_p_, _o_){
        if(65 === _p_)
         var _q_ = _o_;
        else{
         if(68 === _p_ && 68 === _o_) return 68;
         if(65 !== _o_){
          if(66 !== _o_ && 66 !== _p_){67 === _p_; return 67;}
          return 66;
         }
         var _q_ = _p_;
        }
        return _q_;
       }
       function _c_(_m_, _l_){
        if(65 === _m_)
         var _n_ = _l_;
        else{
         if(68 === _m_ && 68 === _l_) return 68;
         if(65 !== _l_){
          if(66 !== _l_ && 66 !== _m_){67 === _m_; return 67;}
          return 66;
         }
         var _n_ = _m_;
        }
        return _n_;
       }
       var _d_ = _a_(67);
       caml_call2(Stdlib_Printf[2], _e_, _d_);
       var _f_ = _a_(65);
       caml_call2(Stdlib_Printf[2], _g_, _f_);
       var _h_ = _a_(_b_(67, 65));
       caml_call2(Stdlib_Printf[2], _i_, _h_);
       var _j_ = _a_(_c_(67, 65));
       caml_call2(Stdlib_Printf[2], _k_, _j_);
       var Test = [0, _a_, _b_, _c_];
       runtime.caml_register_global(9, Test, "Test");
       return;
      }
      (globalThis));
    //end |}];
  Util.compile_and_run ~debug:false prog;
  [%expect {|
    [a] is: `C
    [b] is: `A
    [correct a b] is: `C
    [incorrect a b] is: `C |}]
