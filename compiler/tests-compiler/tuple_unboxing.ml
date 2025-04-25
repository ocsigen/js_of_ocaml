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
       runtime.caml_register_global([0, f, x], "Test");
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
       runtime.caml_register_global([0, f, g], "Test");
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
           function(_c_, _b_, _a_){
            switch(_c_){
              case 2:
               _a_ = _a_ + 1 | 0; break;
              case 1: break;
              default: _a_ = _b_;
            }
            return _a_;
           }];
       runtime.caml_register_global(Test, "Test");
       return;
      }
      (globalThis));
    //end
    |}]

(* Blocks must not be unboxed when they can be mutated between the
   start of their scope and a field access. *)
let%expect_test "mutation through an alias" =
  compile_and_run
    ~flags:[ "--no-inline" ]
    {|
      let f r1 r2 = incr r2; !r1
      let () = let r = ref 0 in Printf.printf "%d\n" (f r r)

      let r = ref 0
      let g x = incr r; !x
      let () = Printf.printf "%d\n" (g r)
    |};
  [%expect {|
    1
    1
    |}]

let%expect_test "mutation in a loop" =
  compile_and_run
    ~flags:[ "--no-inline" ]
    {|
      let f r g = let a = !r in for _ = 1 to 3 do print_int (a + !r); g () done
      let () = let r = ref 0 in f r (fun () -> incr r); print_newline ()
    |};
  [%expect {| 012 |}]

let%expect_test "access from a nested closure" =
  compile_and_run
    ~flags:[ "--no-inline" ]
    {|
      let f r = let _ = !r in fun () -> !r
      let () = let r = ref 0 in let g = f r in r := 1; Printf.printf "%d\n" (g ())
    |};
  [%expect {| 1 |}]

(* Float blocks are unboxed at block parameters, but not at function
   parameters, since function parameters are boxed in JavaScript. *)
let%expect_test "float blocks" =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      {|
      type p = { x : float; y : float }
      let norm { x; y } = sqrt ((x *. x) +. (y *. y))
      let sel b a c = let r = if b then { x = a; y = c } else { x = c; y = a } in r.x -. r.y
      let _ = norm { x = 3.; y = 4. }
    |}
  in
  print_fun_decl program (Some "norm");
  print_fun_decl program (Some "sel");
  [%expect
    {|
           function norm(param){
            var y = param[2], x = param[1];
            return Math.sqrt(x * x + y * y);
           }
           //end
           function sel(b, a, c){
            if(b) var _b_ = c, _a_ = a; else{_b_ = a; _a_ = c;}
            return _a_ - _b_;
           }
           //end
           |}]

let%expect_test "mutations through other functions, closures and Obj" =
  compile_and_run
    {|
      type r = { mutable a : int; b : int }
      let[@inline never] m r = r.a <- 5
      let t1 c =
        let r = if c then { a = 1; b = 2 } else { a = 3; b = 4 } in
        let x = r.a in m r; x + r.a
      let t2 c =
        let r = if c then { a = 1; b = 2 } else { a = 3; b = 4 } in
        let x = r.a in let k () = r.a <- 7 in k (); x + r.a
      let t3 c =
        let r = if c then (1, 2) else (3, 4) in
        let x = fst r in Obj.set_field (Obj.repr r) 0 (Obj.repr 9); x + fst r
      let t4 c =
        let r = if c then [| 1; 2 |] else [| 3; 4 |] in
        let x = r.(0) in r.(0) <- 8; x + r.(0)
      let cell = ref (0, 0)
      let t5 c =
        let r = if c then (1, 2) else (3, 4) in
        cell := r;
        let x = fst r in Obj.set_field (Obj.repr !cell) 0 (Obj.repr 5); x + fst r
      let () =
        Printf.printf "%d %d %d %d %d\n" (t1 true) (t2 false) (t3 true) (t4 true) (t5 false)
    |};
  [%expect {| 6 10 10 9 8 |}]

(* Bound checks can raise, but do not mutate the tuple. *)
let%expect_test "bound checks" =
  let program =
    compile_and_parse
      ~flags:[ "--no-inline" ]
      {|
      let f (p : int * int) (a : int array) = fst p + a.(0) + snd p
      let _ = f (1, 2) [| 3 |]
    |}
  in
  print_fun_decl program (Some "f$0");
  [%expect
    {|
           function f$0(_b_, _c_, a){
            return (_b_ + runtime.caml_check_bound(a, 0)[1] | 0) + _c_ | 0;
           }
           //end
           |}]
