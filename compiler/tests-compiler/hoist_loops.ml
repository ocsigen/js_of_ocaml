(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* End-to-end tests for the IR-level loop hoisting pass.

   The pass extracts the body of an outermost loop of the toplevel
   "unit-init" function into a helper closure. Each test compiles a small
   OCaml program and checks that:
   - the toplevel contains a call to a freshly-defined inner function (the
     loop helper),
   - the helper contains the loop itself,
   - the value computed by the loop is correctly threaded back to the
     toplevel, both for the [Apply] return value and for variables that are
     bound inside the loop body and read again after it. *)

let%expect_test "simple for-loop is extracted into a helper" =
  let prog =
    Util.compile_and_parse
      {|
      let r = ref 0
      let () =
        for i = 1 to 10 do
          r := !r + i
        done
      let () = print_int !r
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), r = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           r[1] = r[1] + i | 0;
           _a_ = i + 1 | 0;
           if(10 === i){
            caml_call1(Stdlib[44], r[1]);
            runtime.caml_register_global([0, r], "Test");
            return;
           }
           i = _a_;
          }
         }
         (1));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "two independent loops, each with its own helper" =
  let prog =
    Util.compile_and_parse
      {|
      let s = ref 0 and t = ref 0
      let () =
        for i = 1 to 5 do s := !s + i done;
        for j = 1 to 5 do t := !t + (2 * j) done
      let () = print_int (!s + !t)
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), dummy = 0, s = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           s[1] = s[1] + i | 0;
           _a_ = i + 1 | 0;
           if(5 === i) return;
           i = _a_;
          }
         }
         (1));
       var t = [0, 0];
       (function(_a_){
          var j = _a_;
          for(;;){
           t[1] = t[1] + (2 * j | 0) | 0;
           _a_ = j + 1 | 0;
           if(5 === j){
            caml_call1(Stdlib[44], s[1] + t[1] | 0);
            runtime.caml_register_global([0, s, t], "Test");
            return;
           }
           j = _a_;
          }
         }
         (1));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "loop body containing a closure: the closure moves with the body" =
  let prog =
    Util.compile_and_parse
      {|
      let acc = ref []
      let () =
        for i = 1 to 3 do
          let v = i in
          acc := (fun () -> v) :: !acc
        done
      let () =
        let rec run = function
          | [] -> ()
          | f :: t -> print_int (f ()); run t
        in
        run !acc
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var dummy = 0, Stdlib = runtime.caml_get_global("Stdlib"), acc = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           let i$0 = i;
           acc[1] = [0, function(param){return i$0;}, acc[1]];
           _a_ = i + 1 | 0;
           if(3 === i) return;
           i = _a_;
          }
         }
         (1));
       function run(_a_){
        for(;;){
         if(! _a_) return;
         var t = _a_[2], f = _a_[1], _a_ = caml_call1(f, 0);
         caml_call1(Stdlib[44], _a_);
         _a_ = t;
        }
       }
       run(acc[1]);
       runtime.caml_register_global([0, acc], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "while loop" =
  let prog =
    Util.compile_and_parse
      {|
      let r = ref 0
      let () =
        let i = ref 1 in
        while !i <= 10 do
          r := !r + !i;
          incr i
        done
      let () = print_int !r
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), r = [0, 0];
       (function(loop_init){
          var i = loop_init;
          for(;;){
           if(10 < i){
            caml_call1(Stdlib[44], r[1]);
            runtime.caml_register_global([0, r], "Test");
            return;
           }
           r[1] = r[1] + i | 0;
           var i$0 = i + 1 | 0;
           i = i$0;
          }
         }
         (1));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "loop with a Switch in the body" =
  let prog =
    Util.compile_and_parse
      {|
      let r = ref 0
      let () =
        for i = 0 to 9 do
          match i mod 4 with
          | 0 -> r := !r + 1
          | 1 -> r := !r + 10
          | 2 -> r := !r + 100
          | _ -> r := !r + 1000
        done
      let () = print_int !r
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), r = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           _a_ = i % 4 | 0;
           if(2 < _a_ >>> 0)
            r[1] = r[1] + 1000 | 0;
           else
            switch(_a_){
              case 0:
               r[1] = r[1] + 1 | 0; break;
              case 1:
               r[1] = r[1] + 10 | 0; break;
              default: r[1] = r[1] + 100 | 0;
            }
           _a_ = i + 1 | 0;
           if(9 === i){
            caml_call1(Stdlib[44], r[1]);
            runtime.caml_register_global([0, r], "Test");
            return;
           }
           i = _a_;
          }
         }
         (0));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "non-trivial live-out: value computed in loop, used after" =
  (* The post-loop region is large enough that [shrink_loops] keeps it
     outside [body_set]. The sum [n] computed in the loop body is then
     a live-out variable: the helper must return it (packed together with
     any exit-cont args), and the dispatch block must unpack and rebind
     it under its original name so the post-loop code keeps working. *)
  let prog =
    Util.compile_and_parse
      {|
      let () =
        let n = ref 0 in
        for i = 1 to 5 do n := !n + i done;
        let v = !n in
        print_string "Sum: "; print_int v; print_newline ();
        print_string "Doubled: "; print_int (2 * v); print_newline ();
        print_string "Tripled: "; print_int (3 * v); print_newline ();
        print_string "Quadrupled: "; print_int (4 * v); print_newline ()
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var
        Stdlib = runtime.caml_get_global("Stdlib"),
        n =
          function(loop_init, _a_){
             var n$0 = loop_init, i = _a_;
             for(;;){
              var n = n$0 + i | 0, _a_ = i + 1 | 0;
              if(5 === i) return [0, n];
              n$0 = n;
              i = _a_;
             }
            }
            (0, 1)
           [1];
       caml_call1(Stdlib[42], caml_string_of_jsbytes("Sum: "));
       caml_call1(Stdlib[44], n);
       caml_call1(Stdlib[47], 0);
       caml_call1(Stdlib[42], caml_string_of_jsbytes("Doubled: "));
       caml_call1(Stdlib[44], 2 * n | 0);
       caml_call1(Stdlib[47], 0);
       caml_call1(Stdlib[42], caml_string_of_jsbytes("Tripled: "));
       caml_call1(Stdlib[44], 3 * n | 0);
       caml_call1(Stdlib[47], 0);
       caml_call1(Stdlib[42], caml_string_of_jsbytes("Quadrupled: "));
       caml_call1(Stdlib[44], 4 * n | 0);
       caml_call1(Stdlib[47], 0);
       runtime.caml_register_global([0], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "post-loop closure captures a loop-bound value" =
  (* A closure defined *after* the loop captures the loop's final value.
     The closure-defining block lives outside [body_set] and outside
     [in_loop] (it's the post-loop region). [compute_uses] still must
     attribute the closure's tracked-var captures to that defining block,
     otherwise the value would not be marked live-out and the post-loop
     closure would reference an unbound variable. *)
  let prog =
    Util.compile_and_parse
      {|
      let () =
        let n = ref 0 in
        for i = 1 to 5 do n := !n + i done;
        let v = !n in
        let display () = print_int v; print_newline () in
        display (); display (); display ()
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var
        Stdlib = runtime.caml_get_global("Stdlib"),
        n =
          function(loop_init, _a_){
             var n$0 = loop_init, i = _a_;
             for(;;){
              var n = n$0 + i | 0, _a_ = i + 1 | 0;
              if(5 === i) return [0, n];
              n$0 = n;
              i = _a_;
             }
            }
            (0, 1)
           [1];
       function display(param){
        caml_call1(Stdlib[44], n);
        return caml_call1(Stdlib[47], 0);
       }
       display(0);
       display(0);
       display(0);
       runtime.caml_register_global([0], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "try-with inside loop body" =
  (* [raise_notrace] keeps the raise-kind argument to
     [caml_maybe_attach_backtrace] stable across compilers; plain [raise] of an
     immediately-caught exception is translated as a "reraise" by OCaml but as
     a regular raise by OxCaml, which makes the snapshot diverge. *)
  let prog =
    Util.compile_and_parse
      {|
      let r = ref 0
      let () =
        for i = 1 to 10 do
          try
            if i = 7 then raise_notrace Exit;
            r := !r + i
          with Exit -> r := !r + 100
        done
      let () = print_int !r
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var
        runtime = globalThis.jsoo_runtime,
        caml_maybe_attach_backtrace = runtime.caml_maybe_attach_backtrace,
        caml_wrap_exception = runtime.caml_wrap_exception;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), r = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           try{if(7 === i) throw Stdlib[3]; r[1] = r[1] + i | 0;}
           catch(exn$0){
            var exn = caml_wrap_exception(exn$0);
            if(exn !== Stdlib[3]) throw caml_maybe_attach_backtrace(exn, 0);
            r[1] = r[1] + 100 | 0;
           }
           _a_ = i + 1 | 0;
           if(10 === i){
            caml_call1(Stdlib[44], r[1]);
            runtime.caml_register_global([0, r], "Test");
            return;
           }
           i = _a_;
          }
         }
         (1));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "nested loops: only the outermost is hoisted" =
  let prog =
    Util.compile_and_parse
      {|
      let r = ref 0
      let () =
        for i = 1 to 3 do
          for j = 1 to 3 do
            r := !r + i * j
          done
        done
      let () = print_int !r
      |}
  in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib"), r = [0, 0];
       (function(_a_){
          var i = _a_;
          for(;;){
           var j = 1;
           for(;;){
            r[1] = r[1] + runtime.caml_mul(i, j) | 0;
            _a_ = j + 1 | 0;
            if(3 === j){
             _a_ = i + 1 | 0;
             if(3 !== i){i = _a_; break;}
             caml_call1(Stdlib[44], r[1]);
             runtime.caml_register_global([0, r], "Test");
             return;
            }
            j = _a_;
           }
          }
         }
         (1));
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "no loop at toplevel: early-exit path leaves code unchanged" =
  let prog = Util.compile_and_parse {|
      let () = print_int 42
      |} in
  Util.print_program prog;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) === 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var Stdlib = runtime.caml_get_global("Stdlib");
       caml_call1(Stdlib[44], 42);
       runtime.caml_register_global([0], "Test");
       return;
      }
      (globalThis));
    //end
    |}]
