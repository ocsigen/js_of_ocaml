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

open! Util

(* The last test uses the [Effect] stdlib module, so it needs OCaml >= 5. *)
[@@@if ocaml_version >= (5, 0, 0)]

(* With OxCaml, calls proven unable to perform an effect stay direct even in
   CPS context (see the unyielding tests). Disable this so that the output is
   the same as with mainstream OCaml. *)
let flags =
  if Js_of_ocaml_compiler.Config.oxcaml
  then [ "--disable"; "oxcaml-use-unyielding-debuginfo-for-effect-cps" ]
  else []

(* [write] has arity 1 and returns an arity-1 closure. It is bound by
   destructuring a tuple returned by [make], so the local flow analysis used
   inside the optimization loop cannot see its arity; only [Global_flow] can.
   [write x y] is thus an over-application of an arity-1 function. [make] is
   called twice to prevent inlining, and everything is local to [test] so
   that no value escapes through the module block. *)
let overapplication_code =
  {|
         let l = ref []
         let cb = ref (fun () -> ())

         let test () =
           let make n =
             l := (fun () -> ()) :: !l; (* prevent inlining *)
             ((fun x -> l := (fun () -> ()) :: !l; fun y -> x + y + n), n)
           in
           let write, dummy = make 0 in
           let write2, dummy2 = make 1 in
           let f x y = l := (fun () -> ()) :: !l; write x y in
           let g x y = l := (fun () -> ()) :: !l; write2 x y in
           !cb (); (* keep [test] CPS-translated *)
           print_int (f 1 2 + g 3 4 + dummy + dummy2); print_newline ()

         let () = test ()
         let () = test ()
|}

let%expect_test "over-application of a known-arity function / effects disabled" =
  let program = compile_and_parse ~flags ~effects:`Disabled overapplication_code in
  print_fun_decl program (Some "test");
  [%expect
    {|
    function test(_a_){
     function make(n){
      l[1] = [0, function(param){return 0;}, l[1]];
      return [0,
              function(x){
               l[1] = [0, function(param){return 0;}, l[1]];
               return function(y){return (x + y | 0) + n | 0;};
              },
              n];
     }
     var
      _a_ = make(0),
      dummy = _a_[2],
      write = _a_[1],
      _a_ = make(1),
      dummy2 = _a_[2],
      write2 = _a_[1];
     caml_call1(cb[1], 0);
     l[1] = [0, function(param){return 0;}, l[1]];
     _a_ = caml_call1(write2(3), 4);
     l[1] = [0, function(param){return 0;}, l[1]];
     _a_ = ((caml_call1(write(1), 2) + _a_ | 0) + dummy | 0) + dummy2 | 0;
     caml_call1(Stdlib[44], _a_);
     return caml_call1(Stdlib[47], 0);
    }
    //end
    |}]

let%expect_test "over-application of a known-arity function / double translation" =
  (* The direct-style version must call [write] exactly ([write(x)]) instead
     of going through [caml_call_gen] with both arguments at once. The
     call to [!cb] keeps [test] CPS-translated, so that both versions are
     emitted. *)
  let program =
    compile_and_parse ~flags ~effects:`Double_translation overapplication_code
  in
  print_double_fun_decl program "test";
  [%expect
    {|
    function test$0(_d_){
     var
      make$0 = make(),
      _d_ = make$0(0),
      dummy = _d_[2],
      write = _d_[1],
      _d_ = make$0(1),
      dummy2 = _d_[2],
      write2 = _d_[1];
     caml_call1(cb[1], 0);
     _d_ = l[1];
     l[1] = [0, _a_(), _d_];
     _d_ = caml_call1(write2(3), 4);
     var _e_ = l[1];
     l[1] = [0, _b_(), _e_];
     _d_ = ((caml_call1(write(1), 2) + _d_ | 0) + dummy | 0) + dummy2 | 0;
     caml_call1(Stdlib[44], _d_);
     return caml_call1(Stdlib[47], 0);
    }
    //end
    function test$1(_c_, cont){
     var
      make$0 = make(),
      _c_ = make$0(0),
      dummy = _c_[2],
      write = _c_[1],
      _c_ = make$0(1),
      dummy2 = _c_[2],
      write2 = _c_[1];
     return caml_trampoline_cps_call2
             (cb[1],
              0,
              function(_c_){
               _c_ = l[1];
               l[1] = [0, _a_(), _c_];
               _c_ = write2(3)(4);
               var _d_ = l[1];
               l[1] = [0, _b_(), _d_];
               _c_ = ((write(1)(2) + _c_ | 0) + dummy | 0) + dummy2 | 0;
               return caml_trampoline_cps_call2
                       (Stdlib[44],
                        _c_,
                        function(_d_){
                         return caml_trampoline_cps_call2(Stdlib[47], 0, cont);
                        });
              });
    }
    //end
    var test = runtime.caml_cps_closure(test$0, test$1);
    //end
    |}]

let%expect_test "over-application of a known-arity function / cps" =
  let program = compile_and_parse ~flags ~effects:`Cps overapplication_code in
  print_fun_decl program (Some "test");
  [%expect
    {|
    function test(_a_, cont){
     function make(n){
      l[1] = [0, function(param, cont){return cont(0);}, l[1]];
      return [0,
              function(x){
               l[1] = [0, function(param, cont){return cont(0);}, l[1]];
               return function(y){return (x + y | 0) + n | 0;};
              },
              n];
     }
     var
      _a_ = make(0),
      dummy = _a_[2],
      write = _a_[1],
      _a_ = make(1),
      dummy2 = _a_[2],
      write2 = _a_[1];
     return caml_trampoline_cps_call2
             (cb[1],
              0,
              function(_a_){
               l[1] = [0, function(param, cont){return cont(0);}, l[1]];
               _a_ = write2(3)(4);
               l[1] = [0, function(param, cont){return cont(0);}, l[1]];
               _a_ = ((write(1)(2) + _a_ | 0) + dummy | 0) + dummy2 | 0;
               return caml_trampoline_cps_call2
                       (Stdlib[44],
                        _a_,
                        function(_a_){
                         return caml_trampoline_cps_call2(Stdlib[47], 0, cont);
                        });
              });
    }
    //end
    |}]

let%expect_test "over-application executes correctly in all effects modes" =
  (* The outer closure prints before returning the inner closure, and the
     inner closure prints when applied, so this also checks that splitting
     the application in two preserves the evaluation order. *)
  let code =
    {|
         let l = ref []

         let test () =
           let make n =
             l := (fun () -> ()) :: !l; (* prevent inlining *)
             ((fun x ->
                 l := (fun () -> ()) :: !l; (* prevent inlining *)
                 Printf.printf "outer(%d) " x;
                 fun y -> Printf.printf "inner(%d) " y; x + y + n),
              n)
           in
           let write, dummy = make 0 in
           let f x y = l := (fun () -> ()) :: !l; write x y in
           let _ = make 1 in
           print_int (f 1 2 + dummy); print_newline ()

         let () = test ()
|}
  in
  compile_and_run ~flags ~effects:`Disabled code;
  [%expect {| outer(1) inner(2) 3 |}];
  compile_and_run ~flags ~effects:`Cps code;
  [%expect {| outer(1) inner(2) 3 |}];
  compile_and_run ~flags ~effects:`Double_translation code;
  [%expect {| outer(1) inner(2) 3 |}]

let%expect_test "over-application whose first application performs an effect" =
  (* The first half of the split application performs an effect: splitting
     must keep both halves CPS-translated so that the effect reaches the
     handler and the continuation resumes with the second application. *)
  let code =
    {|
         [@@@alert "-unsafe_effects"] (* OxCaml warns about [Effect.perform] *)
         open Effect
         open Effect.Deep

         type _ Effect.t += E : int Effect.t

         let l = ref []

         let test () =
           let make n =
             l := (fun () -> ()) :: !l; (* prevent inlining *)
             ((fun x ->
                 l := (fun () -> ()) :: !l; (* prevent inlining *)
                 let e = perform E in
                 fun y -> x + y + e + n),
              n)
           in
           let write, dummy = make 0 in
           let f x y = l := (fun () -> ()) :: !l; write x y in
           let _ = make 1 in
           f 1 2 + dummy

         let () =
           let r =
             try_with test ()
               { effc = (fun (type a) (eff : a Effect.t) ->
                   match eff with
                   | E -> Some (fun (k : (a, _) continuation) -> continue k 10)
                   | _ -> None) }
           in
           print_int r; print_newline ()
|}
  in
  compile_and_run ~flags ~effects:`Cps code;
  [%expect {| 13 |}];
  compile_and_run ~flags ~effects:`Double_translation code;
  [%expect {| 13 |}]
