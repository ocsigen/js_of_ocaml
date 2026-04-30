(* Js_of_ocaml tests
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

(* Regression test for the [x = e + x] -> [x += e] peephole in
   [js_traverse]: it must not fire on string [+], which is not
   commutative. *)
let prog =
  {|
external ( ^ ) : string -> string -> string = "caml_string_concat"

let f cond () =
  let a = "@" in
  let b = a ^ "=" ^ a in
  if cond then b else b ^ "2"

let () = print_endline (f true ())
let () = print_endline (f false ())|}

let%expect_test "inspect" =
  let program = Util.compile_and_parse ~debug:false ~use_js_string:true prog in
  Util.print_program program;
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
       function _a_(_c_, _b_){
        _b_ = "@";
        _b_ = _b_ + "=" + _b_;
        return _c_ ? _b_ : _b_ + "2";
       }
       var _b_ = _a_(1, 0);
       caml_call1(Stdlib[46], _b_);
       _b_ = _a_(0, 0);
       caml_call1(Stdlib[46], _b_);
       runtime.caml_register_global([0, _a_], "Test");
       return;
      }
      (globalThis));
    //end
    |}]

let%expect_test "x = a + \"=\" + a is mis-rewritten by the [x = e + x] peephole" =
  Util.compile_and_run ~debug:false ~use_js_string:true prog;
  [%expect {|
    @=@
    @=@2
    |}]
