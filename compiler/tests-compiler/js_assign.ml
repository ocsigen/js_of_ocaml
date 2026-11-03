(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

(* Variable and label naming (Js_assign) *)

open Js_of_ocaml_compiler

let process ~shortvar ?(rename = true) ?(f = Fun.id) js_prog =
  Config.Flag.set "shortvar" shortvar;
  let lex = Parse_js.Lexer.of_string js_prog in
  let p = Parse_js.parse `Script lex in
  let p = if rename then (new Js_traverse.rename_variable ~esm:false)#program p else p in
  let p = f p in
  let p = Js_assign.program p in
  let buffer = Buffer.create 256 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let (_ : Source_map.info) = Js_output.program pp p in
  print_string (Buffer.contents buffer)

let%expect_test "label inside a default parameter" =
  process
    ~shortvar:true
    {|
function f(g = function () { lbl: for (;;) { break lbl; } },
           h = () => { lbl: for (;;) { continue lbl; } }) {
  return [g, h];
}
|};
  [%expect
    {|
           function f
           (a = function(){a: for(;;){break a;}}, b = ()=>{a: for(;;){continue a;}}){return [a, b];
           }
           |}]

let%expect_test "var in a class static block (names preserved)" =
  process
    ~shortvar:false
    {|
function f(y) {
  class C { static { var x = y + 1; console.log(x); } }
  return C;
}
|};
  [%expect
    {| function f(y){class C{static {var x = y + 1; console.log(x);}} return C;} |}]

(* Turn the source labels [names] into generated labels [L _] *)
let relabel names p =
  let open Javascript in
  let table =
    List.map
      (fun n -> Label.of_string (Stdlib.Utf8_string.of_string_exn n), Label.fresh ())
      names
  in
  let subst l =
    match List.find_opt (fun (l', _) -> Label.equal l l') table with
    | Some (_, l') -> l'
    | None -> l
  in
  let m =
    object
      inherit Js_traverse.map as super

      method! statement s =
        match super#statement s with
        | Labelled_statement (l, s) -> Labelled_statement (subst l, s)
        | Break_statement (Some l) -> Break_statement (Some (subst l))
        | Continue_statement (Some l) -> Continue_statement (Some (subst l))
        | s -> s
    end
  in
  m#program p

let%expect_test "generated labels mixed with source labels" =
  process
    ~shortvar:false
    ~rename:false
    ~f:(relabel [ "outer"; "inner" ])
    {|
outer: for (;;) { a: for (;;) { if (x()) break outer; break a; } }
a: for (;;) { inner: for (;;) { if (x()) break a; break inner; } }
|};
  [%expect
    {|
           b:
           for(;;){a: for(;;){if(x()) break b; break a;}}
           a:
           for(;;){b: for(;;){if(x()) break a; break b;}}
           |}]

(* Name [a] clashes with the free variable [a] in [f] and [g], but remains
   available in [h]. [x] occurs in several functions and is not named as a
   parameter (its position is taken by [a]), so it skips the first names. *)
let%expect_test "short names clashing with a free variable" =
  process
    ~shortvar:true
    {|
function f(x) {
  function g() { return a + x; }
  function h() { var z = x(x, x, x); return z; }
  return [g, h];
}
|};
  [%expect
    {|
    function f(g){
     function b(){return a + g;}
     function c(){var a = g(g, g, g); return a;}
     return [b, c];
    }
    |}]

(* Labels are not visible across function and class static block boundaries:
   the inner [break]s (invalid JavaScript) keep their source label. *)
let%expect_test "break to a label of an enclosing function" =
  process
    ~shortvar:true
    {|
lbl: for (;;) {
  (function () { break lbl; })();
  (() => { break lbl; })();
  class C { static { break lbl; } }
  break lbl;
}
|};
  [%expect
    {|
           a:
           for(;;){
            (function(){break lbl;}());
            (()=>{break lbl;})();
            class a{static {break lbl;}}
            break a;
           }
           |}]

let%expect_test "generated labels inside a class static block" =
  process
    ~shortvar:true
    ~rename:false
    ~f:(relabel [ "outer"; "inner" ])
    {|
outer: for (;;) {
  class C { static { inner: for (;;) { break inner; } } }
  break outer;
}
|};
  [%expect {| a: for(;;){class C{static {a: for(;;){break a;}}} break a;} |}]
