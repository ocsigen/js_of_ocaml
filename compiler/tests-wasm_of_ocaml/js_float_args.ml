(* Wasm_of_ocaml compiler
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

(* Floats converted with [Js.number_of_float] and passed to JavaScript calls:
   wasm_of_ocaml passes them directly as f64 values when possible. *)

open Js_of_ocaml

let show : Js.Unsafe.any =
  Js.Unsafe.js_expr
    {|(...a)=>a.map((x)=>Object.is(x,-0)?'number:-0':typeof x+':'+String(x)).join(' ')|}

let print s = print_endline (Js.to_string s)

let show1 x : Js.js_string Js.t = Js.Unsafe.fun_call show [| Js.Unsafe.inject x |]

let[@inline never] special_values a b c d =
  print
    (Js.Unsafe.fun_call
       show
       [| Js.Unsafe.inject (Js.number_of_float a)
        ; Js.Unsafe.inject (Js.number_of_float b)
        ; Js.Unsafe.inject (Js.number_of_float c)
        ; Js.Unsafe.inject (Js.number_of_float d)
       |])

let[@inline never] meth_call a b =
  let math = Js.Unsafe.get Js.Unsafe.global "Math" in
  Js.float_of_number
    (Js.Unsafe.meth_call
       math
       "max"
       [| Js.Unsafe.inject (Js.number_of_float a)
        ; Js.Unsafe.inject (Js.number_of_float b)
       |])

let cells = Array.make 1 (Js.number_of_float 0., 0)

(* The converted value is also stored in a block: the conversion is kept *)
let[@inline never] also_stored a =
  let v = Js.number_of_float a in
  cells.(0) <- v, 1;
  print (Js.Unsafe.fun_call show [| Js.Unsafe.inject v |])

let closures = Array.make 1 (fun () -> ())

(* The converted value is used in another function *)
let[@inline never] captured a =
  let v = Js.number_of_float a in
  closures.(0) <- (fun () -> print (Js.Unsafe.fun_call show [| Js.Unsafe.inject v |]))

(* A reference updated between the conversion and the call *)
let[@inline never] assigned n =
  let r = ref 5.5 in
  for _ = 1 to n do
    try
      let v = Js.number_of_float !r in
      r := !r +. 1.;
      if n < 0 then raise Exit;
      print
        (Js.Unsafe.fun_call
           show
           [| Js.Unsafe.inject v; Js.Unsafe.inject (Js.number_of_float !r) |])
    with Exit -> Printf.printf "%g\n" !r
  done

let[@inline never] set a =
  let o = Js.Unsafe.obj [||] in
  Js.Unsafe.set o "x" (Js.number_of_float a);
  print (show1 (Js.Unsafe.get o "x"))

let[@inline never] obj a =
  let o =
    Js.Unsafe.obj
      [| "a", Js.Unsafe.inject (Js.number_of_float a)
       ; "b", Js.Unsafe.inject (Js.string "s")
      |]
  in
  print (Js.Unsafe.fun_call show [| Js.Unsafe.get o "a"; Js.Unsafe.get o "b" |])

let[@inline never] new_obj a =
  let n =
    Js.Unsafe.new_obj
      (Js.Unsafe.pure_js_expr "Number")
      [| Js.Unsafe.inject (Js.number_of_float a) |]
  in
  print (show1 (Js.Unsafe.meth_call n "valueOf" [||]))

let o : < > Js.t = Js.Unsafe.js_expr "({x: 1.25, y: -0.5, f: (v) => v * 2})"

(* Results converted with [Js.float_of_number]: the JavaScript calls return
   f64 values when possible *)
let[@inline never] results o =
  let a = Js.float_of_number (Js.Unsafe.get o "x") in
  let b = Js.float_of_number (Js.Unsafe.get o "y") in
  let e =
    Js.float_of_number
      (Js.Unsafe.meth_call o "f" [| Js.Unsafe.inject (Js.number_of_float a) |])
  in
  let f =
    Js.float_of_number
      (Js.Unsafe.fun_call
         (Js.Unsafe.get o "f")
         [| Js.Unsafe.inject (Js.number_of_float b) |])
  in
  Printf.printf "%g %g %g %g\n" a b e f

let names = Array.make 1 "x"

(* Not a constant property name *)
let[@inline never] result_generic_get o =
  Printf.printf "%g\n" (Js.float_of_number (Js.Unsafe.get o names.(0)))

let result_cells = Array.make 1 (Js.number_of_float 0., 0)

(* The result is also stored in a block: it is kept as a JavaScript value *)
let[@inline never] result_also_stored o =
  let r = Js.Unsafe.get o "x" in
  result_cells.(0) <- r, 2;
  Printf.printf "%g\n" (Js.float_of_number r)

(* The result is converted in another function *)
let[@inline never] result_captured o =
  let r = Js.Unsafe.get o "x" in
  closures.(0) <- (fun () -> Printf.printf "%g\n" (Js.float_of_number r))

let symbol : < > Js.t = Js.Unsafe.js_expr "({x: Symbol()})"

(* The result is only converted on one branch: it must not be converted on
   the other branch, where converting this symbol would raise an exception *)
let[@inline never] result_conditional flag o =
  let r = Js.Unsafe.get o "x" in
  if flag then Js.float_of_number r else 0.

let () =
  special_values Float.nan (-0.) Float.infinity 1e300;
  Printf.printf "%g\n" (meth_call 1.5 (-2.5));
  also_stored 3.25;
  Printf.printf "%g\n" (Js.float_of_number (fst cells.(0)));
  captured 4.5;
  closures.(0) ();
  assigned 2;
  set 6.5;
  obj 7.5;
  new_obj 8.5;
  results o;
  result_generic_get o;
  result_also_stored o;
  Printf.printf "%g\n" (Js.float_of_number (fst result_cells.(0)));
  result_captured o;
  closures.(0) ();
  Printf.printf "%g %g\n" (result_conditional true o) (result_conditional false symbol)
