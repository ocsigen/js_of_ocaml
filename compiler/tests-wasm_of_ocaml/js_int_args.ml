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

(* Integers passed to JavaScript calls: wasm_of_ocaml passes them directly
   as i32 values when their type is known. *)

open Js_of_ocaml

let show : Js.Unsafe.any =
  Js.Unsafe.js_expr "(...a)=>a.map((x)=>typeof x+':'+String(x)).join(' ')"

let print s = print_endline (Js.to_string s)

let values = [| 1 lsl 29; -12345; 0 |]

let[@inline never] fun_call a b c =
  print
    (Js.Unsafe.fun_call
       show
       [| Js.Unsafe.inject a
        ; Js.Unsafe.inject b
        ; Js.Unsafe.inject c
        ; Js.Unsafe.inject (a + b)
        ; Js.Unsafe.inject (-a)
        ; Js.Unsafe.inject 42
       |])

let[@inline never] meth_call a =
  let math = Js.Unsafe.get Js.Unsafe.global "Math" in
  let r : int =
    Js.Unsafe.meth_call math "max" [| Js.Unsafe.inject a; Js.Unsafe.inject 7 |]
  in
  Printf.printf "%d\n" r

let[@inline never] set a =
  let o = Js.Unsafe.obj [||] in
  Js.Unsafe.set o "x" a;
  print (Js.Unsafe.fun_call show [| Js.Unsafe.get o "x" |])

let[@inline never] new_obj a =
  let n = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "Number") [| Js.Unsafe.inject a |] in
  print (Js.Unsafe.fun_call show [| Js.Unsafe.meth_call n "valueOf" [||] |])

let cells = Array.make 1 (0, 0)

(* The integer is also stored in a block *)
let[@inline never] also_stored a =
  cells.(0) <- a, 1;
  print (Js.Unsafe.fun_call show [| Js.Unsafe.inject a |]);
  Printf.printf "%d\n" (fst cells.(0))

let () =
  fun_call values.(0) values.(1) values.(2);
  meth_call values.(1);
  set values.(0);
  new_obj values.(1);
  also_stored values.(1)
