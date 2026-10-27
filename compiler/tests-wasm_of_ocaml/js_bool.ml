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

(* Booleans converted with [Js.bool] are JavaScript booleans *)

open Js_of_ocaml

let show : Js.Unsafe.any =
  Js.Unsafe.js_expr "(...a)=>a.map((x)=>typeof x+':'+String(x)).join(' ')"

let print s = print_endline (Js.to_string s)

let values = [| 1; 5 |]

let[@inline never] f x =
  let b = x > 3 in
  print
    (Js.Unsafe.fun_call
       show
       [| Js.Unsafe.inject (Js.bool b)
        ; Js.Unsafe.inject (Js.bool (not b))
        ; Js.Unsafe.inject (Js.bool true)
        ; Js.Unsafe.inject (Js.bool false)
       |]);
  let o = Js.Unsafe.obj [||] in
  Js.Unsafe.set o "b" (Js.bool b);
  print (Js.Unsafe.fun_call show [| Js.Unsafe.get o "b" |]);
  Printf.printf "%b %b\n" (Js.to_bool (Js.bool b)) (Js.to_bool (Js.Unsafe.get o "b"))

let () = Array.iter f values
