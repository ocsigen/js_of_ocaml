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

(* Conversions with [Js.string]: wasm_of_ocaml caches the conversions of
   short strings, inserting a string in the cache when it is converted
   again. Each name is converted three times below: on a cache miss, when it
   is inserted, and on a cache hit. *)

open Js_of_ocaml

let keys : Js.Unsafe.any =
  Js.Unsafe.js_expr "(o)=>Object.keys(o).map((k)=>k+'='+String(o[k])).join(' ')"

let print_obj o =
  print_endline (Js.to_string (Js.Unsafe.fun_call keys [| Js.Unsafe.inject o |]))

let names = [| "width"; "h\195\169ight"; "a_rather_long_property_name"; "id"; "" |]

let () =
  let o = Js.Unsafe.obj [||] in
  for i = 1 to 3 do
    Array.iter (fun name -> Js.Unsafe.set o (Js.string name) (Js.Unsafe.inject i)) names
  done;
  print_obj o;
  print_endline
    (String.concat
       " "
       (Array.to_list
          (Array.map
             (fun name ->
               let v : int = Js.Unsafe.get o (Js.string name) in
               Printf.sprintf "%s:%d" name v)
             names)))

(* The same bytes, mutated after having been converted: the cache must not
   return the conversion of their former contents. ["aaa"] and ["atv"] use
   the same cache entry. *)
let () =
  let o = Js.Unsafe.obj [||] in
  let b = Bytes.of_string "aaa" in
  for _ = 1 to 3 do
    Js.Unsafe.set o (Js.string (Bytes.unsafe_to_string b)) (Js.Unsafe.inject 1)
  done;
  Bytes.set b 1 't';
  Bytes.set b 2 'v';
  for _ = 1 to 3 do
    Js.Unsafe.set o (Js.string (Bytes.unsafe_to_string b)) (Js.Unsafe.inject 2)
  done;
  print_obj o

(* Method names given as OCaml strings are converted as well *)
let method_names = Array.make 1 "toFixed"

let () =
  for _ = 1 to 3 do
    print_endline
      (Js.to_string
         (Js.Unsafe.meth_call
            (Js.number_of_float 1.25)
            method_names.(0)
            [| Js.Unsafe.inject 1 |]))
  done
