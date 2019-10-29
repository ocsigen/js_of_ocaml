(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright Pierre Chambart 2012.
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
open! Import

type 'a t = < > Js.t

let obj = Js.Unsafe.global##._Object

let create () : 'a t = new%js obj

let add (t : 'a t) (k : Js.js_string Js.t) (v : 'a) =
  (* '_' is added to avoid conflicts with objects methods *)
  Js.Unsafe.set t (k##concat (Js.string "_")) v

let remove (t : 'a t) (k : Js.js_string Js.t) =
  Js.Unsafe.delete t (k##concat (Js.string "_"))

let find (t : 'a t) (k : Js.js_string Js.t) : 'a Js.Optdef.t =
  Js.Unsafe.get t (k##concat (Js.string "_"))

let keys (t : 'a t) : Js.js_string Js.t list =
  let key_array : Js.js_string Js.t Js.js_array Js.t =
    Js.Unsafe.global##._Object##keys t
  in
  let res = ref [] in
  for i = 0 to pred key_array##.length do
    let key =
      Js.Optdef.get (Js.array_get key_array i) (fun () -> failwith "Jstable.keys")
    in
    res := key##substring 0 (pred key##.length) :: !res
  done;
  List.rev !res
