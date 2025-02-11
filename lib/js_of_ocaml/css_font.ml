(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 * Copyright (C) 2014 Jérôme Vouillon
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

(** CSS Font Loading API binding

This is a partial binding to the CSS Font Loading API
*)

open Js

class type fontFaceElement = object
  method family : js_string t readonly_prop

  method style : js_string t readonly_prop

  method weight : js_string t readonly_prop

  method stretch : js_string t readonly_prop

  method unicodeRange : js_string t readonly_prop

  method variant : js_string t readonly_prop

  method featureSettings : js_string t readonly_prop

  method display : js_string t readonly_prop

  method src : js_string t readonly_prop

  method status : js_string t readonly_prop

  method load : unit -> 'a meth
end

class type fontFaceSet = object
  method add : fontFaceElement Js.t -> unit meth

  method check : js_string t -> js_string t -> bool meth

  method delete : fontFaceElement Js.t -> unit meth

  method load : unit -> 'a meth

  method ready : bool readonly_prop

  method status : js_string t readonly_prop
end

let constr : (js_string t -> js_string t -> fontFaceElement t) Js.constr =
  Unsafe.pure_js_expr "FontFace"

let create_font_face (family : js_string Js.t) (source : js_string Js.t) =
  new%js constr family source

let load_and_then (font_face : fontFaceElement Js.t) (f : unit -> unit) =
  let f = Unsafe.inject @@ Js.wrap_meth_callback f in
  let js_promise = font_face##load () in
  Unsafe.meth_call js_promise "then" [| Unsafe.coerce f |]
