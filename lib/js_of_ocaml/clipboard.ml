(* Js_of_ocaml library
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
open! Import

class type clipboardItem = object
  method types : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

  method presentationStyle : Js.js_string Js.t Js.readonly_prop

  method getType : Js.js_string Js.t -> File.blob Js.t Promise.t Js.meth
end

let clipboardItem_global = Js.Unsafe.global##._ClipboardItem

let clipboardItem : (Js.Unsafe.any -> clipboardItem Js.t) Js.constr = clipboardItem_global

let item_supports (mime : Js.js_string Js.t) : bool Js.t =
  clipboardItem_global##supports mime

class type clipboard = object
  method read : clipboardItem Js.t Js.js_array Js.t Promise.t Js.meth

  method readText : Js.js_string Js.t Promise.t Js.meth

  method write : clipboardItem Js.t Js.js_array Js.t -> unit Promise.t Js.meth

  method writeText : Js.js_string Js.t -> unit Promise.t Js.meth
end

let navigator = Js.Unsafe.global##.navigator

let clipboard () : clipboard Js.t = navigator##.clipboard

let is_supported () = Js.Optdef.test navigator && Js.Optdef.test navigator##.clipboard
