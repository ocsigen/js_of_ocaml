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

(** CSS Font Loading API binding.

    This is a partial binding to the CSS Font Loading API.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/CSS_Font_Loading_API>
    @see <https://www.w3.org/TR/css-font-loading-3/> *)

open Js

class type fontFace = object
  method family : js_string t prop

  method style : js_string t prop

  method weight : js_string t prop

  method stretch : js_string t prop

  method unicodeRange : js_string t prop

  method featureSettings : js_string t prop

  method variationSettings : js_string t prop

  method display : js_string t prop

  method status : js_string t readonly_prop

  method load : fontFace t Promise.t meth
end

class type fontFaceSet = object
  method add : fontFace Js.t -> fontFaceSet Js.t meth

  method check : js_string t -> js_string t -> bool t meth

  method clear : unit meth

  method delete : fontFace Js.t -> bool t meth

  method has : fontFace Js.t -> bool t meth

  method load : js_string t -> js_string t -> fontFace t js_array t Promise.t meth

  method ready : fontFaceSet t Promise.t readonly_prop

  method status : js_string t readonly_prop
end

val is_supported : unit -> bool
(** [is_supported ()] is [true] if the [FontFace] constructor of the CSS Font
    Loading API is available in the current environment. *)

val create : js_string t -> js_string t -> fontFace t
(** [create family source] wraps [new FontFace(family, source)], where [source]
    is a CSS [src] descriptor string (e.g. ["url(font.woff2)"]). *)

val create_from_buffer : js_string t -> Typed_array.arrayBuffer t -> fontFace t
(** [create_from_buffer family data] wraps [new FontFace(family, data)] with the
    font's binary data as [data], for fonts loaded from raw bytes. *)
