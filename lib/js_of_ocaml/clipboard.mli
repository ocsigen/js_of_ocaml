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

(** Clipboard API.

    A code example:
    {@ocaml[
      if Clipboard.is_supported ()
      then
        ignore
          (Promise.map
             (fun () -> Console.console##log (Js.string "copied"))
             ((Clipboard.clipboard ())##writeText (Js.string "Hello!")))
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API>
    @see <https://w3c.github.io/clipboard-apis/> *)

open Js

class type clipboardItem = object
  method types : js_string t js_array t readonly_prop

  method presentationStyle : js_string t readonly_prop

  method getType : js_string t -> File.blob t Promise.t meth
end

val clipboardItem : (Unsafe.any -> clipboardItem t) constr
(** [new%js clipboardItem data] builds a clipboard item. [data] is a plain
    JavaScript object whose keys are MIME types and whose values are the
    associated {!File.blob}, string, or a promise resolving to either. *)

val item_supports : js_string t -> bool t
(** [item_supports mime] reports whether the given MIME type is supported by the
    clipboard. Bound to the static [ClipboardItem.supports] method. *)

class type clipboard = object
  method read : clipboardItem t js_array t Promise.t meth

  method readText : js_string t Promise.t meth

  method write : clipboardItem t js_array t -> unit Promise.t meth

  method writeText : js_string t -> unit Promise.t meth
end

val clipboard : unit -> clipboard t
(** The [navigator.clipboard] object. *)

val is_supported : unit -> bool
(** Whether [navigator.clipboard] is available in the current environment. *)
