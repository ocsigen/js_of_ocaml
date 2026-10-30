(* Js_of_ocaml compiler
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

(** Functions which signal exceptions by returning a sentinel value
    (JavaScript backend).

    Throwing a JavaScript exception is expensive. A raising function
    stores the exception in a global and returns the sentinel value
    [caml_exn_sentinel] instead; its callers test the result. *)

type t =
  { raising_funcs : Code.Var.Set.t
        (** Functions returning the sentinel value on exception *)
  ; raising_calls : Code.Var.Set.t
        (** Variables bound to the result of a call to a raising function *)
  }

val empty : t

val f : Code.program -> Global_flow.info -> Code.program * t
(** A raising function which can be called through closures is split
    into a body, under a fresh name, and a wrapper with the original
    name, which calls the body and throws the exception. Direct calls
    using the original name of the function are redirected to the
    body. *)
