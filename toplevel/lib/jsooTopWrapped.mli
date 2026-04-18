(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** A [result] type for all the toplevel functions. *)
type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

and error =
  { msg : string
  ; locs : loc list
  }

and warning = error

and loc =
  { loc_start : int * int
  ; loc_end : int * int
  }

val initialize : unit -> unit
(** Install the custom warning reporter and apply the [Includemod]/Marshal
    workaround. Idempotent; must be called before any of the functions
    below are used. *)

include
  JsooTopIntf.Wrapped
    with type toplevel := unit
     and type 'a result := 'a result
     and type output := Format.formatter

val error_of_exn : exn -> error
