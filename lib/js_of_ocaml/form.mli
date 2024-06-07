(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Pierre Chambart
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Js
(** This module provides functions to manipulate forms. *)

class type formData = object
  method append : js_string t -> js_string t -> unit meth

  method append_blob : js_string t -> File.blob t -> unit meth
end

val formData : formData t constr

val formData_form : (Dom_html.formElement t -> formData t) constr

(* be careful, this might not be implemented in all browser.
   To check availability, use [Js.Optdef.to_option (Js.def formData)] *)

type form_elt =
  [ `String of js_string t
  | `File of File.file t
  ]

type form_contents =
  [ `Fields of (string * form_elt) list ref
  | `FormData of formData t
  ]

val append : form_contents -> string * form_elt -> unit

val post_form_contents : Dom_html.formElement t -> form_contents

val get_form_contents : Dom_html.formElement t -> (string * string) list

val empty_form_contents : unit -> form_contents

val form_elements : ?get:bool -> Dom_html.formElement t -> (string * form_elt) list
