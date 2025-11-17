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
open! Js_of_ocaml_toplevel

type toplevel

type 'a result = 'a JsooTopWrapped.result Lwt.t

type output = string -> unit

val create :
     ?cmis_prefix:string
  -> ?after_init:(toplevel -> unit Lwt.t)
  -> pp_stdout:output
  -> pp_stderr:output
  -> js_file:string
  -> unit
  -> toplevel Lwt.t

val set_after_init : toplevel -> (toplevel -> unit Lwt.t) -> unit

val import_cmis_js : toplevel -> string -> unit JsooTopWrapped.result Lwt.t

val reset : toplevel -> ?timeout:(unit -> unit Lwt.t) -> unit -> unit Lwt.t

include
  JsooTopIntf.Wrapped
    with type toplevel := toplevel
     and type 'a result := 'a result
     and type output := output
