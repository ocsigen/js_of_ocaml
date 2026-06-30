(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2016 OCamlPro
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
open! Js_of_ocaml_toplevel

type toplevel

type 'a result = 'a Wrapped.result Lwt.t

type output = string -> unit

type lexbuf
(** Handle for a worker-side parsing session created by {!open_lexbuf} and
    stepped one phrase at a time with {!step}. *)

exception Init_failed of Wrapped.error
(** Raised (as the result of the {!create} thread) when the worker fails to
    initialize its toplevel environment — for instance when the worker
    script cannot be loaded or [stdlib.cmis.js] is missing under the
    [cmis_base_url]. *)

val create :
     ?cmis_base_url:string
  -> ?after_init:(toplevel -> unit Lwt.t)
  -> pp_stdout:output
  -> pp_stderr:output
  -> js_file:string
  -> unit
  -> toplevel Lwt.t
(** Spawn the worker and initialize its toplevel. The returned thread fails
    with {!Init_failed} if initialization does not succeed. *)

val set_after_init : toplevel -> (toplevel -> unit Lwt.t) -> unit

val import_cmis_js : toplevel -> string -> unit Wrapped.result Lwt.t

val reset : toplevel -> ?timeout:(unit -> unit Lwt.t) -> unit -> unit Lwt.t

val clear_check : toplevel -> unit Lwt.t
(** Discard the scratch typing environment left by [check ~setenv:true] on the
    worker, re-enabling {!execute} and the other code-running operations. See
    {!Js_of_ocaml_toplevel.Wrapped.clear_check}. *)

val open_lexbuf : toplevel -> ?ppf_code:output -> string -> lexbuf Lwt.t
(** Open a worker-side parsing session over [code] to be stepped with {!step}.
    When [ppf_code] is given, the source is echoed to it as it is consumed.
    Release it with {!close_lexbuf}. *)

val step :
     toplevel
  -> ?print_outcome:bool
  -> ppf_answer:output
  -> lexbuf
  -> [ `Phrase of bool | `Eof ] result
(** Evaluate the next phrase of a session. [`Phrase ok] ran a phrase ([ok] is
    [false] if it raised at evaluation), [`Eof] once the buffer is exhausted,
    and an [Error] result on a parse/type error. The caller drives the loop and
    decides whether to continue after a failure. *)

val close_lexbuf : toplevel -> lexbuf -> unit Lwt.t
(** Release a session opened with {!open_lexbuf}. *)

include
  Wrapped_intf.Wrapped
    with type toplevel := toplevel
     and type 'a result := 'a result
     and type output := output
