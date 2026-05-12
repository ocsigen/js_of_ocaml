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

(** Conversions between {!Js_of_ocaml.Promise} and {!Lwt}. *)

open Js_of_ocaml

exception Rejected of Promise.error
(** Raised by the [Lwt.t] returned by {!to_lwt} when the underlying
    [Promise] rejects. The rejection reason is wrapped unchanged — even
    when it is an OCaml exception (e.g. produced via
    {!Promise.error_of_exn}), use {!Promise.error_to_any} to inspect it. *)

val to_lwt : 'a Promise.t -> 'a Lwt.t
(** [to_lwt p] is an Lwt thread that fulfils with the value [p] resolves
    with, or fails with {!Rejected} if [p] is rejected. *)

val of_lwt : 'a Lwt.t -> 'a Promise.t
(** [of_lwt t] is a [Promise] that resolves with the value [t] returns,
    or rejects with [Promise.error_of_exn exn] if [t] fails. *)
