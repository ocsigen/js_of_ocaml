(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_obrowser
 * Copyright (C) 2009 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** This module corresponds to Lwt_unix but in an O'Browser environment. *)

(** {6 Sleeping} *)

val sleep : float -> unit Lwt.t
  (** [sleep d] is a threads which remain suspended for [d] seconds
      and then terminates. *)

val yield : unit -> unit Lwt.t
  (** [yield ()] is a threads which suspends itself and then resumes
      as soon as possible and terminates.
      Unlike [Lwt_preemtive.yield],
      that version will also let the browser work if it has something to
      do (for example redraw the page).
  *)

val run : 'a Lwt.t -> 'a

(*
exception Timeout
  (** Exception raised by timeout operations *)

val timeout : float -> 'a Lwt.t
  (** [timeout d] is a threads which remain suspended for [d] seconds
      then fail with {!Timeout} *)

val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [with_timeout d f] is a short-hand for:

      {[
        Lwt.select [Lwt_unix.timeout d; f ()]
      ]}
  *)
*)

(** [http_get url args] sends an HTTP GET request to the server with GET
  * arguments [args] nicely encoded and return
  * [(code, message)] where [code] is the HTTP code and [message] the content of
  * the answer. *)
val http_get : string -> (string * string) list -> (int * string) Lwt.t

(** [http_post url args] sends an HTTP POST request to the server with POST
  * arguments [args] nicely encoded and return
  * [(code, message)] where [code] is the HTTP code and [message] the content of
  * the answer. *)
val http_post : string -> (string * string) list -> (int * string) Lwt.t

(** [http_get_post url get_args post_args] makes an HTTP POST request with
  * [get_args] encoded and appended to [url] and [post_args]Â as POST arguments.
  * It's result also is [(code,message)] *)
val http_get_post : 
  string -> 
  (string * string) list -> 
  (string * string) list -> 
  (int * string) Lwt.t

(** Register a Lwt thread on a DOM event. *)
val register_event : JSOO.obj -> string -> ('a -> unit Lwt.t) -> 'a -> unit
