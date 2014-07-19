(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
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

(** This module provides helpers to perform JSONP calls *)

(** [call ~timeout ~param url] do a jsonp call using [url].
  It uses the named query parameter [param] (default "callback") to pass the name of the callback.
  If a timeout is given and there are no answer before [timeout] seconds, the lwt thread will be cancelled.
*)
val call : ?timeout:float -> ?param:string -> string -> 'b Lwt.t

(** [call_custom_url ~timeout make_url].
    Same as [call] but let you build your own url given a callback name *)
val call_custom_url : ?timeout:float -> (string -> string) -> 'b Lwt.t
