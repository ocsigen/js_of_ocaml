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

(** EventSource binding *)

open Js
open Dom

type state =
  | CONNECTING
  | OPEN
  | CLOSED

type 'a messageEvent = ('a, js_string t) Dom_html.messageEvent
[@@ocaml.deprecated "[since 6.5] Use Dom_html.messageEvent instead."]
(** The type parameter ['a] is the type of the event target; the [data]
    payload of an [EventSource] message is always [js_string t].

    @deprecated Use {!Dom_html.messageEvent}, the single shared binding. *)

class type eventSource = object ('self)
  method url : js_string t readonly_prop

  method withCredentials : bool t readonly_prop

  method readyState : state readonly_prop

  method close : unit meth

  method onopen : ('self t, 'self Dom.event t) event_listener writeonly_prop

  method onmessage :
    ('self t, ('self, js_string t) Dom_html.messageEvent t) event_listener writeonly_prop

  method onerror : ('self t, 'self Dom.event t) event_listener writeonly_prop
end

class type options = object
  method withCredentials : bool t writeonly_prop
end

val withCredentials : bool -> options t

val eventSource : (js_string t -> eventSource t) constr

val eventSource_options : (js_string t -> options t -> eventSource t) constr

val addEventListener :
     (#eventSource t as 'a)
  -> 'b Event.typ
  -> ('a, 'b) event_listener
  -> bool t
  -> event_listener_id
(** Add an event listener.  This function matches the
      [addEventListener] DOM method, except that it returns
      an id for removing the listener. *)
