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

(** Broadcast Channel API.

    A [BroadcastChannel] delivers every posted message to all other channels
    with the same name in the same origin (other tabs, windows, workers).

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Broadcast_Channel_API>
    @see <https://html.spec.whatwg.org/multipage/web-messaging.html#broadcasting-to-other-browsing-contexts> *)

open Js

class type broadcastChannel = object ('self)
  inherit Dom_html.eventTarget

  method name : js_string t readonly_prop
  (** The channel name given to the constructor. *)

  method postMessage : 'a. 'a -> unit meth
  (** [chan##postMessage msg] delivers a structured clone of [msg] to every
      other channel with the same name in the same origin. Raises once the
      channel has been closed. *)

  method close : unit meth

  method onmessage :
    ('self t, ('self, Unsafe.any) Dom_html.messageEvent t) Dom.event_listener
    writeonly_prop

  method onmessageerror :
    ('self t, ('self, Unsafe.any) Dom_html.messageEvent t) Dom.event_listener
    writeonly_prop
end

val broadcastChannel : (js_string t -> broadcastChannel t) constr
(** [new%js broadcastChannel name] joins the broadcast channel [name]. *)

val is_supported : unit -> bool
(** Whether the [BroadcastChannel] global is available in the current
    environment. *)
