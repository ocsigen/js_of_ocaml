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

(** Channel messaging: [MessageChannel], [MessagePort] and [MessageEvent].

    @see <https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel>
    @see <https://developer.mozilla.org/en-US/docs/Web/API/MessagePort>
    @see <https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent>
    @see <https://html.spec.whatwg.org/multipage/web-messaging.html> *)

open Js

(** One of the two endpoints of a {!class-type:messageChannel}. Messages posted
    on one port are delivered to the [message] listener of the other. *)
class type messagePort = object ('self)
  inherit Dom_html.eventTarget

  method postMessage : 'a. 'a -> unit meth

  method postMessage_withTransfer : 'a. 'a -> Unsafe.any js_array t -> unit meth
  (** [port##postMessage_withTransfer msg transfer] posts [msg], transferring
      ownership of the {{:https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Transferable_objects}
      transferable objects} in [transfer] (e.g. [ArrayBuffer]s or other
      {!class-type:messagePort}s) to the receiving context. *)

  method start : unit meth

  method close : unit meth

  method onmessage :
    ('self t, ('self, Unsafe.any) Dom_html.messageEvent t) Dom.event_listener
    writeonly_prop

  method onmessageerror :
    ('self t, ('self, Unsafe.any) Dom_html.messageEvent t) Dom.event_listener
    writeonly_prop

  method onclose : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

class type messageChannel = object
  method port1 : messagePort t readonly_prop

  method port2 : messagePort t readonly_prop
end

val messageChannel : messageChannel t constr

(** {2 MessageEvent constructor} *)

(** Initializer for a {!Dom_html.messageEvent}. All fields are optional; create
    an empty record with {!empty_message_event_init} and populate the ones you
    need. The [data] payload is injected as {!Js.Unsafe.any}; the resulting event
    is read back at whatever type the caller annotates {!messageEvent_with_init}
    with. *)
class type messageEventInit = object
  method data : Unsafe.any writeonly_prop

  method origin : js_string t writeonly_prop

  method lastEventId : js_string t writeonly_prop

  method source : Unsafe.any writeonly_prop

  method ports : messagePort t js_array t writeonly_prop
end

val empty_message_event_init : unit -> messageEventInit t

val messageEvent : (js_string t -> ('b, 'a) Dom_html.messageEvent t) constr

val messageEvent_with_init :
  (js_string t -> messageEventInit t -> ('b, 'a) Dom_html.messageEvent t) constr

val is_supported : unit -> bool
(** Whether the [MessageChannel] global is available in the current
    environment. *)
