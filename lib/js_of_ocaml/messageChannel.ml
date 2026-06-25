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

open! Import

class type ['a] messageEvent = object
  inherit Dom_html.event

  method data : 'a Js.readonly_prop

  method origin : Js.js_string Js.t Js.readonly_prop

  method lastEventId : Js.js_string Js.t Js.readonly_prop

  method source : Js.Unsafe.any Js.opt Js.readonly_prop

  method ports : messagePort Js.t Js.js_array Js.t Js.readonly_prop
end

and messagePort = object ('self)
  inherit Dom_html.eventTarget

  method postMessage : 'a. 'a -> unit Js.meth

  method postMessage_withTransfer :
    'a. 'a -> Js.Unsafe.any Js.js_array Js.t -> unit Js.meth

  method start : unit Js.meth

  method close : unit Js.meth

  method onmessage :
    ('self Js.t, Js.Unsafe.any messageEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onmessageerror :
    ('self Js.t, Js.Unsafe.any messageEvent Js.t) Dom.event_listener Js.writeonly_prop
end

class type messageChannel = object
  method port1 : messagePort Js.t Js.readonly_prop

  method port2 : messagePort Js.t Js.readonly_prop
end

let messageChannel : messageChannel Js.t Js.constr = Js.Unsafe.global##._MessageChannel

class type messageEventInit = object
  method data : Js.Unsafe.any Js.writeonly_prop

  method origin : Js.js_string Js.t Js.writeonly_prop

  method lastEventId : Js.js_string Js.t Js.writeonly_prop

  method source : Js.Unsafe.any Js.writeonly_prop

  method ports : messagePort Js.t Js.js_array Js.t Js.writeonly_prop
end

let empty_message_event_init () : messageEventInit Js.t = Js.Unsafe.obj [||]

let messageEvent : (Js.js_string Js.t -> 'a messageEvent Js.t) Js.constr =
  Js.Unsafe.global##._MessageEvent

let messageEvent_with_init :
    (Js.js_string Js.t -> messageEventInit Js.t -> 'a messageEvent Js.t) Js.constr =
  Js.Unsafe.global##._MessageEvent

let is_supported () = Js.Optdef.test Js.Unsafe.global##._MessageChannel
