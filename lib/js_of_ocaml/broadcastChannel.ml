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

class type broadcastChannel = object ('self)
  inherit Dom_html.eventTarget

  method name : Js.js_string Js.t Js.readonly_prop

  method postMessage : 'a. 'a -> unit Js.meth

  method close : unit Js.meth

  method onmessage :
    ('self Js.t, ('self, Js.Unsafe.any) Dom_html.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop

  method onmessageerror :
    ('self Js.t, ('self, Js.Unsafe.any) Dom_html.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop
end

let broadcastChannel : (Js.js_string Js.t -> broadcastChannel Js.t) Js.constr =
  Js.Unsafe.global##._BroadcastChannel

let is_supported () = Js.Optdef.test Js.Unsafe.global##._BroadcastChannel
