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

class type notificationAction = object
  method action : Js.js_string Js.t Js.prop

  method title : Js.js_string Js.t Js.prop

  method icon : Js.js_string Js.t Js.prop
end

class type notificationOptions = object
  method dir : Js.js_string Js.t Js.writeonly_prop

  method lang : Js.js_string Js.t Js.writeonly_prop

  method body : Js.js_string Js.t Js.writeonly_prop

  method navigate : Js.js_string Js.t Js.writeonly_prop

  method tag : Js.js_string Js.t Js.writeonly_prop

  method icon : Js.js_string Js.t Js.writeonly_prop

  method image : Js.js_string Js.t Js.writeonly_prop

  method badge : Js.js_string Js.t Js.writeonly_prop

  method vibrate : int Js.js_array Js.t Js.writeonly_prop

  method data : Js.Unsafe.any Js.writeonly_prop

  method requireInteraction : bool Js.t Js.writeonly_prop

  method silent : bool Js.t Js.opt Js.writeonly_prop

  method renotify : bool Js.t Js.writeonly_prop

  method timestamp : Js.number_t Js.writeonly_prop

  method actions : notificationAction Js.t Js.js_array Js.t Js.writeonly_prop
end

let empty_options () : notificationOptions Js.t = Js.Unsafe.obj [||]

class type notification = object ('self)
  method title : Js.js_string Js.t Js.readonly_prop

  method dir : Js.js_string Js.t Js.readonly_prop

  method lang : Js.js_string Js.t Js.readonly_prop

  method body : Js.js_string Js.t Js.readonly_prop

  method navigate : Js.js_string Js.t Js.readonly_prop

  method tag : Js.js_string Js.t Js.readonly_prop

  method icon : Js.js_string Js.t Js.readonly_prop

  method image : Js.js_string Js.t Js.readonly_prop

  method badge : Js.js_string Js.t Js.readonly_prop

  method vibrate : int Js.js_array Js.t Js.readonly_prop

  method data : Js.Unsafe.any Js.readonly_prop

  method requireInteraction : bool Js.t Js.readonly_prop

  method silent : bool Js.t Js.opt Js.readonly_prop

  method renotify : bool Js.t Js.readonly_prop

  method timestamp : Js.number_t Js.readonly_prop

  method actions : notificationAction Js.t Js.js_array Js.t Js.readonly_prop

  method close : unit Js.meth

  method onclick : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onclose : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onerror : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onshow : ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

let notification_global = Js.Unsafe.global##._Notification

let is_supported () = Js.Optdef.test notification_global

let notification : (Js.js_string Js.t -> notification Js.t) Js.constr =
  notification_global

let notification_with_options :
    (Js.js_string Js.t -> notificationOptions Js.t -> notification Js.t) Js.constr =
  notification_global

let permission () : Js.js_string Js.t = notification_global##.permission

let max_actions () : int = notification_global##.maxActions

let request_permission () : Js.js_string Js.t Promise.t =
  Promise.of_any notification_global##requestPermission
