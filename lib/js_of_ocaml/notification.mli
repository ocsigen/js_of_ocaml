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

(** Notifications API.

    A code example:
    {[
      if Notification.is_supported ()
      then
        Promise.map
          (fun perm ->
            if Js.to_string perm = "granted"
            then
              let options = Notification.empty_options () in
              options##.body := Js.string "Hello from OCaml!";
              ignore (new%js Notification.notification_with_options
                        (Js.string "Title") options))
          (Notification.request_permission ())
    ]}

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Notifications_API>
    @see <https://notifications.spec.whatwg.org/> *)

open Js

(** A single action shown alongside a (persistent) notification. Used both when
    building {!notificationOptions} and when reading {!notification}'s
    [actions]; the members are therefore read-write. *)
class type notificationAction = object
  method action : js_string t prop

  method title : js_string t prop

  method icon : js_string t prop
end

(** Initializer for {!class-type:notification}. All fields are optional; create
    an empty record with {!empty_options} and populate the ones you need. *)
class type notificationOptions = object
  method dir : js_string t writeonly_prop
  (** Text direction: ["auto"] (default), ["ltr"] or ["rtl"]. *)

  method lang : js_string t writeonly_prop

  method body : js_string t writeonly_prop

  method navigate : js_string t writeonly_prop

  method tag : js_string t writeonly_prop

  method icon : js_string t writeonly_prop

  method image : js_string t writeonly_prop

  method badge : js_string t writeonly_prop

  method vibrate : int js_array t writeonly_prop

  method data : Unsafe.any writeonly_prop

  method requireInteraction : bool t writeonly_prop

  method silent : bool t opt writeonly_prop

  method renotify : bool t writeonly_prop

  method timestamp : number_t writeonly_prop

  method actions : notificationAction t js_array t writeonly_prop
end

val empty_options : unit -> notificationOptions t

class type notification = object ('self)
  method title : js_string t readonly_prop

  method dir : js_string t readonly_prop

  method lang : js_string t readonly_prop

  method body : js_string t readonly_prop

  method navigate : js_string t readonly_prop

  method tag : js_string t readonly_prop

  method icon : js_string t readonly_prop

  method image : js_string t readonly_prop

  method badge : js_string t readonly_prop

  method vibrate : int js_array t readonly_prop

  method data : Unsafe.any readonly_prop

  method requireInteraction : bool t readonly_prop

  method silent : bool t opt readonly_prop

  method renotify : bool t readonly_prop

  method timestamp : number_t readonly_prop

  method actions : notificationAction t js_array t readonly_prop

  method close : unit meth

  method onclick : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onclose : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onerror : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onshow : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

val notification : (js_string t -> notification t) constr

val notification_with_options :
  (js_string t -> notificationOptions t -> notification t) constr

val permission : unit -> js_string t
(** The current permission to display notifications: ["granted"], ["denied"] or
    ["default"]. *)

val max_actions : unit -> int
(** The maximum number of actions supported by notifications on this device. *)

val request_permission : unit -> js_string t Promise.t
(** Request permission from the user to display notifications. The promise
    resolves with the new permission value (see {!permission}). *)

val is_supported : unit -> bool
(** Whether the [Notification] global is available in the current environment. *)
