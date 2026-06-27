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

class type serviceWorker = object ('self)
  inherit Dom_html.eventTarget

  method scriptURL : Js.js_string Js.t Js.readonly_prop

  method state : Js.js_string Js.t Js.readonly_prop

  method postMessage : 'a. 'a -> unit Js.meth

  method postMessage_withTransfer :
    'a. 'a -> Js.Unsafe.any Js.js_array Js.t -> unit Js.meth

  method onstatechange :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

class type registrationOptions = object
  method scope : Js.js_string Js.t Js.writeonly_prop

  method _type : Js.js_string Js.t Js.writeonly_prop

  method updateViaCache : Js.js_string Js.t Js.writeonly_prop
end

let empty_registration_options () : registrationOptions Js.t = Js.Unsafe.obj [||]

class type navigationPreloadState = object
  method enabled : bool Js.t Js.readonly_prop

  method headerValue : Js.js_string Js.t Js.optdef Js.readonly_prop
end

class type navigationPreloadManager = object
  method enable : unit Promise.t Js.meth

  method disable : unit Promise.t Js.meth

  method setHeaderValue : Js.js_string Js.t -> unit Promise.t Js.meth

  method getState : navigationPreloadState Js.t Promise.t Js.meth
end

class type serviceWorkerRegistration = object ('self)
  inherit Dom_html.eventTarget

  method installing : serviceWorker Js.t Js.opt Js.readonly_prop

  method waiting : serviceWorker Js.t Js.opt Js.readonly_prop

  method active : serviceWorker Js.t Js.opt Js.readonly_prop

  method navigationPreload : navigationPreloadManager Js.t Js.readonly_prop

  method scope : Js.js_string Js.t Js.readonly_prop

  method updateViaCache : Js.js_string Js.t Js.readonly_prop

  method update : serviceWorkerRegistration Js.t Promise.t Js.meth

  method unregister : bool Js.t Promise.t Js.meth

  method onupdatefound :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop
end

class type serviceWorkerContainer = object ('self)
  inherit Dom_html.eventTarget

  method controller : serviceWorker Js.t Js.opt Js.readonly_prop

  method ready : serviceWorkerRegistration Js.t Promise.t Js.readonly_prop

  method register : Js.js_string Js.t -> serviceWorkerRegistration Js.t Promise.t Js.meth

  method register_withOptions :
       Js.js_string Js.t
    -> registrationOptions Js.t
    -> serviceWorkerRegistration Js.t Promise.t Js.meth

  method getRegistration : serviceWorkerRegistration Js.t Js.optdef Promise.t Js.meth

  method getRegistration_withUrl :
    Js.js_string Js.t -> serviceWorkerRegistration Js.t Js.optdef Promise.t Js.meth

  method getRegistrations :
    serviceWorkerRegistration Js.t Js.js_array Js.t Promise.t Js.meth

  method startMessages : unit Js.meth

  method oncontrollerchange :
    ('self Js.t, 'self Dom.event Js.t) Dom.event_listener Js.writeonly_prop

  method onmessage :
    ('self Js.t, Js.Unsafe.any MessageChannel.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop

  method onmessageerror :
    ('self Js.t, Js.Unsafe.any MessageChannel.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop
end

let container () : serviceWorkerContainer Js.t Js.optdef =
  Js.Optdef.case
    Js.Unsafe.global##.navigator
    (fun () -> Js.undefined)
    (fun nav -> nav##.serviceWorker)

let is_supported () = Js.Optdef.test (container ())

class type extendableEvent = object
  inherit Dom_html.event

  method waitUntil : 'a. 'a Promise.t -> unit Js.meth
end

class type fetchEvent = object
  inherit extendableEvent

  method request : Fetch.request Js.t Js.readonly_prop

  method clientId : Js.js_string Js.t Js.readonly_prop

  method resultingClientId : Js.js_string Js.t Js.readonly_prop

  method replacesClientId : Js.js_string Js.t Js.readonly_prop

  method preloadResponse : Js.Unsafe.any Promise.t Js.readonly_prop

  method handled : unit Promise.t Js.readonly_prop

  method respondWith : Fetch.response Js.t Promise.t -> unit Js.meth
end

class type client = object
  method id : Js.js_string Js.t Js.readonly_prop

  method url : Js.js_string Js.t Js.readonly_prop

  method _type : Js.js_string Js.t Js.readonly_prop

  method frameType : Js.js_string Js.t Js.readonly_prop

  method postMessage : 'a. 'a -> unit Js.meth

  method postMessage_withTransfer :
    'a. 'a -> Js.Unsafe.any Js.js_array Js.t -> unit Js.meth
end

class type windowClient = object
  inherit client

  method focused : bool Js.t Js.readonly_prop

  method visibilityState : Js.js_string Js.t Js.readonly_prop

  method ancestorOrigins : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

  method focus : windowClient Js.t Promise.t Js.meth

  method navigate : Js.js_string Js.t -> windowClient Js.t Js.opt Promise.t Js.meth
end

class type clientsQueryOptions = object
  method includeUncontrolled : bool Js.t Js.writeonly_prop

  method _type : Js.js_string Js.t Js.writeonly_prop
end

let empty_clients_query_options () : clientsQueryOptions Js.t = Js.Unsafe.obj [||]

class type clients = object
  method get : Js.js_string Js.t -> client Js.t Js.optdef Promise.t Js.meth

  method matchAll : client Js.t Js.js_array Js.t Promise.t Js.meth

  method matchAll_withOptions :
    clientsQueryOptions Js.t -> client Js.t Js.js_array Js.t Promise.t Js.meth

  method openWindow : Js.js_string Js.t -> windowClient Js.t Js.opt Promise.t Js.meth

  method claim : unit Promise.t Js.meth
end

class type serviceWorkerGlobalScope = object ('self)
  inherit Dom_html.eventTarget

  method clients : clients Js.t Js.readonly_prop

  method registration : serviceWorkerRegistration Js.t Js.readonly_prop

  method serviceWorker : serviceWorker Js.t Js.readonly_prop

  method skipWaiting : unit Promise.t Js.meth

  method oninstall :
    ('self Js.t, extendableEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onactivate :
    ('self Js.t, extendableEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onfetch : ('self Js.t, fetchEvent Js.t) Dom.event_listener Js.writeonly_prop

  method onmessage :
    ('self Js.t, Js.Unsafe.any MessageChannel.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop

  method onmessageerror :
    ('self Js.t, Js.Unsafe.any MessageChannel.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop
end

let global () : serviceWorkerGlobalScope Js.t = Js.Unsafe.global
