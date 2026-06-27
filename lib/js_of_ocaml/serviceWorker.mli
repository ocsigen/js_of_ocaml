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

(** Service Workers.

    This module covers both sides of the API: the page-facing registration API
    ({!container}, {!class-type:serviceWorkerContainer},
    {!class-type:serviceWorkerRegistration}, {!class-type:serviceWorker}) and
    the worker-side global scope ({!global},
    {!class-type:serviceWorkerGlobalScope} and its events). The Cache API used
    inside a worker lives in {!Cache}.

    Service Workers require a secure context. The Promise-typed members use
    {!Promise}.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API>
    @see <https://w3c.github.io/ServiceWorker/> *)

open Js

(** A service worker, as seen from a controlled page. Inherits [EventTarget]. *)
class type serviceWorker = object ('self)
  inherit Dom_html.eventTarget

  method scriptURL : js_string t readonly_prop

  method state : js_string t readonly_prop
  (** One of ["parsed"], ["installing"], ["installed"], ["activating"],
      ["activated"] or ["redundant"]. *)

  method postMessage : 'a. 'a -> unit meth

  method postMessage_withTransfer : 'a. 'a -> Unsafe.any js_array t -> unit meth

  method onstatechange : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

(** Options for {!class-type:serviceWorkerContainer}[##register_withOptions].
    Create an empty record with {!empty_registration_options}. *)
class type registrationOptions = object
  method scope : js_string t writeonly_prop

  method _type : js_string t writeonly_prop
  (** Either ["classic"] (default) or ["module"]. *)

  method updateViaCache : js_string t writeonly_prop
  (** Either ["imports"] (default), ["all"] or ["none"]. *)
end

val empty_registration_options : unit -> registrationOptions t

(** The state of a registration's navigation preload, as resolved by
    {!class-type:navigationPreloadManager}[##getState]. *)
class type navigationPreloadState = object
  method enabled : bool t readonly_prop

  method headerValue : js_string t optdef readonly_prop
  (** The [Service-Worker-Navigation-Preload] header value; [undefined] when
      not set. *)
end

(** Manages navigation preloading for a {!class-type:serviceWorkerRegistration},
    exposed as its [navigationPreload]. *)
class type navigationPreloadManager = object
  method enable : unit Promise.t meth

  method disable : unit Promise.t meth

  method setHeaderValue : js_string t -> unit Promise.t meth
  (** Sets the value sent in the [Service-Worker-Navigation-Preload] request
      header for preload requests. *)

  method getState : navigationPreloadState t Promise.t meth
end

(** A registration of a service worker against a scope. Inherits [EventTarget]. *)
class type serviceWorkerRegistration = object ('self)
  inherit Dom_html.eventTarget

  method installing : serviceWorker t opt readonly_prop

  method waiting : serviceWorker t opt readonly_prop

  method active : serviceWorker t opt readonly_prop

  method navigationPreload : navigationPreloadManager t readonly_prop

  method scope : js_string t readonly_prop

  method updateViaCache : js_string t readonly_prop

  method update : serviceWorkerRegistration t Promise.t meth
  (** Triggers a soft update of the registration; resolves with the
      registration object. *)

  method unregister : bool t Promise.t meth
  (** Resolves with [true] if the registration was found and unregistered. *)

  method onupdatefound : ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop
end

(** The page-facing entry point, exposed as [navigator.serviceWorker]
    (see {!container}). Inherits [EventTarget]. *)
class type serviceWorkerContainer = object ('self)
  inherit Dom_html.eventTarget

  method controller : serviceWorker t opt readonly_prop

  method ready : serviceWorkerRegistration t Promise.t readonly_prop

  method register : js_string t -> serviceWorkerRegistration t Promise.t meth

  method register_withOptions :
    js_string t -> registrationOptions t -> serviceWorkerRegistration t Promise.t meth

  method getRegistration : serviceWorkerRegistration t optdef Promise.t meth

  method getRegistration_withUrl :
    js_string t -> serviceWorkerRegistration t optdef Promise.t meth

  method getRegistrations : serviceWorkerRegistration t js_array t Promise.t meth

  method startMessages : unit meth

  method oncontrollerchange :
    ('self t, 'self Dom.event t) Dom.event_listener writeonly_prop

  method onmessage :
    ('self t, Unsafe.any MessageChannel.messageEvent t) Dom.event_listener writeonly_prop

  method onmessageerror :
    ('self t, Unsafe.any MessageChannel.messageEvent t) Dom.event_listener writeonly_prop
end

val container : unit -> serviceWorkerContainer t optdef
(** [navigator.serviceWorker], or [undefined] when service workers are not
    supported (including non-secure contexts). *)

val is_supported : unit -> bool
(** Whether [navigator.serviceWorker] is available in the current environment. *)

(** {1 Service worker global scope}

    The following bindings are meant to be used from {e inside} a service worker
    (see {!global}); they are not available on a controlled page. *)

(** Base class of events that may extend the service worker's lifetime via
    [waitUntil]. *)
class type extendableEvent = object
  inherit Dom_html.event

  method waitUntil : 'a. 'a Promise.t -> unit meth
end

(** The event passed to the worker's [fetch] handler. *)
class type fetchEvent = object
  inherit extendableEvent

  method request : Fetch.request t readonly_prop

  method clientId : js_string t readonly_prop

  method resultingClientId : js_string t readonly_prop

  method replacesClientId : js_string t readonly_prop

  method preloadResponse : Unsafe.any Promise.t readonly_prop

  method handled : unit Promise.t readonly_prop
  (** Resolves once the request has been handled (whether or not
      [respondWith] was called). *)

  method respondWith : Fetch.response t Promise.t -> unit meth
  (** [event##respondWith p] provides the response (as a promise) for the
      intercepted request. *)
end

(** A client (a [Window], worker or shared worker) controlled by the worker. *)
class type client = object
  method id : js_string t readonly_prop

  method url : js_string t readonly_prop

  method _type : js_string t readonly_prop
  (** One of ["window"], ["worker"] or ["sharedworker"]. *)

  method frameType : js_string t readonly_prop
  (** One of ["auxiliary"], ["top-level"], ["nested"] or ["none"]. *)

  method postMessage : 'a. 'a -> unit meth

  method postMessage_withTransfer : 'a. 'a -> Unsafe.any js_array t -> unit meth
end

(** A {!class-type:client} that is a top-level browsing context (a window/tab). *)
class type windowClient = object
  inherit client

  method focused : bool t readonly_prop

  method visibilityState : js_string t readonly_prop

  method ancestorOrigins : js_string t js_array t readonly_prop

  method focus : windowClient t Promise.t meth

  method navigate : js_string t -> windowClient t opt Promise.t meth
end

(** Options for [clients##matchAll_withOptions].
    Create an empty record with {!empty_clients_query_options}. *)
class type clientsQueryOptions = object
  method includeUncontrolled : bool t writeonly_prop

  method _type : js_string t writeonly_prop
end

val empty_clients_query_options : unit -> clientsQueryOptions t

(** The worker's [clients] object, used to enumerate and message controlled
    clients. *)
class type clients = object
  method get : js_string t -> client t optdef Promise.t meth

  method matchAll : client t js_array t Promise.t meth

  method matchAll_withOptions :
    clientsQueryOptions t -> client t js_array t Promise.t meth

  method openWindow : js_string t -> windowClient t opt Promise.t meth

  method claim : unit Promise.t meth
end

(** The global scope of a running service worker (its [self]). Inherits
    [EventTarget]. *)
class type serviceWorkerGlobalScope = object ('self)
  inherit Dom_html.eventTarget

  method clients : clients t readonly_prop

  method registration : serviceWorkerRegistration t readonly_prop

  method serviceWorker : serviceWorker t readonly_prop

  method skipWaiting : unit Promise.t meth

  method oninstall : ('self t, extendableEvent t) Dom.event_listener writeonly_prop

  method onactivate : ('self t, extendableEvent t) Dom.event_listener writeonly_prop

  method onfetch : ('self t, fetchEvent t) Dom.event_listener writeonly_prop

  method onmessage :
    ('self t, Unsafe.any MessageChannel.messageEvent t) Dom.event_listener writeonly_prop

  method onmessageerror :
    ('self t, Unsafe.any MessageChannel.messageEvent t) Dom.event_listener writeonly_prop
end

val global : unit -> serviceWorkerGlobalScope t
(** The service worker's global scope. Only meaningful when the current code is
    actually running as a service worker. *)
