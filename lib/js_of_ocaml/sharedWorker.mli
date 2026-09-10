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

(** Shared Workers.

    A [SharedWorker] is a worker shared by every browsing context of the same
    origin that creates one with the same script URL and name. Each context
    communicates with the worker through the {!MessageChannel.messagePort}
    exposed as {!class-type:sharedWorker}'s [port]; inside the worker, each new
    connection is delivered as a [connect] event carrying the other end of that
    port.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/SharedWorker>
    @see <https://html.spec.whatwg.org/multipage/workers.html#shared-workers-and-the-sharedworker-interface> *)

open Js

(** {2 Page side} *)

class type sharedWorker = object ('self)
  inherit Dom_html.eventTarget

  method port : MessageChannel.messagePort t readonly_prop
  (** The port used to communicate with the worker. When messages are received
      through the [onmessage] property the port is started implicitly;
      otherwise call [port##start] to begin receiving. *)

  method onerror : ('self t, Worker.errorEvent t) Dom.event_listener writeonly_prop
end

(** Options for the {!sharedWorker_withOptions} constructor. All fields are
    optional; create an empty record with {!empty_worker_options}. *)
class type workerOptions = object
  method name : js_string t writeonly_prop
  (** Identifies the worker: contexts constructing a [SharedWorker] with the
      same URL and name share a single worker instance. *)

  method _type : js_string t writeonly_prop
  (** Either ["classic"] (default) or ["module"]. *)

  method credentials : js_string t writeonly_prop
  (** One of ["omit"], ["same-origin"] (default) or ["include"]. Only used
      with module workers. *)
end

val empty_worker_options : unit -> workerOptions t

val sharedWorker : (js_string t -> sharedWorker t) constr
(** [new%js sharedWorker url] connects to the shared worker running the script
    at [url], starting it if it is not running yet. *)

val sharedWorker_withName : (js_string t -> js_string t -> sharedWorker t) constr
(** [new%js sharedWorker_withName url name] connects to the shared worker
    identified by [url] and [name]. *)

val sharedWorker_withOptions : (js_string t -> workerOptions t -> sharedWorker t) constr

val is_supported : unit -> bool
(** Whether the [SharedWorker] global is available in the current
    environment. *)

(** {2 Worker side} *)

(** The global scope of a running shared worker (its [self]). Only meaningful
    when the current code is actually running as a shared worker. *)
class type sharedWorkerGlobalScope = object ('self)
  inherit Dom_html.eventTarget

  method name : js_string t readonly_prop
  (** The name given at construction, or the empty string. *)

  method close : unit meth

  method onconnect :
    ('self t, ('self, Unsafe.any) Dom_html.messageEvent t) Dom.event_listener
    writeonly_prop
  (** Fired each time a new context connects. The first element of the event's
      [ports] is the {!MessageChannel.messagePort} to that context. *)
end

val global : unit -> sharedWorkerGlobalScope t
(** The shared worker's global scope. *)

val set_onconnect : (MessageChannel.messagePort t -> unit) -> unit
(** [set_onconnect f] calls [f port] each time a new context connects to the
    worker, where [port] is the port to that context (already extracted from
    the [connect] event). To receive its messages, set [port##.onmessage] or
    add a [message] listener and call [port##start]. Raises [Invalid_argument]
    when not running in a shared worker. *)
