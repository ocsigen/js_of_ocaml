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

class type sharedWorker = object ('self)
  inherit Dom_html.eventTarget

  method port : MessageChannel.messagePort Js.t Js.readonly_prop

  method onerror :
    ('self Js.t, Worker.errorEvent Js.t) Dom.event_listener Js.writeonly_prop
end

class type workerOptions = object
  method name : Js.js_string Js.t Js.writeonly_prop

  method _type : Js.js_string Js.t Js.writeonly_prop

  method credentials : Js.js_string Js.t Js.writeonly_prop
end

let empty_worker_options () : workerOptions Js.t = Js.Unsafe.obj [||]

let sharedWorker : (Js.js_string Js.t -> sharedWorker Js.t) Js.constr =
  Js.Unsafe.global##._SharedWorker

let sharedWorker_withName :
    (Js.js_string Js.t -> Js.js_string Js.t -> sharedWorker Js.t) Js.constr =
  Js.Unsafe.global##._SharedWorker

let sharedWorker_withOptions :
    (Js.js_string Js.t -> workerOptions Js.t -> sharedWorker Js.t) Js.constr =
  Js.Unsafe.global##._SharedWorker

let is_supported () = Js.Optdef.test Js.Unsafe.global##._SharedWorker

class type sharedWorkerGlobalScope = object ('self)
  inherit Dom_html.eventTarget

  method name : Js.js_string Js.t Js.readonly_prop

  method close : unit Js.meth

  method onconnect :
    ('self Js.t, ('self, Js.Unsafe.any) Dom_html.messageEvent Js.t) Dom.event_listener
    Js.writeonly_prop
end

let global () : sharedWorkerGlobalScope Js.t = Js.Unsafe.global

let set_onconnect handler =
  if not (Js.Optdef.test Js.Unsafe.global##.onconnect)
  then invalid_arg "SharedWorker.set_onconnect: onconnect is undefined";
  let js_handler (ev : (_, _) Dom_html.messageEvent Js.t) =
    let port : MessageChannel.messagePort Js.t =
      Js.Unsafe.coerce
        (Js.Optdef.get (Js.array_get ev##.ports 0) (fun () -> assert false))
    in
    handler port
  in
  Js.Unsafe.global##.onconnect := Js.wrap_callback js_handler
