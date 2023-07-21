(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2015 OCamlPro: Grégoire Henry, Çağdaş Bozman.
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

open Js
open Dom_html
open! Import

class type ['a, 'b] worker =
  object ('self)
    inherit eventTarget

    method onerror : ('self t, errorEvent t) event_listener writeonly_prop

    method onmessage : ('self t, 'b messageEvent t) event_listener writeonly_prop

    method postMessage : 'a -> unit meth

    method terminate : unit meth
  end

and errorEvent =
  object
    inherit event

    method message : js_string t readonly_prop

    method filename : js_string t readonly_prop

    method lineno : int readonly_prop

    method colno : int readonly_prop

    method error : Unsafe.any readonly_prop
  end

and ['a] messageEvent =
  object
    inherit event

    method data : 'a readonly_prop
  end

let worker = Unsafe.global##._Worker

let create script = new%js worker (string script)

let import_scripts scripts : unit =
  if not (Js.Optdef.test Unsafe.global##.importScripts)
  then invalid_arg "Worker.import_scripts is undefined";
  Unsafe.fun_call
    Unsafe.global##.importScripts
    (Array.map (fun s -> Unsafe.inject (string s)) (Array.of_list scripts))

let set_onmessage handler =
  if not (Js.Optdef.test Unsafe.global##.onmessage)
  then invalid_arg "Worker.onmessage is undefined";
  let js_handler (ev : 'a messageEvent Js.t) = handler ev##.data in
  Unsafe.global##.onmessage := wrap_callback js_handler

let post_message msg =
  if not (Js.Optdef.test Unsafe.global##.postMessage)
  then invalid_arg "Worker.onmessage is undefined";
  Unsafe.global##postMessage msg
