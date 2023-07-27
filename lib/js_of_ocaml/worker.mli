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

(** Low-level bindgins to javascript Web Workers.

    See {{:https://developer.mozilla.org/en-US/docs/Web/API/Worker}
    the documented Javascript API} and some more general documentation
    {{:https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers}
    about the usage of WebWorker}.

 *)

open Js
open Dom_html

class type ['a, 'b] worker = object ('self)
  inherit eventTarget

  method onerror : ('self t, errorEvent t) event_listener writeonly_prop

  method onmessage : ('self t, 'b messageEvent t) event_listener writeonly_prop

  method postMessage : 'a -> unit meth

  method terminate : unit meth
end

and errorEvent = object
  inherit event

  method message : js_string t readonly_prop

  method filename : js_string t readonly_prop

  method lineno : int readonly_prop

  method colno : int readonly_prop

  method error : Unsafe.any readonly_prop
end

and ['a] messageEvent = object
  inherit event

  method data : 'a readonly_prop
end

val create : string -> ('a, 'b) worker t

(** {2 Global function to be used by the worker.} *)

val import_scripts : string list -> unit

val set_onmessage : ('a -> unit) -> unit

val post_message : 'a -> unit
