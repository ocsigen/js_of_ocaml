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

class type headers = object
  method append : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method delete : Js.js_string Js.t -> unit Js.meth

  method get : Js.js_string Js.t -> Js.js_string Js.t Js.opt Js.meth

  method has : Js.js_string Js.t -> bool Js.t Js.meth

  method set : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth

  method forEach :
       (Js.js_string Js.t -> Js.js_string Js.t -> headers Js.t -> unit) Js.callback
    -> unit Js.meth
end

let headers : headers Js.t Js.constr = Js.Unsafe.global##._Headers

let headers_of_list (l : (string * string) list) : headers Js.t =
  let h : headers Js.t =
    Js.Unsafe.new_obj (Js.Unsafe.global##._Headers : _ Js.constr) [||]
  in
  List.iter (fun (k, v) -> h##append (Js.string k) (Js.string v)) l;
  h

class type body = object
  method bodyUsed : bool Js.t Js.readonly_prop

  method arrayBuffer : Typed_array.arrayBuffer Js.t Promise.t Js.meth

  method blob : File.blob Js.t Promise.t Js.meth

  method json : Js.Unsafe.any Promise.t Js.meth

  method text : Js.js_string Js.t Promise.t Js.meth

  method formData : Form.formData Js.t Promise.t Js.meth
end

class type requestInit = object
  method _method : Js.js_string Js.t Js.writeonly_prop

  method headers : headers Js.t Js.writeonly_prop

  method body : Js.Unsafe.any Js.writeonly_prop

  method mode : Js.js_string Js.t Js.writeonly_prop

  method credentials : Js.js_string Js.t Js.writeonly_prop

  method cache : Js.js_string Js.t Js.writeonly_prop

  method redirect : Js.js_string Js.t Js.writeonly_prop

  method referrer : Js.js_string Js.t Js.writeonly_prop

  method referrerPolicy : Js.js_string Js.t Js.writeonly_prop

  method integrity : Js.js_string Js.t Js.writeonly_prop

  method keepalive : bool Js.t Js.writeonly_prop

  method signal : Abort.signal Js.t Js.writeonly_prop
end

let empty_request_init () : requestInit Js.t = Js.Unsafe.obj [||]

class type request = object
  inherit body

  method url : Js.js_string Js.t Js.readonly_prop

  method _method : Js.js_string Js.t Js.readonly_prop

  method headers : headers Js.t Js.readonly_prop

  method destination : Js.js_string Js.t Js.readonly_prop

  method referrer : Js.js_string Js.t Js.readonly_prop

  method referrerPolicy : Js.js_string Js.t Js.readonly_prop

  method mode : Js.js_string Js.t Js.readonly_prop

  method credentials : Js.js_string Js.t Js.readonly_prop

  method cache : Js.js_string Js.t Js.readonly_prop

  method redirect : Js.js_string Js.t Js.readonly_prop

  method integrity : Js.js_string Js.t Js.readonly_prop

  method keepalive : bool Js.t Js.readonly_prop

  method signal : Abort.signal Js.t Js.readonly_prop

  method clone : request Js.t Js.meth
end

let request : (Js.js_string Js.t -> request Js.t) Js.constr = Js.Unsafe.global##._Request

let request_with_init : (Js.js_string Js.t -> requestInit Js.t -> request Js.t) Js.constr
    =
  Js.Unsafe.global##._Request

let request_of_request : (request Js.t -> request Js.t) Js.constr =
  Js.Unsafe.global##._Request

let request_of_request_with_init :
    (request Js.t -> requestInit Js.t -> request Js.t) Js.constr =
  Js.Unsafe.global##._Request

class type responseInit = object
  method status : int Js.writeonly_prop

  method statusText : Js.js_string Js.t Js.writeonly_prop

  method headers : headers Js.t Js.writeonly_prop
end

let empty_response_init () : responseInit Js.t = Js.Unsafe.obj [||]

class type response = object
  inherit body

  method headers : headers Js.t Js.readonly_prop

  method ok : bool Js.t Js.readonly_prop

  method redirected : bool Js.t Js.readonly_prop

  method status : int Js.readonly_prop

  method statusText : Js.js_string Js.t Js.readonly_prop

  method _type : Js.js_string Js.t Js.readonly_prop

  method url : Js.js_string Js.t Js.readonly_prop

  method clone : response Js.t Js.meth
end

let response : (Js.Unsafe.any -> response Js.t) Js.constr = Js.Unsafe.global##._Response

let response_with_init : (Js.Unsafe.any -> responseInit Js.t -> response Js.t) Js.constr =
  Js.Unsafe.global##._Response

let fetch_global = Js.Unsafe.global##.fetch

let is_supported () = Js.Optdef.test fetch_global

let fetch (url : Js.js_string Js.t) : response Js.t Promise.t =
  Promise.of_any (Js.Unsafe.fun_call fetch_global [| Js.Unsafe.inject url |])

let fetch_with_init (url : Js.js_string Js.t) (init : requestInit Js.t) :
    response Js.t Promise.t =
  Promise.of_any
    (Js.Unsafe.fun_call fetch_global [| Js.Unsafe.inject url; Js.Unsafe.inject init |])

let fetch_request (req : request Js.t) : response Js.t Promise.t =
  Promise.of_any (Js.Unsafe.fun_call fetch_global [| Js.Unsafe.inject req |])
