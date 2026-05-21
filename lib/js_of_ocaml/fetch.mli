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

(** Fetch API.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API>
    @see <https://fetch.spec.whatwg.org/> *)

open Js

(** {1 Headers} *)

class type headers = object
  method append : js_string t -> js_string t -> unit meth

  method delete : js_string t -> unit meth

  method get : js_string t -> js_string t opt meth

  method has : js_string t -> bool t meth

  method set : js_string t -> js_string t -> unit meth

  method forEach :
    (js_string t -> js_string t -> headers t -> unit) Js.callback -> unit meth
end

val headers : headers t constr

val headers_of_list : (string * string) list -> headers Js.t
(** Build a [headers] object from a list of [(name, value)] pairs. *)

(** {1 Request} *)

(** The body-reader methods are Promise-typed — see {!Promise}. *)
class type body = object
  method bodyUsed : bool t readonly_prop

  method arrayBuffer : Typed_array.arrayBuffer t Promise.t meth

  method blob : File.blob t Promise.t meth

  method json : Unsafe.any Promise.t meth

  method text : js_string t Promise.t meth

  method formData : Form.formData t Promise.t meth
end

(** Initializer for {!request} (and {!fetch_with_init}). All fields are
    optional; create an empty record with {!empty_request_init} and
    populate the ones you need. *)
class type requestInit = object
  method _method : js_string t writeonly_prop

  method headers : headers t writeonly_prop

  method body : Unsafe.any writeonly_prop

  method mode : js_string t writeonly_prop

  method credentials : js_string t writeonly_prop

  method cache : js_string t writeonly_prop

  method redirect : js_string t writeonly_prop

  method referrer : js_string t writeonly_prop

  method referrerPolicy : js_string t writeonly_prop

  method integrity : js_string t writeonly_prop

  method keepalive : bool t writeonly_prop

  method signal : Abort.signal t writeonly_prop
end

val empty_request_init : unit -> requestInit t

class type request = object
  inherit body

  method url : js_string t readonly_prop

  method _method : js_string t readonly_prop

  method headers : headers t readonly_prop

  method destination : js_string t readonly_prop

  method referrer : js_string t readonly_prop

  method referrerPolicy : js_string t readonly_prop

  method mode : js_string t readonly_prop

  method credentials : js_string t readonly_prop

  method cache : js_string t readonly_prop

  method redirect : js_string t readonly_prop

  method integrity : js_string t readonly_prop

  method keepalive : bool t readonly_prop

  method signal : Abort.signal t readonly_prop

  method clone : request t meth
end

val request : (js_string t -> request t) constr

val request_with_init : (js_string t -> requestInit t -> request t) constr

(** {1 Response} *)

class type responseInit = object
  method status : int writeonly_prop

  method statusText : js_string t writeonly_prop

  method headers : headers t writeonly_prop
end

val empty_response_init : unit -> responseInit t

class type response = object
  inherit body

  method headers : headers t readonly_prop

  method ok : bool t readonly_prop

  method redirected : bool t readonly_prop

  method status : int readonly_prop

  method statusText : js_string t readonly_prop

  method _type : js_string t readonly_prop

  method url : js_string t readonly_prop

  method clone : response t meth
end

val response : (Unsafe.any -> response t) constr

val response_with_init : (Unsafe.any -> responseInit t -> response t) constr

(** {1 fetch} *)

val fetch : js_string t -> response t Promise.t

val fetch_with_init : js_string t -> requestInit t -> response t Promise.t

val fetch_request : request t -> response t Promise.t

val is_supported : unit -> bool
(** Whether the [fetch] global is available in the current environment. *)
