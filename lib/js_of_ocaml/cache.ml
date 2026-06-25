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

class type queryOptions = object
  method ignoreSearch : bool Js.t Js.writeonly_prop

  method ignoreMethod : bool Js.t Js.writeonly_prop

  method ignoreVary : bool Js.t Js.writeonly_prop
end

let empty_query_options () : queryOptions Js.t = Js.Unsafe.obj [||]

class type cache = object
  method match_ : Fetch.request Js.t -> Fetch.response Js.t Js.optdef Promise.t Js.meth

  method match_url : Js.js_string Js.t -> Fetch.response Js.t Js.optdef Promise.t Js.meth

  method match_withOptions :
       Fetch.request Js.t
    -> queryOptions Js.t
    -> Fetch.response Js.t Js.optdef Promise.t Js.meth

  method matchAll : Fetch.response Js.t Js.js_array Js.t Promise.t Js.meth

  method matchAll_request :
    Fetch.request Js.t -> Fetch.response Js.t Js.js_array Js.t Promise.t Js.meth

  method matchAll_withOptions :
       Fetch.request Js.t
    -> queryOptions Js.t
    -> Fetch.response Js.t Js.js_array Js.t Promise.t Js.meth

  method add : Fetch.request Js.t -> unit Promise.t Js.meth

  method add_url : Js.js_string Js.t -> unit Promise.t Js.meth

  method addAll : Fetch.request Js.t Js.js_array Js.t -> unit Promise.t Js.meth

  method addAll_url : Js.js_string Js.t Js.js_array Js.t -> unit Promise.t Js.meth

  method put : Fetch.request Js.t -> Fetch.response Js.t -> unit Promise.t Js.meth

  method put_url : Js.js_string Js.t -> Fetch.response Js.t -> unit Promise.t Js.meth

  method delete : Fetch.request Js.t -> bool Js.t Promise.t Js.meth

  method delete_url : Js.js_string Js.t -> bool Js.t Promise.t Js.meth

  method delete_withOptions :
    Fetch.request Js.t -> queryOptions Js.t -> bool Js.t Promise.t Js.meth

  method keys : Fetch.request Js.t Js.js_array Js.t Promise.t Js.meth

  method keys_request :
    Fetch.request Js.t -> Fetch.request Js.t Js.js_array Js.t Promise.t Js.meth
end

class type cacheStorage = object
  method match_ : Fetch.request Js.t -> Fetch.response Js.t Js.optdef Promise.t Js.meth

  method match_url : Js.js_string Js.t -> Fetch.response Js.t Js.optdef Promise.t Js.meth

  method has : Js.js_string Js.t -> bool Js.t Promise.t Js.meth

  method open_ : Js.js_string Js.t -> cache Js.t Promise.t Js.meth

  method delete : Js.js_string Js.t -> bool Js.t Promise.t Js.meth

  method keys : Js.js_string Js.t Js.js_array Js.t Promise.t Js.meth
end

let caches () : cacheStorage Js.t = Js.Unsafe.global##.caches

let is_supported () = Js.Optdef.test Js.Unsafe.global##.caches
