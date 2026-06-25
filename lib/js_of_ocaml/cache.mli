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

(** Cache API: persistent storage of {!Fetch} [Request]/[Response] pairs.

    Available from {!Dom_html.window} and from {!ServiceWorker} contexts; the
    Cache API requires a secure context. The methods are Promise-typed — see
    {!Promise}.

    @see <https://developer.mozilla.org/en-US/docs/Web/API/Cache>
    @see <https://developer.mozilla.org/en-US/docs/Web/API/CacheStorage>
    @see <https://w3c.github.io/ServiceWorker/#cache-interface> *)

open Js

(** Options controlling how a request is matched against cached entries.
    Create an empty record with {!empty_query_options}. *)
class type queryOptions = object
  method ignoreSearch : bool t writeonly_prop

  method ignoreMethod : bool t writeonly_prop

  method ignoreVary : bool t writeonly_prop
end

val empty_query_options : unit -> queryOptions t

(** A single named {!Cache} instance, obtained from {!class-type:cacheStorage}.

    The methods come in two flavours: the [_url] variants take the request as a
    URL string, while the others take a {!Fetch.request}. *)
class type cache = object
  method match_ : Fetch.request t -> Fetch.response t optdef Promise.t meth

  method match_url : js_string t -> Fetch.response t optdef Promise.t meth

  method match_withOptions :
    Fetch.request t -> queryOptions t -> Fetch.response t optdef Promise.t meth

  method matchAll : Fetch.response t js_array t Promise.t meth

  method matchAll_request : Fetch.request t -> Fetch.response t js_array t Promise.t meth

  method matchAll_withOptions :
    Fetch.request t -> queryOptions t -> Fetch.response t js_array t Promise.t meth

  method add : Fetch.request t -> unit Promise.t meth
  (** [cache##add req] fetches [req] and stores the resulting response. The
      promise rejects if the response does not have an ok status. *)

  method add_url : js_string t -> unit Promise.t meth

  method addAll : Fetch.request t js_array t -> unit Promise.t meth

  method addAll_url : js_string t js_array t -> unit Promise.t meth

  method put : Fetch.request t -> Fetch.response t -> unit Promise.t meth

  method put_url : js_string t -> Fetch.response t -> unit Promise.t meth

  method delete : Fetch.request t -> bool t Promise.t meth
  (** [cache##delete req] resolves with [true] if a matching entry was found
      and removed, [false] otherwise. *)

  method delete_url : js_string t -> bool t Promise.t meth

  method delete_withOptions : Fetch.request t -> queryOptions t -> bool t Promise.t meth

  method keys : Fetch.request t js_array t Promise.t meth

  method keys_request : Fetch.request t -> Fetch.request t js_array t Promise.t meth
end

(** The set of named {!Cache}s for the current origin, exposed as the global
    [caches] (see {!caches}). *)
class type cacheStorage = object
  method match_ : Fetch.request t -> Fetch.response t optdef Promise.t meth

  method match_url : js_string t -> Fetch.response t optdef Promise.t meth

  method has : js_string t -> bool t Promise.t meth

  method open_ : js_string t -> cache t Promise.t meth
  (** [caches##open_ name] resolves with the {!class-type:cache} named [name],
      creating it if it does not yet exist. *)

  method delete : js_string t -> bool t Promise.t meth

  method keys : js_string t js_array t Promise.t meth
end

val caches : unit -> cacheStorage t
(** The [caches] global ([CacheStorage]) for the current origin. Guard with
    {!is_supported} in environments where the Cache API may be missing. *)

val is_supported : unit -> bool
(** Whether the [caches] global is available in the current environment. *)
