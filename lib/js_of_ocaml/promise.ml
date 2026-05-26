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

(* The wrap/unwrap helpers live in [runtime/js/promise.js] (with a Wasm
   counterpart in [runtime/wasm/promise.wat]) and are also exposed as
   OCaml externals from [Jsoo_runtime.Promise]. Wrapping is conditional
   on the value being thenable, so non-thenable resolves pay no
   allocation and foreign promises handed in via [of_any] pass through
   unchanged.

   [Wrapped.t] keeps the wrapped form opaque so callers (and the
   class-type signatures below) must go through [unwrap] before treating
   the value as an OCaml ['a]. *)

module Wrapped : sig
  type +'a t

  external wrap : 'a -> 'a t = "caml_jsoo_promise_wrap"

  external unwrap : 'a t -> 'a = "caml_jsoo_promise_unwrap"
end = struct
  type +'a t = Js.Unsafe.any

  external wrap : 'a -> 'a t = "caml_jsoo_promise_wrap"

  external unwrap : 'a t -> 'a = "caml_jsoo_promise_unwrap"
end

class type promise_object = object
  method _then :
    'a. ('a Wrapped.t -> promise_object Js.t) Js.callback -> promise_object Js.t Js.meth

  method _then_err :
    'a.
       ('a Wrapped.t -> promise_object Js.t) Js.callback
    -> (Js.Unsafe.any -> promise_object Js.t) Js.callback
    -> promise_object Js.t Js.meth

  method _catch :
    (Js.Unsafe.any -> promise_object Js.t) Js.callback -> promise_object Js.t Js.meth

  method _finally : (unit -> unit) Js.callback -> promise_object Js.t Js.meth
end

type +'a t = promise_object Js.t

type error = Js.Unsafe.any

class type with_resolvers = object
  method promise : promise_object Js.t Js.readonly_prop

  method resolve : Js.Unsafe.any Js.readonly_prop

  method reject : Js.Unsafe.any Js.readonly_prop
end

class type all_settled_entry = object
  method status : Js.js_string Js.t Js.readonly_prop

  method value : 'a. 'a Wrapped.t Js.readonly_prop

  method reason : Js.Unsafe.any Js.readonly_prop
end

class type promise_constructor = object
  method resolve : 'a. 'a Wrapped.t -> promise_object Js.t Js.meth

  method reject : Js.Unsafe.any -> promise_object Js.t Js.meth

  method all : promise_object Js.t Js.js_array Js.t -> promise_object Js.t Js.meth

  method allSettled : promise_object Js.t Js.js_array Js.t -> promise_object Js.t Js.meth

  method any : promise_object Js.t Js.js_array Js.t -> promise_object Js.t Js.meth

  method race : promise_object Js.t Js.js_array Js.t -> promise_object Js.t Js.meth

  method withResolvers : with_resolvers Js.t Js.meth
end

let promise_global : promise_constructor Js.t = Js.Unsafe.global##._Promise

let promise_constr :
    ((Js.Unsafe.any -> Js.Unsafe.any -> unit) Js.callback -> promise_object Js.t)
    Js.constr =
  Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test (Js.Unsafe.global##._Promise : _ Js.Optdef.t)

let resolve (x : 'a) : 'a t = promise_global##resolve (Wrapped.wrap x)

let reject (e : error) : 'a t = promise_global##reject e

let make (f : resolve:('a -> unit) -> reject:(error -> unit) -> unit) : 'a t =
  let body =
    Js.wrap_callback (fun resolve_js reject_js ->
        let resolve x =
          ignore
            (Js.Unsafe.fun_call resolve_js [| Js.Unsafe.inject (Wrapped.wrap x) |]
              : Js.Unsafe.any)
        in
        let reject e =
          ignore (Js.Unsafe.fun_call reject_js [| Js.Unsafe.inject e |] : Js.Unsafe.any)
        in
        f ~resolve ~reject)
  in
  new%js promise_constr body

let with_resolvers () : 'a t * ('a -> unit) * (error -> unit) =
  let r = promise_global##withResolvers in
  let resolve x =
    ignore
      (Js.Unsafe.fun_call r##.resolve [| Js.Unsafe.inject (Wrapped.wrap x) |]
        : Js.Unsafe.any)
  in
  let reject e =
    ignore (Js.Unsafe.fun_call r##.reject [| Js.Unsafe.inject e |] : Js.Unsafe.any)
  in
  r##.promise, resolve, reject

let then_ ?on_error (f : 'a -> 'b t) (p : 'a t) : 'b t =
  let cb = Js.wrap_callback (fun (w : 'a Wrapped.t) -> f (Wrapped.unwrap w)) in
  match on_error with
  | None -> p##_then cb
  | Some g ->
      let cb_err = Js.wrap_callback g in
      p##_then_err cb cb_err

let catch (f : error -> 'a t) (p : 'a t) : 'a t = p##_catch (Js.wrap_callback f)

let finally (f : unit -> unit) (p : 'a t) : 'a t = p##_finally (Js.wrap_callback f)

let map (f : 'a -> 'b) (p : 'a t) : 'b t = then_ (fun x -> resolve (f x)) p

let bind f p = then_ f p

let all (ps : 'a t list) : 'a list t =
  let arr = Js.array (Array.of_list ps) in
  let raw = promise_global##all arr in
  let cb =
    Js.wrap_callback (fun (w : 'a Wrapped.t Js.js_array Js.t Wrapped.t) ->
        let arr = Wrapped.unwrap w in
        resolve (List.map Wrapped.unwrap (Array.to_list (Js.to_array arr))))
  in
  raw##_then cb

let all_settled (ps : 'a t list) : ('a, error) result list t =
  let arr = Js.array (Array.of_list ps) in
  let raw = promise_global##allSettled arr in
  let cb =
    Js.wrap_callback (fun (w : all_settled_entry Js.t Js.js_array Js.t Wrapped.t) ->
        let arr = Wrapped.unwrap w in
        let lst =
          List.map
            (fun (entry : all_settled_entry Js.t) ->
              if String.equal (Js.to_string entry##.status) "fulfilled"
              then Ok (Wrapped.unwrap entry##.value)
              else Error (entry##.reason : error))
            (Array.to_list (Js.to_array arr))
        in
        resolve lst)
  in
  raw##_then cb

let any (ps : 'a t list) : 'a t =
  let arr = Js.array (Array.of_list ps) in
  promise_global##any arr

let race (ps : 'a t list) : 'a t =
  let arr = Js.array (Array.of_list ps) in
  promise_global##race arr

let error_of_any (x : Js.Unsafe.any) : error = x

let error_to_any (e : error) : Js.Unsafe.any = e

let error_of_exn (e : exn) : error = Js.Unsafe.inject e

let to_any (p : 'a t) : Js.Unsafe.any = Js.Unsafe.coerce p

let of_any (x : Js.Unsafe.any) : 'a t = Js.Unsafe.coerce x
