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

type +'a t = Js.Unsafe.any

type error = Js.Unsafe.any

(* The wrap/unwrap helpers live in [runtime/js/promise.js] (with a Wasm
   counterpart in [runtime/wasm/promise.wat]) and are also exposed as
   OCaml externals from [Jsoo_runtime.Promise]. Wrapping is conditional
   on the value being thenable, so non-thenable resolves pay no
   allocation and foreign promises handed in via [of_any] pass through
   unchanged. *)

external wrap : 'a -> Js.Unsafe.any = "caml_jsoo_promise_wrap"

external unwrap : Js.Unsafe.any -> 'a = "caml_jsoo_promise_unwrap"

let promise_global = Js.Unsafe.global##._Promise

let is_supported () = Js.Optdef.test promise_global

let resolve (x : 'a) : 'a t = Js.Unsafe.meth_call promise_global "resolve" [| wrap x |]

let reject (e : error) : 'a t =
  Js.Unsafe.meth_call promise_global "reject" [| Js.Unsafe.inject e |]

let make (f : resolve:('a -> unit) -> reject:(error -> unit) -> unit) : 'a t =
  let body =
    Js.wrap_callback (fun resolve_js reject_js ->
        let resolve x =
          ignore (Js.Unsafe.fun_call resolve_js [| wrap x |] : Js.Unsafe.any)
        in
        let reject e =
          ignore (Js.Unsafe.fun_call reject_js [| Js.Unsafe.inject e |] : Js.Unsafe.any)
        in
        f ~resolve ~reject)
  in
  Js.Unsafe.new_obj promise_global [| Js.Unsafe.inject body |]

let then_ ?on_error (f : 'a -> 'b t) (p : 'a t) : 'b t =
  let cb = Js.wrap_callback (fun (w : Js.Unsafe.any) -> f (unwrap w)) in
  match on_error with
  | None -> Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject cb |]
  | Some g ->
      let cb_err = Js.wrap_callback g in
      Js.Unsafe.meth_call p "then" [| Js.Unsafe.inject cb; Js.Unsafe.inject cb_err |]

let catch (f : error -> 'a t) (p : 'a t) : 'a t =
  let cb = Js.wrap_callback f in
  Js.Unsafe.meth_call p "catch" [| Js.Unsafe.inject cb |]

let finally (f : unit -> unit) (p : 'a t) : 'a t =
  let cb = Js.wrap_callback f in
  Js.Unsafe.meth_call p "finally" [| Js.Unsafe.inject cb |]

let map (f : 'a -> 'b) (p : 'a t) : 'b t = then_ (fun x -> resolve (f x)) p

let bind f p = then_ f p

let all (ps : 'a t list) : 'a list t =
  let arr = Js.array (Array.of_list ps) in
  let raw = Js.Unsafe.meth_call promise_global "all" [| Js.Unsafe.inject arr |] in
  let cb =
    Js.wrap_callback (fun (a : Js.Unsafe.any Js.js_array Js.t) ->
        let lst = List.map unwrap (Array.to_list (Js.to_array a)) in
        resolve lst)
  in
  Js.Unsafe.meth_call raw "then" [| Js.Unsafe.inject cb |]

let race (ps : 'a t list) : 'a t =
  let arr = Js.array (Array.of_list ps) in
  Js.Unsafe.meth_call promise_global "race" [| Js.Unsafe.inject arr |]

let error_of_any (x : Js.Unsafe.any) : error = x

let error_to_any (e : error) : Js.Unsafe.any = e

let error_of_exn (e : exn) : error = Js.Unsafe.inject e

let to_any (p : 'a t) : Js.Unsafe.any = p

let of_any (x : Js.Unsafe.any) : 'a t = x
