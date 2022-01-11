(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(* This local module [Js] is needed so that the ppx_js extension work within that file. *)
module Js = struct
  module Unsafe = struct
    include Js_of_ocaml_runtime.Js.Unsafe

    let global = pure_js_expr "globalThis"
  end

  include (
    Js_of_ocaml_runtime.Js :
      module type of struct
        include Js_of_ocaml_runtime.Js
      end
      with module Unsafe := Js_of_ocaml_runtime.Js.Unsafe)
end

include Js

let null : 'a opt = Unsafe.pure_js_expr "null"

let undefined : 'a optdef = Unsafe.pure_js_expr "undefined"

let _true = Unsafe.pure_js_expr "true"

let _false = Unsafe.pure_js_expr "false"

external some : 'a -> 'a opt = "%identity"

external def : 'a -> 'a optdef = "%identity"

module type OPT = sig
  type 'a t

  val empty : 'a t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val test : 'a t -> bool

  val iter : 'a t -> ('a -> unit) -> unit

  val case : 'a t -> (unit -> 'b) -> ('a -> 'b) -> 'b

  val get : 'a t -> (unit -> 'a) -> 'a

  val option : 'a option -> 'a t

  val to_option : 'a t -> 'a option
end

module Opt : OPT with type 'a t = 'a opt = struct
  type 'a t = 'a opt

  let empty = null

  let return = some

  let map x f = if Unsafe.equals x null then null else return (f x)

  let bind x f = if Unsafe.equals x null then null else f x

  let test x = not (Unsafe.equals x null)

  let iter x f = if not (Unsafe.equals x null) then f x

  let case x f g = if Unsafe.equals x null then f () else g x

  let get x f = if Unsafe.equals x null then f () else x

  let option x =
    match x with
    | None -> empty
    | Some x -> return x

  let to_option x = case x (fun () -> None) (fun x -> Some x)
end

module Optdef : OPT with type 'a t = 'a optdef = struct
  type 'a t = 'a optdef

  let empty = undefined

  let return = def

  let map x f = if x == undefined then undefined else return (f x)

  let bind x f = if x == undefined then undefined else f x

  let test x = x != undefined

  let iter x f = if x != undefined then f x

  let case x f g = if x == undefined then f () else g x

  let get x f = if x == undefined then f () else x

  let option x =
    match x with
    | None -> empty
    | Some x -> return x

  let to_option x = case x (fun () -> None) (fun x -> Some x)
end

(****)

let coerce x f g = Opt.get (f x) (fun () -> g x)

let coerce_opt x f g = Opt.get (Opt.bind x f) (fun () -> g x)

class type string_constr =
  object
    method fromCharCode : int -> js_string t meth
  end

let string_constr = Unsafe.global##._String

let regExp = Unsafe.global##._RegExp

let regExp_copy = regExp

let regExp_withFlags = regExp

let object_constructor = Unsafe.global##._Object

let object_keys o : js_string t js_array t = object_constructor##keys o

let array_constructor = Unsafe.global##._Array

let array_empty = array_constructor

let array_length = array_constructor

let array_get : 'a #js_array t -> int -> 'a optdef = Unsafe.get

let array_set : 'a #js_array t -> int -> 'a -> unit = Unsafe.set

let array_map_poly :
    'a #js_array t -> ('a -> int -> 'a #js_array t -> 'b) callback -> 'b #js_array t =
 fun a cb -> (Unsafe.coerce a)##map cb

let array_map f a = array_map_poly a (wrap_callback (fun x _idx _ -> f x))

let array_mapi f a = array_map_poly a (wrap_callback (fun x idx _ -> f idx x))

class type match_result =
  object
    inherit [js_string t] js_array

    method index : int readonly_prop

    method input : js_string t readonly_prop
  end

let str_array : string_array t -> js_string t js_array t = Unsafe.coerce

let match_result : match_result_handle t -> match_result t = Unsafe.coerce

class type date =
  object
    method toString : js_string t meth

    method toDateString : js_string t meth

    method toTimeString : js_string t meth

    method toLocaleString : js_string t meth

    method toLocaleDateString : js_string t meth

    method toLocaleTimeString : js_string t meth

    method valueOf : float meth

    method getTime : float meth

    method getFullYear : int meth

    method getUTCFullYear : int meth

    method getMonth : int meth

    method getUTCMonth : int meth

    method getDate : int meth

    method getUTCDate : int meth

    method getDay : int meth

    method getUTCDay : int meth

    method getHours : int meth

    method getUTCHours : int meth

    method getMinutes : int meth

    method getUTCMinutes : int meth

    method getSeconds : int meth

    method getUTCSeconds : int meth

    method getMilliseconds : int meth

    method getUTCMilliseconds : int meth

    method getTimezoneOffset : int meth

    method setTime : float -> float meth

    method setFullYear : int -> float meth

    method setUTCFullYear : int -> float meth

    method setMonth : int -> float meth

    method setUTCMonth : int -> float meth

    method setDate : int -> float meth

    method setUTCDate : int -> float meth

    method setDay : int -> float meth

    method setUTCDay : int -> float meth

    method setHours : int -> float meth

    method setUTCHours : int -> float meth

    method setMinutes : int -> float meth

    method setUTCMinutes : int -> float meth

    method setSeconds : int -> float meth

    method setUTCSeconds : int -> float meth

    method setMilliseconds : int -> float meth

    method setUTCMilliseconds : int -> float meth

    method toUTCString : js_string t meth

    method toISOString : js_string t meth

    method toJSON : 'a -> js_string t meth
  end

class type date_constr =
  object
    method parse : js_string t -> float meth

    method _UTC_month : int -> int -> float meth

    method _UTC_day : int -> int -> float meth

    method _UTC_hour : int -> int -> int -> int -> float meth

    method _UTC_min : int -> int -> int -> int -> int -> float meth

    method _UTC_sec : int -> int -> int -> int -> int -> int -> float meth

    method _UTC_ms : int -> int -> int -> int -> int -> int -> int -> float meth

    method now : float meth
  end

let date_constr = Unsafe.global##._Date

let date : date_constr t = date_constr

let date_now : date t constr = date_constr

let date_fromTimeValue : (float -> date t) constr = date_constr

let date_month : (int -> int -> date t) constr = date_constr

let date_day : (int -> int -> int -> date t) constr = date_constr

let date_hour : (int -> int -> int -> int -> date t) constr = date_constr

let date_min : (int -> int -> int -> int -> int -> date t) constr = date_constr

let date_sec : (int -> int -> int -> int -> int -> int -> date t) constr = date_constr

let date_ms : (int -> int -> int -> int -> int -> int -> int -> date t) constr =
  date_constr

class type math =
  object
    method _E : float readonly_prop

    method _LN2 : float readonly_prop

    method _LN10 : float readonly_prop

    method _LOG2E : float readonly_prop

    method _LOG10E : float readonly_prop

    method _PI : float readonly_prop

    method _SQRT1_2_ : float readonly_prop

    method _SQRT2 : float readonly_prop

    method abs : float -> float meth

    method acos : float -> float meth

    method asin : float -> float meth

    method atan : float -> float meth

    method atan2 : float -> float -> float meth

    method ceil : float -> float meth

    method cos : float -> float meth

    method exp : float -> float meth

    method floor : float -> float meth

    method log : float -> float meth

    method max : float -> float -> float meth

    method max_3 : float -> float -> float -> float meth

    method max_4 : float -> float -> float -> float -> float meth

    method min : float -> float -> float meth

    method min_3 : float -> float -> float -> float meth

    method min_4 : float -> float -> float -> float -> float meth

    method pow : float -> float -> float meth

    method random : float meth

    method round : float -> float meth

    method sin : float -> float meth

    method sqrt : float -> float meth

    method tan : float -> float meth
  end

let math = Unsafe.global##._Math

exception Error = Js_of_ocaml_runtime.Js_error.Exn

let error_constr = Unsafe.global##._Error

let raise_js_error : error t -> 'a = Unsafe.js_expr "(function (exn) { throw exn })"

class type json =
  object
    method parse : js_string t -> 'a meth

    method stringify : 'a -> js_string t meth
  end

let _JSON : json t = Unsafe.global##._JSON

let decodeURI (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.decodeURI [| Unsafe.inject s |]

let decodeURIComponent (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.decodeURIComponent [| Unsafe.inject s |]

let encodeURI (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.encodeURI [| Unsafe.inject s |]

let encodeURIComponent (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.encodeURIComponent [| Unsafe.inject s |]

let escape (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.escape [| Unsafe.inject s |]

let unescape (s : js_string t) : js_string t =
  Unsafe.fun_call Unsafe.global##.unescape [| Unsafe.inject s |]

let isNaN (i : 'a) : bool =
  to_bool (Unsafe.fun_call Unsafe.global##.isNaN [| Unsafe.inject i |])

let parseInt (s : js_string t) : int =
  let s = Unsafe.fun_call Unsafe.global##.parseInt [| Unsafe.inject s |] in
  if isNaN s then failwith "parseInt" else s

let parseFloat (s : js_string t) : float =
  let s = Unsafe.fun_call Unsafe.global##.parseFloat [| Unsafe.inject s |] in
  if isNaN s then failwith "parseFloat" else s

let _ =
  Printexc.register_printer (function
      | Error e -> Some (to_string e##toString)
      | _ -> None)

let _ =
  Printexc.register_printer (fun e ->
      let e : < .. > t = Obj.magic e in
      if instanceof e array_constructor then None else Some (to_string e##toString))

let string_of_error e = to_string e##toString

let export_js (field : js_string t) x =
  Unsafe.set (Unsafe.pure_js_expr "jsoo_exports") field x

let export field x = export_js (string field) x

let export_all obj =
  let keys = object_keys obj in
  keys##forEach
    (wrap_callback (fun (key : js_string t) _ _ -> export_js key (Unsafe.get obj key)))

(****)

(* DEPRECATED *)

type float_prop = float prop

external float : float -> float = "%identity"

external to_float : float -> float = "%identity"
