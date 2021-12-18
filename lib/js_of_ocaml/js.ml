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
  type +'a t

  type (-'a, +'b) meth_callback

  module Unsafe = struct
    type top

    type any = top t

    type any_js_array = any

    external inject : 'a -> any = "%identity"

    external coerce : _ t -> _ t = "%identity"

    external get : 'a -> 'b -> 'c = "caml_js_get"

    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"

    external delete : 'a -> 'b -> unit = "caml_js_delete"

    external call : 'a -> 'b -> any array -> 'c = "caml_js_call"

    external fun_call : 'a -> any array -> 'b = "caml_js_fun_call"

    external meth_call : 'a -> string -> any array -> 'b = "caml_js_meth_call"

    external new_obj : 'a -> any array -> 'b = "caml_js_new"

    external new_obj_arr : 'a -> any_js_array -> 'b = "caml_ojs_new_arr"

    external obj : (string * any) array -> 'a = "caml_js_object"

    external equals : 'a -> 'b -> bool = "caml_js_equals"

    external pure_expr : (unit -> 'a) -> 'a = "caml_js_pure_expr"

    external eval_string : string -> 'a = "caml_js_eval_string"

    external js_expr : string -> 'a = "caml_js_expr"

    external pure_js_expr : string -> 'a = "caml_pure_js_expr"

    let global = pure_js_expr "globalThis"

    external callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback = "%identity"

    external callback_with_arguments :
      (any_js_array -> 'b) -> ('c, any_js_array -> 'b) meth_callback
      = "caml_js_wrap_callback_arguments"

    external callback_with_arity : int -> ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
      = "caml_js_wrap_callback_strict"

    external meth_callback : ('b -> 'a) -> ('b, 'a) meth_callback
      = "caml_js_wrap_meth_callback_unsafe"

    external meth_callback_with_arity : int -> ('b -> 'a) -> ('b, 'a) meth_callback
      = "caml_js_wrap_meth_callback_strict"

    external meth_callback_with_arguments :
      ('b -> any_js_array -> 'a) -> ('b, any_js_array -> 'a) meth_callback
      = "caml_js_wrap_meth_callback_arguments"

    (* DEPRECATED *)
    external variable : string -> 'a = "caml_js_var"
  end

  (****)

  type 'a opt = 'a

  type 'a optdef = 'a

  external debugger : unit -> unit = "debugger"

  let null : 'a opt = Unsafe.pure_js_expr "null"

  external some : 'a -> 'a opt = "%identity"

  let undefined : 'a optdef = Unsafe.pure_js_expr "undefined"

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

  (****)

  type +'a meth

  type +'a gen_prop

  type 'a readonly_prop = < get : 'a > gen_prop

  type 'a writeonly_prop = < set : 'a -> unit > gen_prop

  type 'a prop = < get : 'a ; set : 'a -> unit > gen_prop

  type 'a optdef_prop = < get : 'a optdef ; set : 'a -> unit > gen_prop

  type +'a constr

  (****)

  type 'a callback = (unit, 'a) meth_callback

  external wrap_callback : ('a -> 'b) -> ('c, 'a -> 'b) meth_callback
    = "caml_js_wrap_callback"

  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback
    = "caml_js_wrap_meth_callback"

  (****)

  let _true = Unsafe.pure_js_expr "true"

  let _false = Unsafe.pure_js_expr "false"

  type match_result_handle

  type string_array

  class type js_string =
    object
      method toString : js_string t meth

      method valueOf : js_string t meth

      method charAt : int -> js_string t meth

      method charCodeAt : int -> float meth

      (* This may return NaN... *)
      method concat : js_string t -> js_string t meth

      method concat_2 : js_string t -> js_string t -> js_string t meth

      method concat_3 : js_string t -> js_string t -> js_string t -> js_string t meth

      method concat_4 :
        js_string t -> js_string t -> js_string t -> js_string t -> js_string t meth

      method indexOf : js_string t -> int meth

      method indexOf_from : js_string t -> int -> int meth

      method lastIndexOf : js_string t -> int meth

      method lastIndexOf_from : js_string t -> int -> int meth

      method localeCompare : js_string t -> float meth

      method _match : regExp t -> match_result_handle t opt meth

      method replace : regExp t -> js_string t -> js_string t meth

      method replace_string : js_string t -> js_string t -> js_string t meth

      method search : regExp t -> int meth

      method slice : int -> int -> js_string t meth

      method slice_end : int -> js_string t meth

      method split : js_string t -> string_array t meth

      method split_limited : js_string t -> int -> string_array t meth

      method split_regExp : regExp t -> string_array t meth

      method split_regExpLimited : regExp t -> int -> string_array t meth

      method substring : int -> int -> js_string t meth

      method substring_toEnd : int -> js_string t meth

      method toLowerCase : js_string t meth

      method toLocaleLowerCase : js_string t meth

      method toUpperCase : js_string t meth

      method toLocaleUpperCase : js_string t meth

      method trim : js_string t meth

      method length : int readonly_prop
    end

  and regExp =
    object
      method exec : js_string t -> match_result_handle t opt meth

      method test : js_string t -> bool t meth

      method toString : js_string t meth

      method source : js_string t readonly_prop

      method global : bool t readonly_prop

      method ignoreCase : bool t readonly_prop

      method multiline : bool t readonly_prop

      method lastIndex : int prop
    end

  (* string is used by ppx_js, it needs to come before any use of the
     new syntax in this file *)
  external string : string -> js_string t = "caml_jsstring_of_string"

  external to_string : js_string t -> string = "caml_string_of_jsstring"
end

include Js

class type string_constr =
  object
    method fromCharCode : int -> js_string t meth
  end

let string_constr = Unsafe.global##._String

let regExp = Unsafe.global##._RegExp

let regExp_copy = regExp

let regExp_withFlags = regExp

class type ['a] js_array =
  object
    method toString : js_string t meth

    method toLocaleString : js_string t meth

    method concat : 'a js_array t -> 'a js_array t meth

    method join : js_string t -> js_string t meth

    method pop : 'a optdef meth

    method push : 'a -> int meth

    method push_2 : 'a -> 'a -> int meth

    method push_3 : 'a -> 'a -> 'a -> int meth

    method push_4 : 'a -> 'a -> 'a -> 'a -> int meth

    method reverse : 'a js_array t meth

    method shift : 'a optdef meth

    method slice : int -> int -> 'a js_array t meth

    method slice_end : int -> 'a js_array t meth

    method sort : ('a -> 'a -> float) callback -> 'a js_array t meth

    method sort_asStrings : 'a js_array t meth

    method splice : int -> int -> 'a js_array t meth

    method splice_1 : int -> int -> 'a -> 'a js_array t meth

    method splice_2 : int -> int -> 'a -> 'a -> 'a js_array t meth

    method splice_3 : int -> int -> 'a -> 'a -> 'a -> 'a js_array t meth

    method splice_4 : int -> int -> 'a -> 'a -> 'a -> 'a -> 'a js_array t meth

    method unshift : 'a -> int meth

    method unshift_2 : 'a -> 'a -> int meth

    method unshift_3 : 'a -> 'a -> 'a -> int meth

    method unshift_4 : 'a -> 'a -> 'a -> 'a -> int meth

    method some : ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth

    method every : ('a -> int -> 'a js_array t -> bool t) callback -> bool t meth

    method forEach : ('a -> int -> 'a js_array t -> unit) callback -> unit meth

    method map : ('a -> int -> 'a js_array t -> 'b) callback -> 'b js_array t meth

    method filter : ('a -> int -> 'a js_array t -> bool t) callback -> 'a js_array t meth

    method reduce_init :
      ('b -> 'a -> int -> 'a js_array t -> 'b) callback -> 'b -> 'b meth

    method reduce : ('a -> 'a -> int -> 'a js_array t -> 'a) callback -> 'a meth

    method reduceRight_init :
      ('b -> 'a -> int -> 'a js_array t -> 'b) callback -> 'b -> 'b meth

    method reduceRight : ('a -> 'a -> int -> 'a js_array t -> 'a) callback -> 'a meth

    method length : int prop
  end

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

class type number =
  object
    method toString : js_string t meth

    method toString_radix : int -> js_string t meth

    method toLocaleString : js_string t meth

    method toFixed : int -> js_string t meth

    method toExponential : js_string t meth

    method toExponential_digits : int -> js_string t meth

    method toPrecision : int -> js_string t meth
  end

external number_of_float : float -> number t = "caml_js_from_float"

external float_of_number : number t -> float = "caml_js_to_float"

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

class type error =
  object
    method name : js_string t prop

    method message : js_string t prop

    method stack : js_string t optdef prop

    method toString : js_string t meth
  end

exception Error of error t

let error_constr = Unsafe.global##._Error

let _ = Callback.register_exception "jsError" (Error (Unsafe.obj [||]))

let raise_js_error : error t -> 'a = Unsafe.js_expr "(function (exn) { throw exn })"

external exn_with_js_backtrace : exn -> force:bool -> exn = "caml_exn_with_js_backtrace"

external js_error_of_exn : exn -> error t opt = "caml_js_error_of_exception"

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

external bool : bool -> bool t = "caml_js_from_bool"

external to_bool : bool t -> bool = "caml_js_to_bool"

external array : 'a array -> 'a js_array t = "caml_js_from_array"

external to_array : 'a js_array t -> 'a array = "caml_js_to_array"

external bytestring : string -> js_string t = "caml_jsbytes_of_string"

external to_bytestring : js_string t -> string = "caml_string_of_jsbytes"

external typeof : _ t -> js_string t = "caml_js_typeof"

external instanceof : _ t -> _ constr -> bool = "caml_js_instanceof"

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

external get_export_var : unit -> < .. > t = "caml_js_export_var"

let export_js (field : js_string t) x = Unsafe.set (get_export_var ()) field x

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

[@@@ocaml.warning "-32-60"]

module For_compatibility_only = struct
  (* Add primitives for compatibility reasons. Existing users might
     depend on it (e.g. gen_js_api), we dont want the ocaml compiler
     to complain about theses missing primitives. *)

  external caml_js_from_string : string -> js_string t = "caml_js_from_string"

  external caml_js_to_byte_string : js_string t -> string = "caml_js_to_byte_string"

  external caml_js_to_string : js_string t -> string = "caml_js_to_string"

  external caml_list_of_js_array : 'a js_array t -> 'a list = "caml_list_of_js_array"

  external caml_list_to_js_array : 'a list -> 'a js_array t = "caml_list_to_js_array"
end
