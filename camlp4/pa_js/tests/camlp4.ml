Sys.interactive := false;;
#use "topfind";;
#camlp4o;;
#load "../lib/pa_js.cma";;

module Js_of_ocaml = struct
module Js = struct
  type +'a t
  type (-'a, +'b) meth_callback
  type 'a opt = 'a
  type 'a optdef = 'a

  type +'a meth
  type +'a gen_prop
  type 'a readonly_prop = <get : 'a> gen_prop
  type 'a writeonly_prop = <set : 'a -> unit> gen_prop
  type 'a prop = <get : 'a; set : 'a -> unit> gen_prop
  type 'a optdef_prop = <get : 'a optdef; set : 'a -> unit> gen_prop

  type +'a constr

  (****)

  type 'a callback = (unit, 'a) meth_callback
  module Unsafe = struct
    type any
    type any_js_array = any

    let inject : 'a -> any =
      fun _ -> assert false

    let get : 'a -> 'b -> 'c = fun _ _ -> assert false
    let set : 'a -> 'b -> 'c -> unit = fun _ _ _ -> assert false

    let meth_call : 'a -> string -> any array -> 'b =
      fun _ _ _ -> assert false

    let obj : (string * any) array -> 'a =
      fun _ -> assert false

  end
  let wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback =
    fun _ -> assert false
end
end
open Js_of_ocaml;;
Sys.interactive := true;;
(* Test methods. *)

fun (obj : int) -> obj##m();;

fun (obj : < > Js.t) -> obj##m();;

fun (obj : < m : float Js.prop > Js.t) -> obj##m();;

fun (obj : < m : float Js.meth > Js.t) -> obj##m() + 1;;

fun (obj : < m : int -> int Js.meth > Js.t) -> obj##m() + 1;;

fun (obj : < m : int Js.meth > Js.t) -> obj##m(1);;

fun (obj : < m : int -> float Js.meth > Js.t) -> obj##m(1) + 1;;

fun (obj : < m : 'a. 'a -> unit Js.meth > Js.t) -> obj##m("string"); obj##m(); obj##w();;


(* Test prop reading. *)

fun (obj : int) -> obj##p;;

fun (obj : < > Js.t) -> obj##p;;

fun (obj : < p : float Js.writeonly_prop > Js.t) -> obj##p + 1;;

fun (obj : < p : float Js.prop > Js.t) -> obj##p + 1;;


(* Test prop writing. *)

fun (obj : int) -> obj##p <- 2;;

fun (obj : < > Js.t) -> obj##p <- 2;;

fun (obj : < p : float Js.readonly_prop > Js.t) -> obj##p <- 1;;

fun (obj : < p : float Js.prop > Js.t) -> obj##p <- 1;;

fun (obj : < p : int Js.prop > Js.t) -> (obj##p <- 1) + 1;;


(* Test objects. *)

fun (obj : < > Js.t) -> obj = jsobject val m = () end;;

fun (obj : < m : float Js.prop > Js.t) -> obj = jsobject val mutable m = 0 end;;

fun () ->
  (jsobject
    val r = 2
    val mutable w = 3.
    method m = ""
  end : < m : int Js.meth; .. > Js.t);;

fun () ->
  jsobject
    val r = 2
    val _r = 2
  end;;

fun () ->
  (jsobject
    val _r_a = 2
    val _r_b = 2
  end : <_r_a : int Js.readonly_prop > Js.t);;

fun () ->
  jsobject
    val mutable w = 2
    val mutable _w = 2
  end;;

fun () ->
  (jsobject
    val mutable _w_a = 2
    val mutable _w_b = 2
  end : <_r_a : int Js.prop > Js.t);;

fun () ->
  jsobject
    method m = ""
    method _m = ""
  end;;

fun () ->
  (jsobject
    method _m_a = ""
    method _m_bc = ""
  end : <_m_a : string Js.meth; .. > Js.t);;
