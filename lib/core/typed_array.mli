(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2012 Jérôme Vouillon
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

(** Typed Array binding *)

open Js

class type arrayBuffer = object
  method byteLength : int readonly_prop
end

val arrayBuffer : (int -> arrayBuffer t) constr

class type arrayBufferView = object
  method buffer : arrayBuffer t readonly_prop
  method byteOffset : int readonly_prop
  method byteLength : int readonly_prop
end

class type ['a, 'b] typedArray = object
  inherit arrayBufferView
  method _BYTES_PER_ELEMENT : int readonly_prop
  method length : int readonly_prop
  method set_fromArray : 'a js_array t -> int -> unit meth
  method set_fromTypedArray : ('a, 'b) typedArray t -> int -> unit meth
  method subarray : int -> int -> ('a, 'b) typedArray t meth
  method subarray_toEnd : int -> ('a, 'b) typedArray t meth
  method _content_type_ : 'b
end

type int8Array = (int, [`Int8]) typedArray
type uint8Array = (int, [`Uint8]) typedArray
type int16Array = (int, [`Int16]) typedArray
type uint16Array = (int, [`Uint16]) typedArray
type int32Array = (int, [`Int32]) typedArray
type uint32Array = (float, [`Uint32]) typedArray
type float32Array = (float, [`Float32]) typedArray
type float64Array = (float, [`Float64]) typedArray

val int8Array : (int -> int8Array t) constr
val int8Array_fromArray : (int js_array t -> int8Array t) constr
val int8Array_fromTypedArray : (int8Array t -> int8Array t) constr
val int8Array_fromBuffer : (arrayBuffer t -> int8Array t) constr
val int8Array_inBuffer : (arrayBuffer t -> int -> int -> int8Array t) constr

val uint8Array : (int -> uint8Array t) constr
val uint8Array_fromArray : (int js_array t -> uint8Array t) constr
val uint8Array_fromTypedArray : (uint8Array t -> uint8Array t) constr
val uint8Array_fromBuffer : (arrayBuffer t -> uint8Array t) constr
val uint8Array_inBuffer : (arrayBuffer t -> int -> int -> uint8Array t) constr

val int16Array : (int -> int16Array t) constr
val int16Array_fromArray : (int js_array t -> int16Array t) constr
val int16Array_fromTypedArray : (int16Array t -> int16Array t) constr
val int16Array_fromBuffer : (arrayBuffer t -> int16Array t) constr
val int16Array_inBuffer : (arrayBuffer t -> int -> int -> int16Array t) constr

val uint16Array : (int -> uint16Array t) constr
val uint16Array_fromArray : (int js_array t -> uint16Array t) constr
val uint16Array_fromTypedArray : (uint16Array t -> uint16Array t) constr
val uint16Array_fromBuffer : (arrayBuffer t -> uint16Array t) constr
val uint16Array_inBuffer : (arrayBuffer t -> int -> int -> uint16Array t) constr

val int32Array : (int -> int32Array t) constr
val int32Array_fromArray : (int js_array t -> int32Array t) constr
val int32Array_fromTypedArray : (int32Array t -> int32Array t) constr
val int32Array_fromBuffer : (arrayBuffer t -> int32Array t) constr
val int32Array_inBuffer : (arrayBuffer t -> int -> int -> int32Array t) constr

val uint32Array : (int -> uint32Array t) constr
val uint32Array_fromArray : (float js_array t -> uint32Array t) constr
val uint32Array_fromTypedArray : (uint32Array t -> uint32Array t) constr
val uint32Array_fromBuffer : (arrayBuffer t -> uint32Array t) constr
val uint32Array_inBuffer : (arrayBuffer t -> int -> int -> uint32Array t) constr

val float32Array : (int -> float32Array t) constr
val float32Array_fromArray : (float js_array t -> float32Array t) constr
val float32Array_fromTypedArray : (float32Array t -> float32Array t) constr
val float32Array_fromBuffer : (arrayBuffer t -> float32Array t) constr
val float32Array_inBuffer :
  (arrayBuffer t -> int -> int -> float32Array t) constr

val float64Array : (int -> float64Array t) constr
val float64Array_fromArray : (float js_array t -> float64Array t) constr
val float64Array_fromTypedArray : (float64Array t -> float64Array t) constr
val float64Array_fromBuffer : (arrayBuffer t -> float64Array t) constr
val float64Array_inBuffer :
  (arrayBuffer t -> int -> int -> float64Array t) constr

val set : ('a,'b) typedArray t -> int -> 'a -> unit
val get : ('a,'b) typedArray t -> int -> 'a optdef
val unsafe_get : ('a,'b) typedArray t -> int -> 'a

class type dataView = object
  inherit arrayBufferView
  method getInt8 : int -> int meth
  method getUint8 : int -> int meth
  method getInt16_ : int -> bool t -> int meth
  method getUint16 : int -> int meth
  method getUint16_ : int -> bool t -> int meth
  method getInt32 : int -> int meth
  method getInt32_ : int -> bool t -> int meth
  method getUint32 : int -> float meth
  method getUint32_ : int -> bool t -> float meth
  method getFloat32 : int -> float meth
  method getFloat32_ : int -> bool t -> float meth
  method getFloat64 : int -> float meth
  method getFloat64_ : int -> bool t -> float meth
  method setInt8 : int -> int -> unit meth
  method setUint8 : int -> int -> unit meth
  method setInt16 : int -> int -> unit meth
  method setInt16_ : int -> int -> bool t -> unit meth
  method setUint16 : int -> int -> unit meth
  method setUint16_ : int -> int -> bool t -> unit meth
  method setInt32 : int -> int -> unit meth
  method setInt32_ : int -> int -> bool t -> unit meth
  method setUint32 : int -> float -> unit meth
  method setUint32_ : int -> float -> bool t -> unit meth
  method setFloat32 : int -> float -> unit meth
  method setFloat32_ : int -> float -> bool t -> unit meth
  method setFloat64 : int -> float -> unit meth
  method setFloat64_ : int -> float -> bool t -> unit meth
end

val dataView : (arrayBuffer t -> dataView t) constr
val dataView_inBuffer : (arrayBuffer t -> int -> int -> dataView t) constr


module Bigstring : sig
  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  val to_arrayBuffer : t -> arrayBuffer Js.t
  val of_arrayBuffer : arrayBuffer Js.t -> t
end


module String : sig
  val of_arrayBuffer : arrayBuffer Js.t -> string
end
