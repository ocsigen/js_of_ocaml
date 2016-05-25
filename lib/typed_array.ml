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

open Js

class type arrayBuffer = object
  method byteLength : int readonly_prop
end

let arrayBuffer : (int -> arrayBuffer t) constr =
  Js.Unsafe.global ## _ArrayBuffer

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

let int8Array = Js.Unsafe.global ## _Int8Array
let int8Array_fromArray = int8Array
let int8Array_fromTypedArray = int8Array
let int8Array_fromBuffer = int8Array
let int8Array_inBuffer = int8Array

let uint8Array = Js.Unsafe.global ## _Uint8Array
let uint8Array_fromArray = uint8Array
let uint8Array_fromTypedArray = uint8Array
let uint8Array_fromBuffer = uint8Array
let uint8Array_inBuffer = uint8Array

let int16Array = Js.Unsafe.global ## _Int16Array
let int16Array_fromArray = int16Array
let int16Array_fromTypedArray = int16Array
let int16Array_fromBuffer = int16Array
let int16Array_inBuffer = int16Array

let uint16Array = Js.Unsafe.global ## _Uint16Array
let uint16Array_fromArray = uint16Array
let uint16Array_fromTypedArray = uint16Array
let uint16Array_fromBuffer = uint16Array
let uint16Array_inBuffer = uint16Array

let int32Array = Js.Unsafe.global ## _Int32Array
let int32Array_fromArray = int32Array
let int32Array_fromTypedArray = int32Array
let int32Array_fromBuffer = int32Array
let int32Array_inBuffer = int32Array

let uint32Array = Js.Unsafe.global ## _Uint32Array
let uint32Array_fromArray = uint32Array
let uint32Array_fromTypedArray = uint32Array
let uint32Array_fromBuffer = uint32Array
let uint32Array_inBuffer = uint32Array

let float32Array = Js.Unsafe.global ## _Float32Array
let float32Array_fromArray = float32Array
let float32Array_fromTypedArray = float32Array
let float32Array_fromBuffer = float32Array
let float32Array_inBuffer = float32Array

let float64Array = Js.Unsafe.global ## _Float64Array
let float64Array_fromArray = float64Array
let float64Array_fromTypedArray = float64Array
let float64Array_fromBuffer = float64Array
let float64Array_inBuffer = float64Array

let set : ('a,'b) typedArray t -> int -> 'a -> unit =
  fun a i v -> array_set (Unsafe.coerce a) i v
let get : ('a,'b) typedArray t -> int -> 'a optdef =
  fun a i -> Js.Unsafe.get a i
let unsafe_get : ('a,'b) typedArray t -> int -> 'a =
  fun a i -> Js.Unsafe.get a i

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

let dataView = Js.Unsafe.global ## _DataView
let dataView_inBuffer = dataView


module Bigstring = struct
  type t = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  external to_arrayBuffer : t -> arrayBuffer Js.t = "bigstring_to_array_buffer"
  external of_arrayBuffer : arrayBuffer Js.t -> t = "bigstring_of_array_buffer"
end

module String = struct
  external of_uint8Array : uint8Array Js.t -> string = "caml_string_of_array"
  let of_arrayBuffer ab =
    let uint8 = jsnew uint8Array_fromBuffer(ab) in
    of_uint8Array uint8
end
