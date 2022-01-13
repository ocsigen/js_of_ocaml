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
open! Import
open Js

include (
  Js_of_ocaml_runtime.Typed_array :
    module type of struct
      include Js_of_ocaml_runtime.Typed_array
    end
    with module String := Js_of_ocaml_runtime.Typed_array.String)

let arrayBuffer : (int -> arrayBuffer t) constr = Js.Unsafe.global##._ArrayBuffer

type int8Array = (int, Bigarray.int8_signed_elt) typedArray

type uint8Array = (int, Bigarray.int8_unsigned_elt) typedArray

type int16Array = (int, Bigarray.int16_signed_elt) typedArray

type uint16Array = (int, Bigarray.int16_unsigned_elt) typedArray

type int32Array = (int32, Bigarray.int32_elt) typedArray

type uint32Array = (int32, Bigarray.int32_elt) typedArray

type float32Array = (float, Bigarray.float32_elt) typedArray

type float64Array = (float, Bigarray.float64_elt) typedArray

let int8Array = Js.Unsafe.global##._Int8Array

let int8Array_fromArray = int8Array

let int8Array_fromTypedArray = int8Array

let int8Array_fromBuffer = int8Array

let int8Array_inBuffer = int8Array

let uint8Array = Js.Unsafe.global##._Uint8Array

let uint8Array_fromArray = uint8Array

let uint8Array_fromTypedArray = uint8Array

let uint8Array_fromBuffer = uint8Array

let uint8Array_inBuffer = uint8Array

let int16Array = Js.Unsafe.global##._Int16Array

let int16Array_fromArray = int16Array

let int16Array_fromTypedArray = int16Array

let int16Array_fromBuffer = int16Array

let int16Array_inBuffer = int16Array

let uint16Array = Js.Unsafe.global##._Uint16Array

let uint16Array_fromArray = uint16Array

let uint16Array_fromTypedArray = uint16Array

let uint16Array_fromBuffer = uint16Array

let uint16Array_inBuffer = uint16Array

let int32Array = Js.Unsafe.global##._Int32Array

let int32Array_fromArray = int32Array

let int32Array_fromTypedArray = int32Array

let int32Array_fromBuffer = int32Array

let int32Array_inBuffer = int32Array

let uint32Array = Js.Unsafe.global##._Uint32Array

let uint32Array_fromArray = uint32Array

let uint32Array_fromTypedArray = uint32Array

let uint32Array_fromBuffer = uint32Array

let uint32Array_inBuffer = uint32Array

let float32Array = Js.Unsafe.global##._Float32Array

let float32Array_fromArray = float32Array

let float32Array_fromTypedArray = float32Array

let float32Array_fromBuffer = float32Array

let float32Array_inBuffer = float32Array

let float64Array = Js.Unsafe.global##._Float64Array

let float64Array_fromArray = float64Array

let float64Array_fromTypedArray = float64Array

let float64Array_fromBuffer = float64Array

let float64Array_inBuffer = float64Array

class type dataView =
  object
    inherit arrayBufferView

    method getInt8 : int -> int meth

    method getUint8 : int -> int meth

    method getInt16 : int -> int meth

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

let dataView = Js.Unsafe.global##._DataView

let dataView_inBuffer = dataView

module String = struct
  include Js_of_ocaml_runtime.Typed_array.String

  let of_arrayBuffer ab =
    let uint8 = new%js uint8Array_fromBuffer ab in
    of_uint8Array uint8
end
