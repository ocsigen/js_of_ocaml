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
  Js.Unsafe.variable "window.ArrayBuffer"

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
type uint32Array = (int, [`Uint32]) typedArray
type float32Array = (float, [`Float32]) typedArray
type float64Array = (float, [`Float64]) typedArray

let int8Array = Js.Unsafe.variable "window.Int8Array"
let int8Array_fromArray = int8Array
let int8Array_fromTypedArray = int8Array
let int8Array_fromBuffer = int8Array
let int8Array_inBuffer = int8Array

let int16Array = Js.Unsafe.variable "window.Int16Array"
let int16Array_fromArray = int16Array
let int16Array_fromTypedArray = int16Array
let int16Array_fromBuffer = int16Array
let int16Array_inBuffer = int16Array

let int32Array = Js.Unsafe.variable "window.Int32Array"
let int32Array_fromArray = int32Array
let int32Array_fromTypedArray = int32Array
let int32Array_fromBuffer = int32Array
let int32Array_inBuffer = int32Array

let float32Array = Js.Unsafe.variable "window.Float32Array"
let float32Array_fromArray = float32Array
let float32Array_fromTypedArray = float32Array
let float32Array_fromBuffer = float32Array
let float32Array_inBuffer = float32Array

let float64Array = Js.Unsafe.variable "window.Float64Array"
let float64Array_fromArray = float64Array
let float64Array_fromTypedArray = float64Array
let float64Array_fromBuffer = float64Array
let float64Array_inBuffer = float64Array

let set : ('a,'b) typedArray t -> int -> 'a -> unit =
  fun a i v -> array_set (Unsafe.coerce a) i v
let get : ('a,'b) typedArray t -> int -> 'a optdef =
  fun a i -> array_get (Unsafe.coerce a) i
