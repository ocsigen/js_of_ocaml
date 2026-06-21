
# Module `Js_of_ocaml.Typed_array`

Typed Array binding

```ocaml
class type  arrayBuffer = object ... end
```
```ocaml
val arrayBuffer : (int -> arrayBuffer Js.t) Js.constr
```
```ocaml
class type  arrayBufferView = object ... end
```
```ocaml
class type ['a, 'b, 'c] typedArray = object ... end
```
```ocaml
type int8Array = (int, int, Stdlib.Bigarray.int8_signed_elt) typedArray
```
```ocaml
type uint8Array = (int, int, Stdlib.Bigarray.int8_unsigned_elt) typedArray
```
```ocaml
type int16Array = (int, int, Stdlib.Bigarray.int16_signed_elt) typedArray
```
```ocaml
type uint16Array = (int, int, Stdlib.Bigarray.int16_unsigned_elt) typedArray
```
```ocaml
type int32Array =
  (Js.number_t, Stdlib.Int32.t, Stdlib.Bigarray.int32_elt) typedArray
```
```ocaml
type uint32Array =
  (Js.number_t, Stdlib.Int32.t, Stdlib.Bigarray.int32_elt) typedArray
```
```ocaml
type float32Array =
  (Js.number_t, float, Stdlib.Bigarray.float32_elt) typedArray
```
```ocaml
type float64Array =
  (Js.number_t, float, Stdlib.Bigarray.float64_elt) typedArray
```
```ocaml
type (_, _, _) kind = 
  | Int8_signed : (int, int, Stdlib.Bigarray.int8_signed_elt) kind
  | Int8_unsigned : (int, int, Stdlib.Bigarray.int8_unsigned_elt) kind
  | Int16_signed : (int, int, Stdlib.Bigarray.int16_signed_elt) kind
  | Int16_unsigned : (int, int, Stdlib.Bigarray.int16_unsigned_elt) kind
  | Int32_signed : (Js.number_t, Stdlib.Int32.t, Stdlib.Bigarray.int32_elt) kind
  | Int32_unsigned : (Js.number_t, Stdlib.Int32.t, Stdlib.Bigarray.int32_elt) kind
  | Float32 : (Js.number_t, float, Stdlib.Bigarray.float32_elt) kind
  | Float64 : (Js.number_t, float, Stdlib.Bigarray.float64_elt) kind
```
The first type parameter is the type of values that can be read and written in the [`typedArray`](./Js_of_ocaml-Typed_array-class-type-typedArray.md). The last two type parameters define the kind of bigarrays that can be converted to and from the [`typedArray`](./Js_of_ocaml-Typed_array-class-type-typedArray.md). See `Stdlib.Bigarray.kind`.

```ocaml
val kind : 
  ('typed_array, 'bigarray, 'elt) typedArray Js.t ->
  ('bigarray, 'elt) Stdlib.Bigarray.kind
```
```ocaml
val from_genarray : 
  ('typed_array, 'bigarray, 'elt) kind ->
  ('bigarray, 'elt, Stdlib.Bigarray.c_layout) Stdlib.Bigarray.Genarray.t ->
  ('typed_array, 'bigarray, 'elt) typedArray Js.t
```
```ocaml
val to_genarray : 
  ('typed_array, 'bigarray, 'elt) typedArray Js.t ->
  ('bigarray, 'elt, Stdlib.Bigarray.c_layout) Stdlib.Bigarray.Genarray.t
```
```ocaml
val int8Array : (int -> int8Array Js.t) Js.constr
```
```ocaml
val int8Array_fromArray : (int Js.js_array Js.t -> int8Array Js.t) Js.constr
```
```ocaml
val int8Array_fromTypedArray : (int8Array Js.t -> int8Array Js.t) Js.constr
```
```ocaml
val int8Array_fromBuffer : (arrayBuffer Js.t -> int8Array Js.t) Js.constr
```
```ocaml
val int8Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> int8Array Js.t) Js.constr
```
```ocaml
val uint8Array : (int -> uint8Array Js.t) Js.constr
```
```ocaml
val uint8Array_fromArray : (int Js.js_array Js.t -> uint8Array Js.t) Js.constr
```
```ocaml
val uint8Array_fromTypedArray : (uint8Array Js.t -> uint8Array Js.t) Js.constr
```
```ocaml
val uint8Array_fromBuffer : (arrayBuffer Js.t -> uint8Array Js.t) Js.constr
```
```ocaml
val uint8Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> uint8Array Js.t) Js.constr
```
```ocaml
val int16Array : (int -> int16Array Js.t) Js.constr
```
```ocaml
val int16Array_fromArray : (int Js.js_array Js.t -> int16Array Js.t) Js.constr
```
```ocaml
val int16Array_fromTypedArray : (int16Array Js.t -> int16Array Js.t) Js.constr
```
```ocaml
val int16Array_fromBuffer : (arrayBuffer Js.t -> int16Array Js.t) Js.constr
```
```ocaml
val int16Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> int16Array Js.t) Js.constr
```
```ocaml
val uint16Array : (int -> uint16Array Js.t) Js.constr
```
```ocaml
val uint16Array_fromArray : 
  (int Js.js_array Js.t -> uint16Array Js.t) Js.constr
```
```ocaml
val uint16Array_fromTypedArray : 
  (uint16Array Js.t -> uint16Array Js.t) Js.constr
```
```ocaml
val uint16Array_fromBuffer : (arrayBuffer Js.t -> uint16Array Js.t) Js.constr
```
```ocaml
val uint16Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> uint16Array Js.t) Js.constr
```
```ocaml
val int32Array : (int -> int32Array Js.t) Js.constr
```
```ocaml
val int32Array_fromArray : (int Js.js_array Js.t -> int32Array Js.t) Js.constr
```
```ocaml
val int32Array_fromTypedArray : (int32Array Js.t -> int32Array Js.t) Js.constr
```
```ocaml
val int32Array_fromBuffer : (arrayBuffer Js.t -> int32Array Js.t) Js.constr
```
```ocaml
val int32Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> int32Array Js.t) Js.constr
```
```ocaml
val uint32Array : (int -> uint32Array Js.t) Js.constr
```
```ocaml
val uint32Array_fromArray : 
  (Js.number_t Js.js_array Js.t -> uint32Array Js.t) Js.constr
```
```ocaml
val uint32Array_fromTypedArray : 
  (uint32Array Js.t -> uint32Array Js.t) Js.constr
```
```ocaml
val uint32Array_fromBuffer : (arrayBuffer Js.t -> uint32Array Js.t) Js.constr
```
```ocaml
val uint32Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> uint32Array Js.t) Js.constr
```
```ocaml
val float32Array : (int -> float32Array Js.t) Js.constr
```
```ocaml
val float32Array_fromArray : 
  (float Js.js_array Js.t -> float32Array Js.t) Js.constr
```
```ocaml
val float32Array_fromTypedArray : 
  (float32Array Js.t -> float32Array Js.t) Js.constr
```
```ocaml
val float32Array_fromBuffer : (arrayBuffer Js.t -> float32Array Js.t) Js.constr
```
```ocaml
val float32Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> float32Array Js.t) Js.constr
```
```ocaml
val float64Array : (int -> float64Array Js.t) Js.constr
```
```ocaml
val float64Array_fromArray : 
  (float Js.js_array Js.t -> float64Array Js.t) Js.constr
```
```ocaml
val float64Array_fromTypedArray : 
  (float64Array Js.t -> float64Array Js.t) Js.constr
```
```ocaml
val float64Array_fromBuffer : (arrayBuffer Js.t -> float64Array Js.t) Js.constr
```
```ocaml
val float64Array_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> float64Array Js.t) Js.constr
```
```ocaml
val set : ('a, _, _) typedArray Js.t -> int -> 'a -> unit
```
```ocaml
val get : ('a, _, _) typedArray Js.t -> int -> 'a Js.optdef
```
```ocaml
val unsafe_get : ('a, _, _) typedArray Js.t -> int -> 'a
```
```ocaml
class type  dataView = object ... end
```
```ocaml
val dataView : (arrayBuffer Js.t -> dataView Js.t) Js.constr
```
```ocaml
val dataView_inBuffer : 
  (arrayBuffer Js.t -> int -> int -> dataView Js.t) Js.constr
```
```ocaml
module Bigstring : sig ... end
```
```ocaml
module String : sig ... end
```
```ocaml
module Bytes : sig ... end
```