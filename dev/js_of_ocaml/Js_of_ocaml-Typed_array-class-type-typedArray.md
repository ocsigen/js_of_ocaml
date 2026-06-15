
# Class type `Typed_array.typedArray`

```ocaml
inherit arrayBufferView
```
```ocaml
method _BYTES_PER_ELEMENT_ : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method length : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method set_fromArray : 'a Js_of_ocaml__.Js.js_array Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method set_fromTypedArray : ('a, 'b, 'c) typedArray Js_of_ocaml__.Js.t ->
  int ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method subarray : int ->
  int ->
  ('a, 'b, 'c) typedArray Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method subarray_toEnd : int ->
  ('a, 'b, 'c) typedArray Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method slice : int ->
  int ->
  ('a, 'b, 'c) typedArray Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method slice_toEnd : int ->
  ('a, 'b, 'c) typedArray Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method _content_type_ : ('b * 'c) Js_of_ocaml__.Js.optdef
                          Js_of_ocaml__.Js.readonly_prop
```