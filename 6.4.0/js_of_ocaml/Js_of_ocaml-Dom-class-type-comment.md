
# Class type `Dom.comment`

Specification of `Comment` objects

```ocaml
inherit node
```
```ocaml
method data : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.prop
```
```ocaml
method length : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method substringData : int ->
  int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.meth
```
```ocaml
method appendData : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method insertData : int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method deleteData : int -> int -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method replaceData : int ->
  int ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  unit Js_of_ocaml__.Js.meth
```