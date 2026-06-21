
# Class type `Dom_html.history`

Browser history information

```ocaml
method length : int Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method state : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method go : int Js_of_ocaml__.Js.opt -> unit Js_of_ocaml__.Js.meth
```
```ocaml
method back : unit Js_of_ocaml__.Js.meth
```
```ocaml
method forward : unit Js_of_ocaml__.Js.meth
```
```ocaml
method pushState : 'a. 'a ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```
```ocaml
method replaceState : 'a. 'a ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t ->
  Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.opt ->
  unit Js_of_ocaml__.Js.meth
```