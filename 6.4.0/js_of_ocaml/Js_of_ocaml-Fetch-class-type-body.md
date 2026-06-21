
# Class type `Fetch.body`

The body-reader methods are Promise-typed — see [`Promise`](./Js_of_ocaml-Promise.md).

```ocaml
method bodyUsed : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.readonly_prop
```
```ocaml
method arrayBuffer : Js_of_ocaml__.Typed_array.arrayBuffer Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Promise.t
                       Js_of_ocaml__.Js.meth
```
```ocaml
method blob : Js_of_ocaml__.File.blob Js_of_ocaml__.Js.t
                Js_of_ocaml__.Promise.t
                Js_of_ocaml__.Js.meth
```
```ocaml
method json : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Promise.t
                Js_of_ocaml__.Js.meth
```
```ocaml
method text : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Promise.t
                Js_of_ocaml__.Js.meth
```
```ocaml
method formData : Js_of_ocaml__.Form.formData Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Promise.t
                    Js_of_ocaml__.Js.meth
```