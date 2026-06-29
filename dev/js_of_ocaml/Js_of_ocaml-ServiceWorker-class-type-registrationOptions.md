
# Class type `ServiceWorker.registrationOptions`

Options for [`serviceWorkerContainer`](./Js_of_ocaml-ServiceWorker-class-type-serviceWorkerContainer.md)`##register_withOptions`. Create an empty record with [`empty_registration_options`](./Js_of_ocaml-ServiceWorker.md#val-empty_registration_options).

```ocaml
method scope : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method _type : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```
Either `"classic"` (default) or `"module"`.

```ocaml
method updateViaCache : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                          Js_of_ocaml__.Js.writeonly_prop
```
Either `"imports"` (default), `"all"` or `"none"`.
