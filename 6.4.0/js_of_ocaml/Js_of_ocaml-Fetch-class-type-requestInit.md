
# Class type `Fetch.requestInit`

Initializer for [`request`](./Js_of_ocaml-Fetch-class-type-request.md) (and [`fetch_with_init`](./Js_of_ocaml-Fetch.md#val-fetch_with_init)). All fields are optional; create an empty record with [`empty_request_init`](./Js_of_ocaml-Fetch.md#val-empty_request_init) and populate the ones you need.

```ocaml
method _method : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                   Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method headers : headers Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method body : Js_of_ocaml__.Js.Unsafe.any Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method mode : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method credentials : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                       Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method cache : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                 Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method redirect : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method referrer : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                    Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method referrerPolicy : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                          Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method integrity : Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t
                     Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method keepalive : bool Js_of_ocaml__.Js.t Js_of_ocaml__.Js.writeonly_prop
```
```ocaml
method signal : Js_of_ocaml__.Abort.signal Js_of_ocaml__.Js.t
                  Js_of_ocaml__.Js.writeonly_prop
```