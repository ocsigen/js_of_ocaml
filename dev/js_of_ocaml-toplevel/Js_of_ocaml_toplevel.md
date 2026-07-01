
# Module `Js_of_ocaml_toplevel`

```ocaml
module Direct = Js_of_ocaml_toplevel_common.Direct
```
```ocaml
module Wrapped = Js_of_ocaml_toplevel_common.Wrapped
```
```ocaml
module Wrapped_intf = Js_of_ocaml_toplevel_protocol.Wrapped_intf
```
```ocaml
val initialize : unit -> unit
```
Initialize the toplevel environment (directives and environment). Must be called before evaluating with [`Direct`](./Js_of_ocaml_toplevel_common-Direct.md) or [`Wrapped`](./Js_of_ocaml_toplevel_common-Wrapped.md). Idempotent.

```ocaml
module JsooTop = Direct
```
```ocaml
module JsooTopWrapped = Wrapped
```
```ocaml
module JsooTopIntf = Wrapped_intf
```