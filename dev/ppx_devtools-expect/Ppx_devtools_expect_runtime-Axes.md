# Module `Ppx_devtools_expect_runtime.Axes`

Predefined axes of `[@when]` predicates.

Predefined axes of `[@when]` predicates. Further axes can be added with `Ppx_devtools_expect.register_axes`.

```ocaml
val backend : string
```
`"js"` / `"wasm"` / `"native"` (from `Sys.backend_type`).

```ocaml
val js : bool
```
`backend = "js"`

```ocaml
val wasm : bool
```
`backend = "wasm"`

```ocaml
val native : bool
```
`backend = "native"`

```ocaml
val os_type : string
```
`Sys.os_type`.

```ocaml
val win32 : bool
```
`os_type = "Win32"`

```ocaml
val unix : bool
```
`os_type = "Unix"`

```ocaml
val cygwin : bool
```
`os_type = "Cygwin"`

```ocaml
val ocaml_version : int list
```
The running OCaml version as a list of integers, e.g. `[5; 2; 0]`.

```ocaml
val oxcaml : bool
```
Whether the compiler is the OxCaml flavour.
