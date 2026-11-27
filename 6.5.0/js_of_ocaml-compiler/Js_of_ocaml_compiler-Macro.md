# Module `Js_of_ocaml_compiler.Macro`

```ocaml
val link_time_names : string list
```
The macros expanded only when `flags` is `true`, at link time

```ocaml
val f : flags:bool -> Javascript.program -> Javascript.program * bool
```
