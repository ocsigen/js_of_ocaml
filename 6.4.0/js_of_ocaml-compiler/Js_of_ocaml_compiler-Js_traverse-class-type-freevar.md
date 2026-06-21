
# Class type `Js_traverse.freevar`

```ocaml
inherit mapper
```
```ocaml
method merge_info : 'a -> unit
```
```ocaml
method merge_block_info : 'a -> unit
```
```ocaml
method record_block : block -> unit
```
```ocaml
method def_var : Js_of_ocaml_compiler.Javascript.ident -> unit
```
```ocaml
method def_local : Js_of_ocaml_compiler.Javascript.ident -> unit
```
```ocaml
method use_var : Js_of_ocaml_compiler.Javascript.ident -> unit
```
```ocaml
method state : t
```
```ocaml
method get_free : Js_of_ocaml_compiler.Javascript.IdentSet.t
```
```ocaml
method get_def : Js_of_ocaml_compiler.Javascript.IdentSet.t
```
```ocaml
method get_use : Js_of_ocaml_compiler.Javascript.IdentSet.t
```