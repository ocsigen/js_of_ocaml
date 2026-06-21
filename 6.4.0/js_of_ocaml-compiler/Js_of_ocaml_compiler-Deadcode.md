
# Module `Js_of_ocaml_compiler.Deadcode`

```ocaml
type variable_uses = int array
```
```ocaml
val f : Pure_fun.t -> Code.program -> Code.program * variable_uses
```
```ocaml
val remove_unused_blocks : Code.program -> Code.program
```
```ocaml
val merge_blocks : Code.program -> Code.program
```