# Module `Js_of_ocaml_compiler.Box_assigned`

```ocaml
val f : Code.program -> Code.program
```
Store in a mutable block the variables which are the target of an `Assign` instruction and are referenced from another function than the one that binds them. Such references are introduced by the CPS transformation, and closures may capture variables by value.
