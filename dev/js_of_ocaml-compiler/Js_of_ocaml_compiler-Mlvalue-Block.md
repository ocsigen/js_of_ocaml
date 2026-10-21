# Module `Mlvalue.Block`

```ocaml
val make : tag:int -> args:Javascript.element list -> Javascript.expression
```
```ocaml
val tag : Javascript.expression -> Javascript.expression
```
```ocaml
val set_tag : 
  Javascript.expression ->
  Javascript.expression ->
  Javascript.expression
```
`set_tag e tag` updates the tag of block `e` in place. `e` may be evaluated more than once.

```ocaml
val field : Javascript.expression -> int -> Javascript.expression
```
