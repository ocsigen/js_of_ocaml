# Module `Code.Constant`

```ocaml
type t = constant
```
```ocaml
val equal : t -> t -> bool
```
Structural equality; floats are compared bitwise

```ocaml
val ocaml_equal : t -> t -> bool option
```
Guaranteed equality in terms of OCaml `(=)`: if `constant_equal a b = Some v`, then `Poly.equal a b = v`. This is used for optimization purposes.
