
# Module `Javascript.Label`

```ocaml
type t = 
  | L of Code.Var.t
  | S of Stdlib.Utf8_string.t
```
```ocaml
val fresh : unit -> t
```
```ocaml
val of_string : Stdlib.Utf8_string.t -> t
```
```ocaml
val equal : t -> t -> bool
```