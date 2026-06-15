
# Module `Javascript.Num`

```ocaml
type t
```
Conversions

```ocaml
val of_string_unsafe : string -> t
```
```ocaml
val of_targetint : Targetint.t -> t
```
```ocaml
val of_float : float -> t
```
```ocaml
val to_string : t -> string
```
```ocaml
val to_targetint : t -> Targetint.t
```
```ocaml
val hash : t -> int
```
Predicates

```ocaml
val is_zero : t -> bool
```
```ocaml
val is_one : t -> bool
```
```ocaml
val is_neg : t -> bool
```
```ocaml
val equal : t -> t -> bool
```
Arithmetic

```ocaml
val add : t -> t -> t
```
```ocaml
val neg : t -> t
```