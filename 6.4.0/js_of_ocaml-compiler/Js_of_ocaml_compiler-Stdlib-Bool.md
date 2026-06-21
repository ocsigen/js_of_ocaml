
# Module `Stdlib.Bool`

```ocaml
type t = bool = 
  | false
  | true
```
```ocaml
val not : bool -> bool
```
```ocaml
val (&&) : bool -> bool -> bool
```
```ocaml
val (||) : bool -> bool -> bool
```
```ocaml
val logand : bool -> bool -> bool
```
```ocaml
val logor : bool -> bool -> bool
```
```ocaml
val logxor : bool -> bool -> bool
```
```ocaml
val equal : bool -> bool -> bool
```
```ocaml
val compare : bool -> bool -> int
```
```ocaml
val to_int : bool -> int
```
```ocaml
val to_float : bool -> float
```
```ocaml
val to_string : bool -> string
```
```ocaml
val seeded_hash : int -> bool -> int
```
```ocaml
val hash : bool -> int
```
```ocaml
val (<>) : bool -> bool -> bool
```
```ocaml
val (=) : bool -> bool -> bool
```
```ocaml
val (>) : bool -> bool -> bool
```