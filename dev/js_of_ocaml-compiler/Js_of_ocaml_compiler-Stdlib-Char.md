
# Module `Stdlib.Char`

```ocaml
type t = char
```
```ocaml
val code : char -> int
```
```ocaml
val chr : int -> char
```
```ocaml
val escaped : char -> string
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
module Ascii : sig ... end
```
```ocaml
val lowercase_ascii : char -> char
```
```ocaml
val uppercase_ascii : char -> char
```
```ocaml
val seeded_hash : int -> t -> int
```
```ocaml
val hash : t -> int
```
```ocaml
val unsafe_chr : int -> char
```
```ocaml
val (<) : char -> char -> bool
```
```ocaml
val (<=) : char -> char -> bool
```
```ocaml
val (<>) : char -> char -> bool
```
```ocaml
val (=) : char -> char -> bool
```
```ocaml
val (>) : char -> char -> bool
```
```ocaml
val (>=) : char -> char -> bool
```
```ocaml
val is_letter : char -> bool
```
```ocaml
val is_digit : char -> bool
```