
# Module `Sexp.Util`

```ocaml
val single : (t -> 'a) -> t list -> 'a
```
```ocaml
val mandatory : (t list -> 'a) -> t list option -> 'a
```
```ocaml
val string : t -> string
```
```ocaml
val bool : t -> bool
```
```ocaml
val assoc : t -> (string * t list) list
```
```ocaml
val member : string -> t -> t list option
```