
# Module `Code.Print`

```ocaml
type xinstr = 
  | Instr of instr
  | Last of last
```
```ocaml
val expr : Stdlib.Format.formatter -> expr -> unit
```
```ocaml
val constant : Stdlib.Format.formatter -> constant -> unit
```
```ocaml
val var_list : Stdlib.Format.formatter -> Var.t list -> unit
```
```ocaml
val instr : Stdlib.Format.formatter -> instr -> unit
```
```ocaml
val block : 
  Stdlib.Format.formatter ->
  (Addr.t -> xinstr -> string) ->
  int ->
  block ->
  unit
```
```ocaml
val program : 
  Stdlib.Format.formatter ->
  (Addr.t -> xinstr -> string) ->
  program ->
  unit
```
```ocaml
val last : Stdlib.Format.formatter -> last -> unit
```
```ocaml
val cont : Stdlib.Format.formatter -> cont -> unit
```