
# Module `Worker_msg.Message_id`

```ocaml
type t
```
```ocaml
val first : t
```
```ocaml
val next : t -> t
```
```ocaml
val to_int : t -> int
```
For logging only.

```ocaml
module Map : Stdlib.Map.S with type key = t
```