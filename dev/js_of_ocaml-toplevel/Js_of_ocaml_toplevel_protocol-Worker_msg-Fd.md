
# Module `Worker_msg.Fd`

```ocaml
type t
```
```ocaml
val stdout : t
```
```ocaml
val stderr : t
```
```ocaml
val first_free : t
```
First descriptor available to `next`; `stdout` and `stderr` are reserved below it and persist across resets.

```ocaml
val next : t -> t
```
```ocaml
val is_reserved : t -> bool
```
```ocaml
val to_int : t -> int
```
For logging only.

```ocaml
module Map : Stdlib.Map.S with type key = t
```