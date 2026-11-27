# Module `Code.Block_desc`

Introspection metadata attached to a block: the reserved header bits Introcaml associates to the block's descriptor (see `Introspect.Desc`).

```ocaml
type t
```
```ocaml
val none : t
```
The block has no descriptor.

```ocaml
val is_none : t -> bool
```
```ocaml
val of_int : int -> t
```
From the reserved header bits, `0` meaning [`none`](./#val-none).

```ocaml
val to_int : t -> int
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val print : Stdlib.Format.formatter -> t -> unit
```
