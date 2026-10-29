# Module `Linear_scan.Builder`

Graph construction.

```ocaml
type t
```
```ocaml
val create : unit -> t
```
```ocaml
val reserve : t -> node
```
A fresh node, to be defined later with `set`.

```ocaml
val set : t -> node -> action -> node list -> unit
```
```ocaml
val add : t -> action -> node list -> node
```
```ocaml
val hint : t -> Code.Var.t -> Code.Var.t -> unit
```
`hint b x y`: `x` is a copy of `y`.

```ocaml
val try_block : t -> catch_entry:node -> body_entry:node -> unit
```
```ocaml
val finish : t -> entry:node -> graph
```
