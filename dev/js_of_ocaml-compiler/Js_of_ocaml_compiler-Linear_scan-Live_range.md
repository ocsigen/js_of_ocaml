# Module `Linear_scan.Live_range`

```ocaml
type interval = {
  start_pos : int;
  end_pos : int;
}
```
```ocaml
type t = {
  id : Code.Var.t;
  mutable ranges : interval list;
  mutable free : bool;
}
```
```ocaml
val print : Stdlib.Format.formatter -> t -> unit
```
