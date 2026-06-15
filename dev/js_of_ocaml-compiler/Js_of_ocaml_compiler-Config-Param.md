
# Module `Config.Param`

This module contains parameters that may be modified through command-line flags.

```ocaml
val set : string -> string -> unit
```
```ocaml
val all : 
  unit ->
  (string * string * (string -> (unit, string) Stdlib.Result.t)) list
```
```ocaml
val switch_max_case : unit -> int
```
```ocaml
val inlining_limit : unit -> int
```
```ocaml
val tailcall_max_depth : unit -> int
```
```ocaml
val constant_max_depth : unit -> int
```
```ocaml
val merge_node_max : unit -> int
```
```ocaml
type tc = 
  | TcNone
  | TcTrampoline
```
```ocaml
val tailcall_optim : unit -> tc
```
```ocaml
val lambda_lifting_threshold : unit -> int
```
```ocaml
val lambda_lifting_baseline : unit -> int
```