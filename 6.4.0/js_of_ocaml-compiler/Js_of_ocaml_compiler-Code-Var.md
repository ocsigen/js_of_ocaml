
# Module `Code.Var`

```ocaml
type t
```
```ocaml
val print : Stdlib.Format.formatter -> t -> unit
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val idx : t -> int
```
```ocaml
val of_idx : int -> t
```
```ocaml
val fresh : unit -> t
```
```ocaml
val fresh_n : string -> t
```
```ocaml
val fork : t -> t
```
```ocaml
val count : unit -> int
```
```ocaml
val compare : t -> t -> int
```
```ocaml
val get_name : t -> string option
```
```ocaml
val set_name : t -> ?generated:bool -> string -> unit
```
```ocaml
val generated_name : t -> bool
```
```ocaml
val propagate_name : t -> t -> unit
```
```ocaml
val forget_generated_name : t -> unit
```
```ocaml
val reset : unit -> unit
```
```ocaml
module Set : Stdlib.Set.S with type elt = t
```
```ocaml
module Map : Stdlib.Map.S with type key = t
```
```ocaml
module Hashtbl : Js_of_ocaml_compiler.Stdlib.Hashtbl.S with type key = t
```
```ocaml
module Tbl : sig ... end
```
```ocaml
module ISet : sig ... end
```