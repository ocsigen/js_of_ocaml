
# Module `Js_of_ocaml_compiler.Shape`

```ocaml
type desc = 
  | Top
  | Block of t list
  | Function of {
    arity : int;
    pure : bool;
    res : t;
  }
```
```ocaml
and t = {
  id : int;
  mutable desc : desc;
}
```
```ocaml
val top : t
```
```ocaml
val block : t list -> t
```
```ocaml
val funct : arity:int -> pure:bool -> res:t -> t
```
```ocaml
val proxy : unit -> t
```
```ocaml
module Set : Stdlib.Set.S with type elt = t
```
```ocaml
val to_string : t -> string
```
```ocaml
val of_string : string -> t
```
```ocaml
module Store : sig ... end
```
```ocaml
module State : sig ... end
```