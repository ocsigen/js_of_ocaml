
# Module `Js_of_ocaml_compiler.Dgraph`

```ocaml
module type SET = sig ... end
```
```ocaml
module type MAP = sig ... end
```
```ocaml
module Make
  (N : sig ... end)
  (NSet : SET with type elt = N.t)
  (NMap : MAP with type key = N.t) : 
  sig ... end
```
```ocaml
module type ISet = sig ... end
```
```ocaml
module type Tbl = sig ... end
```
```ocaml
module Make_Imperative
  (N : sig ... end)
  (NSet : ISet with type elt = N.t)
  (NTbl : Tbl with type key = N.t) : 
  sig ... end
```
```ocaml
module type ACTION = sig ... end
```
```ocaml
module type DOMAIN = sig ... end
```
```ocaml
module Solver
  (N : sig ... end)
  (NSet : ISet with type elt = N.t)
  (NTbl : Tbl with type key = N.t)
  (A : ACTION)
  (D : DOMAIN) : 
  sig ... end
```