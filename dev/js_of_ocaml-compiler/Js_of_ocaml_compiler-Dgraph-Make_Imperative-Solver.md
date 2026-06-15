
# Module `Make_Imperative.Solver`


## Parameters

```ocaml
module D : DOMAIN
```

## Signature

```ocaml
val f : NTbl.size -> t -> (D.t NTbl.t -> N.t -> D.t) -> D.t NTbl.t
```
```ocaml
val f' : 
  NTbl.size ->
  t ->
  (update:(children:bool -> N.t -> unit) -> D.t NTbl.t -> N.t -> D.t) ->
  D.t NTbl.t
```
```ocaml
val check : 
  t ->
  D.t NTbl.t ->
  (update:(children:bool -> N.t -> unit) -> D.t NTbl.t -> N.t -> D.t) ->
  (N.t -> D.t -> D.t -> unit) ->
  unit
```