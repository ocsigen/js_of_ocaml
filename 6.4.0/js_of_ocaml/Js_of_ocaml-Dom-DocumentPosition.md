
# Module `Dom.DocumentPosition`

```ocaml
type t = private int
```
```ocaml
type mask = private int
```
```ocaml
val disconnected : mask
```
```ocaml
val preceding : mask
```
```ocaml
val following : mask
```
```ocaml
val contains : mask
```
```ocaml
val contained_by : mask
```
```ocaml
val implementation_specific : mask
```
```ocaml
val has : t -> mask -> bool
```
```ocaml
val add : mask -> mask -> mask
```
```ocaml
val (+) : mask -> mask -> mask
```