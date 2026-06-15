
# Module `CSS.Angle`

```ocaml
type t = 
  | Deg of float
  | Grad of float
  | Rad of float
  | Turns of float
```
```ocaml
type js_t = private Js.js_string Js.t
```
```ocaml
val string_of_t : t -> string
```
```ocaml
val js : t -> js_t
```
```ocaml
val ml : js_t -> t
```