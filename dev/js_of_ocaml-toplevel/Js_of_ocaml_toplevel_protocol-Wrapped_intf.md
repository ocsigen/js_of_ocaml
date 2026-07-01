
# Module `Js_of_ocaml_toplevel_protocol.Wrapped_intf`

```ocaml
type loc = {
  loc_start : int * int;
  loc_end : int * int;
}
```
```ocaml
type error = {
  msg : string;
  locs : loc list;
}
```
```ocaml
type warning = error
```
```ocaml
type 'a result = 
  | Success of 'a * warning list
  | Error of error * warning list
```
```ocaml
module type Wrapped = sig ... end
```