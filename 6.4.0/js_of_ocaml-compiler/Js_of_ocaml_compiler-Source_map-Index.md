
# Module `Source_map.Index`

```ocaml
type section = {
  offset : Offset.t;
  map : Standard.t;
}
```
```ocaml
type t = {
  version : int;
  file : string option;
  sections : section list;
}
```