
# Module `Sys.Config`

```ocaml
val use_js_string : unit -> bool
```
```ocaml
type effects_backend = [ 
  | `Disabled
  | `Cps
  | `Double_translation
  | `Jspi
 ]
```
```ocaml
val effects_ : unit -> string
```
```ocaml
val effects : unit -> [> `Cps | `Disabled | `Double_translation | `Jspi ]
```
```ocaml
val build_config : unit -> string
```