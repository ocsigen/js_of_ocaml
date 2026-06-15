
# Module `Js_of_ocaml_compiler.Build_info`

```ocaml
val string_of_effects_backend : Config.effects_backend -> string
```
```ocaml
val effects_backend_of_string : string -> Config.effects_backend
```
```ocaml
val effects_backend_of_string_result : 
  string ->
  (Config.effects_backend, string) Stdlib.result
```
```ocaml
val effects_backends_javascript : 
  (string * [ `Cps | `Double_translation | `Disabled ]) list
```
```ocaml
val effects_backends_wasm : 
  (string * [ `Jspi | `Cps | `Native | `Disabled ]) list
```
```ocaml
type config_key = 
  | Bool_key of {
    name : string;
    get : unit -> bool;
    set : bool -> unit;
    default : bool;
  }
  | Enum_key of {
    name : string;
    get : unit -> string;
    set : string -> unit;
    valid : string list;
  }
```
```ocaml
val config_key_name : config_key -> string
```
```ocaml
val config_keys : [ `JavaScript | `Wasm ] -> config_key list
```
```ocaml
val config_key_values : config_key -> string list
```
```ocaml
val get_non_default_values : config_key list -> (string * string) list
```
```ocaml
val set_values : config_key list -> (string * string) list -> unit
```
```ocaml
val to_config_string : (string * string) list -> string
```
```ocaml
val parse_config_string : string -> (string * string) list
```
```ocaml
type t
```
```ocaml
type kind = [ 
  | `Runtime
  | `Exe
  | `Cmo
  | `Cma
  | `Unknown
 ]
```
```ocaml
val create : kind -> t
```
```ocaml
val to_comment : t -> string
```
```ocaml
val parse_comment : string -> t option
```
```ocaml
val to_map : t -> string Stdlib.StringMap.t
```
```ocaml
val of_map : string Stdlib.StringMap.t -> t
```
```ocaml
val with_kind : t -> kind -> t
```
```ocaml
exception Incompatible_build_info of {
  key : string;
  first : string * string option;
  second : string * string option;
}
```
```ocaml
val merge : [ `JavaScript | `Wasm ] -> string -> t -> string -> t -> t
```
```ocaml
val kind : t -> kind
```
```ocaml
val configure : t -> unit
```