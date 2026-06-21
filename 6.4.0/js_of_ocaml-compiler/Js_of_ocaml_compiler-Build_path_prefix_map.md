
# Module `Js_of_ocaml_compiler.Build_path_prefix_map`

Rewrite paths for reproducible builds

**Warning:** this module is unstable and part of `compiler-libs`.

```ocaml
type path = string
```
```ocaml
type path_prefix = string
```
```ocaml
type error_message = string
```
```ocaml
type ('a, 'b) result = 
  | Ok of 'a
  | Error of 'b
```
```ocaml
val encode_prefix : path_prefix -> string
```
```ocaml
val decode_prefix : string -> (path_prefix, error_message) result
```
```ocaml
type pair = {
  target : path_prefix;
  source : path_prefix;
}
```
```ocaml
val encode_pair : pair -> string
```
```ocaml
val decode_pair : string -> (pair, error_message) result
```
```ocaml
type map = pair option list
```
```ocaml
val encode_map : map -> string
```
```ocaml
val decode_map : string -> (map, error_message) result
```
```ocaml
val rewrite_opt : map -> path -> path option
```
`rewrite_opt map path` tries to find a source in `map` that is a prefix of the input `path`. If it succeeds, it replaces this prefix with the corresponding target. If it fails, it just returns `None`.

```ocaml
val rewrite : map -> path -> path
```
```ocaml
val flip : map -> map
```
```ocaml
val get_build_path_prefix_map : unit -> map option
```