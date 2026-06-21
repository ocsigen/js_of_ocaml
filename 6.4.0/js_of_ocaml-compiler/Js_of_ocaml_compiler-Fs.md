
# Module `Js_of_ocaml_compiler.Fs`

```ocaml
val find_in_path : string list -> string -> string option
```
```ocaml
val absolute_path : string -> string
```
```ocaml
val read_file : string -> string
```
```ocaml
val write_file : name:string -> contents:string -> unit
```
```ocaml
val gen_file : string -> (string -> 'a) -> 'a
```
```ocaml
val with_intermediate_file : string -> (string -> 'a) -> 'a
```