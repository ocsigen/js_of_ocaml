
# Module `Wasm_of_ocaml_compiler.Zip`

```ocaml
type output
```
```ocaml
val open_out : string -> output
```
```ocaml
val add_entry : output -> name:string -> contents:string -> unit
```
```ocaml
val add_file : output -> name:string -> file:string -> unit
```
```ocaml
val close_out : output -> unit
```
```ocaml
type input
```
```ocaml
val open_in : string -> input
```
```ocaml
val with_open_in : string -> (input -> 'a) -> 'a
```
```ocaml
val has_entry : input -> name:string -> bool
```
```ocaml
val read_entry : input -> name:string -> string
```
```ocaml
val get_entry : input -> name:string -> Stdlib.in_channel * int * int * int32
```
```ocaml
val extract_file : input -> name:string -> file:string -> unit
```
```ocaml
val copy_file : input -> output -> src_name:string -> dst_name:string -> unit
```
```ocaml
val close_in : input -> unit
```