
# Module `Js_of_ocaml_compiler.Pretty_print`

```ocaml
type t
```
```ocaml
type pos = {
  mutable p_line : int;
  mutable p_col : int;
}
```
```ocaml
val string : t -> string -> unit
```
```ocaml
val genbreak : t -> string -> int -> unit
```
```ocaml
val break : t -> unit
```
```ocaml
val break1 : t -> unit
```
```ocaml
val non_breaking_space : t -> unit
```
```ocaml
val space : ?indent:int -> t -> unit
```
```ocaml
val start_group : t -> int -> unit
```
```ocaml
val end_group : t -> unit
```
```ocaml
val newline : t -> unit
```
```ocaml
val to_out_channel : Stdlib.out_channel -> t
```
```ocaml
val to_buffer : Stdlib.Buffer.t -> t
```
```ocaml
val pos : t -> pos
```
```ocaml
val total : t -> int
```
```ocaml
val set_compact : t -> bool -> unit
```
```ocaml
val compact : t -> bool
```
```ocaml
val set_needed_space_function : t -> (char -> char -> bool) -> unit
```
```ocaml
val set_adjust_indentation_function : t -> (int -> int) -> unit
```
```ocaml
val check : t -> unit
```