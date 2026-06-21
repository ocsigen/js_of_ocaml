
# Module `Js_of_ocaml_compiler.Vlq64`

```ocaml
val in_alphabet : char -> bool
```
```ocaml
type input = {
  string : string;
  mutable pos : int;
  len : int;
}
```
```ocaml
val encode : Stdlib.Buffer.t -> int -> unit
```
```ocaml
val encode_l : Stdlib.Buffer.t -> int list -> unit
```
```ocaml
val decode : input -> int
```
```ocaml
val decode_l : string -> pos:int -> len:int -> int list
```