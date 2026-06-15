
# Module `Js_of_ocaml_compiler.Unit_info`

```ocaml
type t = {
  provides : Global_name.Compunit_set.t;
  requires : Global_name.Compunit_set.t;
  primitives : string list;
  aliases : (string * string) list;
  force_link : bool;
  effects_without_cps : bool;
}
```
```ocaml
val of_cmo : Ocaml_compiler.Cmo_format.t -> t
```
```ocaml
val of_primitives : aliases:(string * string) list -> string list -> t
```
```ocaml
val union : t -> t -> t
```
```ocaml
val empty : t
```
```ocaml
val prefix : string
```
```ocaml
val to_string : t -> string
```
```ocaml
val parse : t -> string -> t option
```