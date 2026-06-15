
# Module `Js_of_ocaml_compiler.Primitive`

```ocaml
val is_pure : string -> bool
```
```ocaml
val exists : string -> bool
```
```ocaml
type kind = [ 
  | `Pure
  | `Mutable
  | `Mutator
 ]
```
```ocaml
type kind_arg = [ 
  | `Shallow_const
  | `Object_literal
  | `Const
  | `Mutable
 ]
```
```ocaml
type condition = [ 
  | `If of string
  | `Ifnot of string
 ]
```
```ocaml
type t = [ 
  | `Requires of string list
  | `Provides of string * kind * kind_arg list option
  | `Version of ((int -> int -> bool) * string) list
  | `Weakdef
  | `Always
  | `Alias of string
  | `Deprecated of string
  | condition
 ]
```
```ocaml
val kind : string -> kind
```
```ocaml
val kind_args : string -> kind_arg list option
```
```ocaml
val register : string -> kind -> kind_arg list option -> int option -> unit
```
```ocaml
val arity : string -> int
```
```ocaml
val has_arity : string -> int -> bool
```
```ocaml
val alias : string -> string -> unit
```
```ocaml
val aliases : unit -> (string * string) list
```
```ocaml
val resolve : string -> string
```
```ocaml
val add_external : string -> unit
```
```ocaml
val get_external : unit -> Stdlib.StringSet.t
```
```ocaml
val need_named_value : string -> bool
```
```ocaml
val register_named_value : string -> unit
```
```ocaml
val reset : unit -> unit
```