
# Module `Js_of_ocaml_compiler.Warning`

```ocaml
type t = [ 
  | `Integer_overflow
  | `Missing_debug_event
  | `Missing_cmi
  | `Effect_handlers_without_effect_backend
  | `Missing_primitive
  | `Missing_define
  | `Missing_deps
  | `Free_variables_in_primitive
  | `Deprecated_joo_global_object
  | `Overriding_primitive
  | `Overriding_primitive_purity
  | `Deprecated_primitive
  | `Unused_js_variable
 ]
```
```ocaml
val all : t list
```
```ocaml
val name : t -> string
```
```ocaml
val parse : string -> t option
```
```ocaml
val enable : t -> unit
```
```ocaml
val disable : t -> unit
```
```ocaml
val enabled : t -> bool
```
```ocaml
val quiet : bool Stdlib.ref
```
```ocaml
val werror : bool Stdlib.ref
```
```ocaml
val warn : t -> ('a, Stdlib.Format.formatter, unit, unit) Stdlib.format4 -> 'a
```
```ocaml
val process_warnings : unit -> unit
```