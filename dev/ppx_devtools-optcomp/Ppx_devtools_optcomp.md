# Module `Ppx_devtools_optcomp`

Minimal version of ppx\_optcomp It only support the following attribute

```ocaml
  [@if ocaml_version < (4,12,0)]
  [@if os_type <> "Win32"]
```
The supported predicates are `ocaml_version`, `ast_version`, `oxcaml`, `introspect`, `arch_sixtyfour` and `os_type` (a string, e.g. "Unix" / "Win32" / "Cygwin"), combined with the usual comparison and boolean operators (see [`Ppx_devtools_predicate.Predicate`](./Ppx_devtools_predicate-Predicate.md)). They can be placed on module (Pstr\_module), toplevel bindings (Pstr\_value, Pstr\_primitive) toplevel extensions, e.g. `let%expect_test "..." = ... [@@if ...]` (Pstr\_extension) pattern in case (pc\_lhs) and module in signature (Psig\_module)

For toplevel extensions to be gated we must run before context-free rewriters such as ppx\_devtools-expect expand them; this is achieved by registering the implementation transformation as a `Before` instrumentation (see registration at the end of the file).

```ocaml
module Predicate = Ppx_devtools_predicate.Predicate
```
```ocaml
val keep : 'a -> Ppxlib.Parsetree.attributes -> bool
```
```ocaml
val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
```
```ocaml
val filter_pattern : 
  Ppxlib.Parsetree.pattern ->
  Ppxlib.Parsetree.pattern option
```
```ocaml
val drop_attr : Ppxlib.Parsetree.attribute
```
```ocaml
val drop_str : Astlib__.Location.t -> Ppxlib.Parsetree.structure_item
```
```ocaml
val drop_sig : Astlib__.Location.t -> Ppxlib.Parsetree.signature_item
```
```ocaml
val is_if_attr : Ppxlib.Parsetree.attribute -> bool
```
```ocaml
val strip_if : 
  Ppxlib.Parsetree.attribute list ->
  Ppxlib.Parsetree.attribute list
```
```ocaml
val extension_if_attrs : 
  Ppxlib.Parsetree.extension ->
  Ppxlib.Parsetree.attribute list ->
  Ppxlib.Parsetree.attribute list
```
```ocaml
val strip_extension_if : 
  Ppxlib.Parsetree.extension ->
  Ppxlib.Parsetree.attribute list ->
  (string Ppxlib_ast__Versions.OCaml_502.Ast.Asttypes.loc
   * Ppxlib.Parsetree.payload)
  * Ppxlib.Parsetree.attribute list
```
```ocaml
val truncate_floating_str : 
  Ppxlib.Parsetree.structure_item list ->
  Ppxlib.Parsetree.structure_item list
```
```ocaml
val traverse : Ppxlib.Ast_traverse.map
```
