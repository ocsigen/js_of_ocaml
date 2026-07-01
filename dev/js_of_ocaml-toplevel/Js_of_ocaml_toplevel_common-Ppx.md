
# Module `Js_of_ocaml_toplevel_common.Ppx`

One can add a ppx rewriter to a toplevel by registering it

```ocaml
  let init () =
    Compiler_libs.Ast_mapper.register "js_of_ocaml" (fun _ -> Ppx_js.mapper)
```
Helpers to embed PPX into the toplevel.

```ocaml
val preprocess_structure : Parsetree.structure -> Parsetree.structure
```
```ocaml
val preprocess_signature : Parsetree.signature -> Parsetree.signature
```
```ocaml
val preprocess_phrase : Parsetree.toplevel_phrase -> Parsetree.toplevel_phrase
```