
# Class type `Js_traverse.iterator`

```ocaml
method fun_decl : Js_of_ocaml_compiler.Javascript.function_declaration -> unit
```
```ocaml
method class_decl : Js_of_ocaml_compiler.Javascript.class_declaration -> unit
```
```ocaml
method class_element : Js_of_ocaml_compiler.Javascript.class_element -> unit
```
```ocaml
method early_error : Js_of_ocaml_compiler.Javascript.early_error -> unit
```
```ocaml
method expression : Js_of_ocaml_compiler.Javascript.expression -> unit
```
```ocaml
method expression_o : Js_of_ocaml_compiler.Javascript.expression option -> unit
```
```ocaml
method switch_case : Js_of_ocaml_compiler.Javascript.expression -> unit
```
```ocaml
method block : Js_of_ocaml_compiler.Javascript.statement_list -> unit
```
```ocaml
method initialiser : (Js_of_ocaml_compiler.Javascript.expression
                      * Js_of_ocaml_compiler.Javascript.location) ->
  unit
```
```ocaml
method initialiser_o : (Js_of_ocaml_compiler.Javascript.expression
                        * Js_of_ocaml_compiler.Javascript.location)
                         option ->
  unit
```
```ocaml
method for_binding : Js_of_ocaml_compiler.Javascript.variable_declaration_kind ->
  Js_of_ocaml_compiler.Javascript.for_binding ->
  unit
```
```ocaml
method variable_declaration : Js_of_ocaml_compiler.Javascript.variable_declaration_kind ->
  Js_of_ocaml_compiler.Javascript.variable_declaration ->
  unit
```
```ocaml
method statement : Js_of_ocaml_compiler.Javascript.statement -> unit
```
```ocaml
method statement_o : (Js_of_ocaml_compiler.Javascript.statement
                      * Js_of_ocaml_compiler.Javascript.location)
                       option ->
  unit
```
```ocaml
method statements : Js_of_ocaml_compiler.Javascript.statement_list -> unit
```
```ocaml
method formal_parameter_list : Js_of_ocaml_compiler.Javascript.formal_parameter_list ->
  unit
```
```ocaml
method ident : Js_of_ocaml_compiler.Javascript.ident -> unit
```
```ocaml
method program : Js_of_ocaml_compiler.Javascript.program -> unit
```
```ocaml
method function_body : Js_of_ocaml_compiler.Javascript.statement_list -> unit
```
```ocaml
method import : Js_of_ocaml_compiler.Javascript.import -> unit
```
```ocaml
method export : Js_of_ocaml_compiler.Javascript.export -> unit
```