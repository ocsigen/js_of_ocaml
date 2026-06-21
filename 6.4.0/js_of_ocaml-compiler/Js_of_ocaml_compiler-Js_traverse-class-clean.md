
# Class `Js_traverse.clean`

```ocaml
method loc : Js_of_ocaml_compiler.Javascript.location ->
  Js_of_ocaml_compiler.Javascript.location
```
```ocaml
method parse_info : Js_of_ocaml_compiler.Parse_info.t ->
  Js_of_ocaml_compiler.Parse_info.t
```
```ocaml
method expression : Js_of_ocaml_compiler.Javascript.expression ->
  Js_of_ocaml_compiler.Javascript.expression
```
```ocaml
method expression_o : Js_of_ocaml_compiler.Javascript.expression option ->
  Js_of_ocaml_compiler.Javascript.expression option
```
```ocaml
method switch_case : Js_of_ocaml_compiler.Javascript.expression ->
  Js_of_ocaml_compiler.Javascript.expression
```
```ocaml
method block : Js_of_ocaml_compiler.Javascript.statement_list ->
  Js_of_ocaml_compiler.Javascript.statement_list
```
```ocaml
method fun_decl : Js_of_ocaml_compiler.Javascript.function_declaration ->
  Js_of_ocaml_compiler.Javascript.function_declaration
```
```ocaml
method class_decl : Js_of_ocaml_compiler.Javascript.class_declaration ->
  Js_of_ocaml_compiler.Javascript.class_declaration
```
```ocaml
method class_element : Js_of_ocaml_compiler.Javascript.class_element ->
  Js_of_ocaml_compiler.Javascript.class_element
```
```ocaml
method initialiser : (Js_of_ocaml_compiler.Javascript.expression
                      * Js_of_ocaml_compiler.Javascript.location) ->
  Js_of_ocaml_compiler.Javascript.expression
  * Js_of_ocaml_compiler.Javascript.location
```
```ocaml
method initialiser_o : (Js_of_ocaml_compiler.Javascript.expression
                        * Js_of_ocaml_compiler.Javascript.location)
                         option ->
  (Js_of_ocaml_compiler.Javascript.expression
   * Js_of_ocaml_compiler.Javascript.location)
    option
```
```ocaml
method for_binding : Js_of_ocaml_compiler.Javascript.variable_declaration_kind ->
  Js_of_ocaml_compiler.Javascript.for_binding ->
  Js_of_ocaml_compiler.Javascript.for_binding
```
```ocaml
method binding_property : Js_of_ocaml_compiler.Javascript.binding_property ->
  Js_of_ocaml_compiler.Javascript.binding_property
```
```ocaml
method variable_declaration : Js_of_ocaml_compiler.Javascript.variable_declaration_kind ->
  Js_of_ocaml_compiler.Javascript.variable_declaration ->
  Js_of_ocaml_compiler.Javascript.variable_declaration
```
```ocaml
method statement : Js_of_ocaml_compiler.Javascript.statement ->
  Js_of_ocaml_compiler.Javascript.statement
```
```ocaml
method statements : Js_of_ocaml_compiler.Javascript.statement_list ->
  Js_of_ocaml_compiler.Javascript.statement_list
```
```ocaml
method statement_o : (Js_of_ocaml_compiler.Javascript.statement
                      * Js_of_ocaml_compiler.Javascript.location)
                       option ->
  (Js_of_ocaml_compiler.Javascript.statement
   * Js_of_ocaml_compiler.Javascript.location)
    option
```
```ocaml
method ident : Js_of_ocaml_compiler.Javascript.ident ->
  Js_of_ocaml_compiler.Javascript.ident
```
```ocaml
method formal_parameter_list : Js_of_ocaml_compiler.Javascript.formal_parameter_list ->
  Js_of_ocaml_compiler.Javascript.formal_parameter_list
```
```ocaml
method program : Js_of_ocaml_compiler.Javascript.program ->
  Js_of_ocaml_compiler.Javascript.program
```
```ocaml
method function_body : Js_of_ocaml_compiler.Javascript.statement_list ->
  Js_of_ocaml_compiler.Javascript.statement_list
```
```ocaml
method import : Js_of_ocaml_compiler.Javascript.import ->
  Js_of_ocaml_compiler.Javascript.import
```
```ocaml
method export : Js_of_ocaml_compiler.Javascript.export ->
  Js_of_ocaml_compiler.Javascript.export
```