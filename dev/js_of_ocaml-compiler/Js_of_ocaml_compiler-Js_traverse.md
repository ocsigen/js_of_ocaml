
# Module `Js_of_ocaml_compiler.Js_traverse`

```ocaml
class type  mapper = object ... end
```
```ocaml
class type  iterator = object ... end
```
```ocaml
class map : mapper
```
```ocaml
class iter : iterator
```
```ocaml
type t = {
  use : Javascript.IdentSet.t;
  def_var : Javascript.IdentSet.t;
  def_local : Javascript.IdentSet.t;
}
```
```ocaml
type block = 
  | Catch of Javascript.formal_parameter
  | Params of Javascript.formal_parameter_list
  | Var_scope (* A scope that anchors var declarations but has no parameters: the program top level and class static initialization blocks. Its vars do not propagate to an enclosing scope. *)
  | Let_scope (* A lexical block: it anchors block-scoped (let/const/using) bindings. Its vars hoist to the nearest enclosing Params/Var_scope scope. *)
```
```ocaml
class type  freevar = object ... end
```
```ocaml
class free : freevar
```
```ocaml
val declared_names : Javascript.program -> Stdlib.StringSet.t
```
```ocaml
class fast_freevar : (string -> unit) -> iterator
```
```ocaml
type scope = 
  | Module
  | Script
  | Lexical_block
  | Fun_block of Javascript.ident option
```
```ocaml
class rename_variable : esm:bool -> object ... end
```
```ocaml
val share_constant : Javascript.program -> Javascript.program
```
```ocaml
class compact_vardecl : object ... end
```
```ocaml
class clean : mapper
```
```ocaml
class simpl : mapper
```