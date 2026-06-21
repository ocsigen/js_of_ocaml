
# Module `Js_of_ocaml_compiler.Javascript`

```ocaml
module Num : sig ... end
```
```ocaml
module Label : sig ... end
```
```ocaml
type location = 
  | Pi of Parse_info.t
  | N
  | U
```
```ocaml
type identifier = Stdlib.Utf8_string.t
```
```ocaml
type ident_string = {
  name : identifier;
  var : Code.Var.t option;
  loc : location;
}
```
```ocaml
type early_error = {
  loc : Parse_info.t;
  reason : string option;
}
```
```ocaml
type ident = 
  | S of ident_string
  | V of Code.Var.t
```
```ocaml
and array_litteral = element_list
```
```ocaml
and element_list = element list
```
```ocaml
and element = 
  | ElementHole
  | Element of expression
  | ElementSpread of expression
```
```ocaml
and binop = 
  | Eq
  | StarEq
  | SlashEq
  | ModEq
  | PlusEq
  | MinusEq
  | LslEq
  | AsrEq
  | LsrEq
  | BandEq
  | BxorEq
  | BorEq
  | Or
  | OrEq
  | And
  | AndEq
  | Bor
  | Bxor
  | Band
  | EqEq
  | NotEq
  | EqEqEq
  | NotEqEq
  | Lt
  | Le
  | Gt
  | Ge
  | LtInt
  | LeInt
  | GtInt
  | GeInt
  | InstanceOf
  | In
  | Lsl
  | Lsr
  | Asr
  | Plus
  | Minus
  | Mul
  | Div
  | Mod
  | Exp
  | ExpEq
  | Coalesce
  | CoalesceEq
```
```ocaml
and unop = 
  | Not
  | Neg
  | Pl
  | Typeof
  | Void
  | Delete
  | Bnot
  | IncrA
  | DecrA
  | IncrB
  | DecrB
  | Await
```
```ocaml
and arguments = argument list
```
```ocaml
and argument = 
  | Arg of expression
  | ArgSpread of expression
```
```ocaml
and property_list = property list
```
```ocaml
and property = 
  | Property of property_name * expression
  | PropertySpread of expression
  | PropertyMethod of property_name * method_
  | CoverInitializedName of early_error * ident * initialiser
```
```ocaml
and method_ = 
  | MethodGet of function_declaration
  | MethodSet of function_declaration
  | Method of function_declaration
```
```ocaml
and property_name = 
  | PNI of identifier
  | PNS of Stdlib.Utf8_string.t
  | PNN of Num.t
  | PComputed of expression
```
```ocaml
and expression = 
  | ESeq of expression * expression
  | ECond of expression * expression * expression
  | EAssignTarget of assignment_target
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * access_kind * arguments * location
  | ECallTemplate of expression * template * location
  | EAccess of expression * access_kind * expression
  | EDot of expression * access_kind * identifier
  | EDotPrivate of expression * access_kind * identifier
  | ENew of expression * arguments option * location
  | EVar of ident
  | EFun of ident option * function_declaration
  | EClass of ident option * class_declaration
  | EArrow of function_declaration * bool * arrow_info
  | EStr of Stdlib.Utf8_string.t
  | ETemplate of template
  | EArr of array_litteral
  | EBool of bool
  | ENum of Num.t
  | EObj of property_list
  | ERegexp of string * string option
  | EYield of {
    delegate : bool;
    expr : expression option;
  }
  | EPrivName of identifier
  | CoverParenthesizedExpressionAndArrowParameterList of early_error
  | CoverCallExpressionAndAsyncArrowHead of early_error
```
```ocaml
and arrow_info = 
  | AUnknown
  | AUse_parent_fun_context
  | ANo_fun_context
```
```ocaml
and template = template_part list
```
```ocaml
and template_part = 
  | TStr of Stdlib.Utf8_string.t
  | TExp of expression
```
```ocaml
and access_kind = 
  | ANormal
  | ANullish
```
```ocaml
and statement = 
  | Block of block
  | Variable_statement of variable_declaration_kind * variable_declaration list
  | Function_declaration of ident * function_declaration
  | Class_declaration of ident * class_declaration
  | Empty_statement
  | Expression_statement of expression
  | If_statement of expression
    * statement * location
    * (statement * location) option
  | Do_while_statement of statement * location * expression
  | While_statement of expression * statement * location
  | For_statement of (expression option,
                     variable_declaration_kind * variable_declaration list)
                     either
    * expression option
    * expression option
    * statement * location
  | ForIn_statement of (expression, variable_declaration_kind * for_binding) either
    * expression
    * statement * location
  | ForOf_statement of (expression, variable_declaration_kind * for_binding) either
    * expression
    * statement * location
  | ForAwaitOf_statement of (expression, variable_declaration_kind * for_binding)
                            either
    * expression
    * statement * location
  | Continue_statement of Label.t option
  | Break_statement of Label.t option
  | Return_statement of expression option * location
  | With_statement of expression * statement * location
  | Labelled_statement of Label.t * statement * location
  | Switch_statement of expression
    * case_clause list
    * statement_list option
    * case_clause list
  | Throw_statement of expression
  | Try_statement of block
    * (formal_parameter option * block) option
    * block option
  | Debugger_statement
  | Import of import * Parse_info.t
  | Export of export * Parse_info.t
```
```ocaml
and ('left, 'right) either = 
  | Left of 'left
  | Right of 'right
```
```ocaml
and block = statement_list
```
```ocaml
and statement_list = (statement * location) list
```
```ocaml
and variable_declaration = 
  | DeclIdent of ident * initialiser option
  | DeclPattern of binding_pattern * initialiser
```
```ocaml
and variable_declaration_kind = 
  | Var
  | Let
  | Const
  | Using
  | AwaitUsing
```
```ocaml
and case_clause = expression * statement_list
```
```ocaml
and initialiser = expression * location
```
```ocaml
and function_declaration =
  function_kind * formal_parameter_list * function_body * location
```
```ocaml
and function_kind = {
  async : bool;
  generator : bool;
}
```
```ocaml
and decorator = expression
```
```ocaml
and class_declaration = {
  decorators : decorator list;
  extends : expression option;
  body : class_element list;
}
```
```ocaml
and class_element = 
  | CEMethod of decorator list * bool * class_element_name * method_
  | CEField of decorator list * bool * class_element_name * initialiser option
  | CEAccessor of decorator list * bool * class_element_name * initialiser option
  | CEStaticBLock of statement_list
```
```ocaml
and class_element_name = 
  | PropName of property_name
  | PrivName of identifier
```
```ocaml
and ('a, 'b) list_with_rest = {
  list : 'a list;
  rest : 'b option;
}
```
```ocaml
and formal_parameter_list = (formal_parameter, binding) list_with_rest
```
```ocaml
and formal_parameter = binding_element
```
```ocaml
and for_binding = binding
```
```ocaml
and binding_element = binding * initialiser option
```
```ocaml
and binding = 
  | BindingIdent of ident
  | BindingPattern of binding_pattern
```
```ocaml
and binding_pattern = 
  | ObjectBinding of (binding_property, ident) list_with_rest
  | ArrayBinding of (binding_element option, binding) list_with_rest
```
```ocaml
and object_target_elt = 
  | TargetPropertyId of ident_prop * initialiser option
  | TargetProperty of property_name * expression * initialiser option
  | TargetPropertySpread of expression
  | TargetPropertyMethod of property_name * method_
```
```ocaml
and array_target_elt = 
  | TargetElementId of ident * initialiser option
  | TargetElementHole
  | TargetElement of expression
  | TargetElementSpread of expression
```
```ocaml
and assignment_target = 
  | ObjectTarget of object_target_elt list
  | ArrayTarget of array_target_elt list
```
```ocaml
and ident_prop = 
  | Prop_and_ident of ident
```
```ocaml
and binding_property = 
  | Prop_binding of property_name * binding_element
  | Prop_ident of ident_prop * initialiser option
```
```ocaml
and function_body = statement_list
```
```ocaml
and program = statement_list
```
```ocaml
and export = 
  | ExportVar of variable_declaration_kind * variable_declaration list
  | ExportFun of ident * function_declaration
  | ExportClass of ident * class_declaration
  | ExportNames of (ident * Stdlib.Utf8_string.t) list
  | ExportDefaultFun of ident option * function_declaration
  | ExportDefaultClass of ident option * class_declaration
  | ExportDefaultExpression of expression
  | ExportFrom of {
    kind : export_from_kind;
    from : Stdlib.Utf8_string.t;
    withClause : withClause option;
  }
  | CoverExportFrom of early_error
```
```ocaml
and export_from_kind = 
  | Export_all of Stdlib.Utf8_string.t option
  | Export_names of (Stdlib.Utf8_string.t * Stdlib.Utf8_string.t) list
```
```ocaml
and import = {
  from : Stdlib.Utf8_string.t;
  kind : import_kind;
  withClause : withClause option;
}
```
```ocaml
and withClause = (Stdlib.Utf8_string.t * Stdlib.Utf8_string.t) list
```
```ocaml
and import_default = ident
```
```ocaml
and import_kind = 
  | DeferNamespace of ident
  | Namespace of import_default option * ident
  | Named of import_default option * (Stdlib.Utf8_string.t * ident) list
  | Default of import_default
  | SideEffect
```
```ocaml
and program_with_annots =
  (statement_list * (Js_token.Annot.t * Parse_info.t) list) list
```
```ocaml
val compare_ident : ident -> ident -> int
```
```ocaml
val is_ident : string -> bool
```
```ocaml
val is_ident' : Stdlib.Utf8_string.t -> bool
```
```ocaml
val ident_equal : ident -> ident -> bool
```
```ocaml
val ident : ?loc:location -> ?var:Code.Var.t -> identifier -> ident
```
```ocaml
val param : ?loc:location -> ?var:Code.Var.t -> identifier -> formal_parameter
```
```ocaml
val param' : ident -> formal_parameter
```
```ocaml
val ident_unsafe : ?loc:location -> ?var:Code.Var.t -> identifier -> ident
```
```ocaml
val bound_idents_of_params : formal_parameter_list -> ident list
```
```ocaml
val bound_idents_of_variable_declaration : variable_declaration -> ident list
```
```ocaml
val bound_idents_of_pattern : binding_pattern -> ident list
```
```ocaml
val bound_idents_of_binding : binding -> ident list
```
```ocaml
module IdentSet : Stdlib.Set.S with type elt = ident
```
```ocaml
module IdentMap : Stdlib.Map.S with type key = ident
```
```ocaml
val dot : expression -> identifier -> expression
```
```ocaml
val array : expression list -> expression
```
```ocaml
val call : expression -> expression list -> location -> expression
```
```ocaml
val variable_declaration : 
  ?kind:variable_declaration_kind ->
  (ident * initialiser) list ->
  statement
```
```ocaml
val list : 'a list -> ('a, _) list_with_rest
```
```ocaml
val early_error : ?reason:string -> Parse_info.t -> early_error
```
```ocaml
val fun_ : ident list -> statement_list -> location -> function_declaration
```
```ocaml
val assignment_target_of_expr : binop option -> expression -> expression
```
```ocaml
val location_equal : location -> location -> bool
```