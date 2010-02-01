(*
   variable_declaration_list_no_in
   variable_declaration_no_in
   initialiser_no_in
...


*)
type foo = unit

(* A.3 Expressions *)

and array_litteral = element_list

and element_list = expression option list

and binop =
    Eq | StarEq | SlashEq | ModEq | PlusEq | MinusEq (*... XXX*)
  | Or | And | Bor | Bxor | Band
  | EqEq | NotEq | EqEqEq | NotEqEq
  | Lt | Le
  | Lsl | Lsr | Asr
  | Plus | Minus
  | Mul | Div | Mod

and unop = Not | Neg
(*XXX*)

and arguments = expression list

and property_name_and_value_list = (property_name * expression) list

and property_name =
    PNI of identifier
  | PNS of string
  | PNN of float

and expression =
    ESeq of expression * expression
  | ECond of expression * expression * expression
  | EBin of binop * expression * expression
  | EUn of unop * expression
  | ECall of expression * arguments
  | EAccess of expression * expression
  | EDot of expression * identifier
  | EVar of identifier
  | EFun of function_expression
  | EStr of string
  | EArr of array_litteral
  | ENum of float
  | EObj of property_name_and_value_list
  | EQuote of string

(****)

(* A.4 Statements *)

and statement =
    Block of block
  | Variable_statement of variable_declaration_list
(*
  | Empty_statement
*)
  | Expression_statement of expression
  | If_statement of expression * statement * statement option
(*
  | Iteration_statement
  | Continue_statement
  | Break_statement
*)
  | Return_statement of expression option
(*
  | With_statement
  | Labelled_statement
*)
  | Switch_statement of expression * case_clause list * statement_list option
  | Throw_statement of expression
  | Try_statement of block * (identifier * block) option * block option
(*
  | Debugger_statement
*)

and block = statement_list

and statement_list = statement list

and variable_statement = variable_declaration_list

and variable_declaration_list = variable_declaration list

and variable_declaration = identifier * initialiser option

and case_clause = expression * statement_list

and initialiser = expression

(*... *)

(****)

(* A.5 Functions and programs *)

and function_declaration =
  identifier * formal_parameter_list * function_body

and function_expression =
  identifier option * formal_parameter_list * function_body

and formal_parameter_list = identifier list

and function_body = source_elements

and program = source_elements

and source_elements = source_element list

and source_element =
    Statement of statement
  | Function_declaration of function_declaration

and identifier = string
