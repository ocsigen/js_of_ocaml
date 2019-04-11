open Import

val parse_js : string -> J.program

val compile_ocaml_to_bytecode : string -> in_channel

val print_compiled_js : ?pretty:bool -> in_channel -> string

type find_result =
  { expressions : J.expression list
  ; statements : J.statement list
  ; var_decls : J.variable_declaration list }

val find_javascript :
     ?expression:(J.expression -> bool)
  -> ?statement:(J.statement -> bool)
  -> ?var_decl:(J.variable_declaration -> bool)
  -> J.program
  -> find_result

val expression_to_string : ?compact:bool -> J.expression -> string
