
open Javascript

val eplus_int : expression -> expression -> expression

val enot : expression -> expression

val source_elements : statement_list -> source_elements

val statement_list : statement_list -> statement_list

val block : statement_list -> statement

val if_statement : expression -> statement -> statement option -> statement
