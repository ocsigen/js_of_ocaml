
open Javascript

val source_elements : statement_list -> source_elements

val statement_list : statement_list -> statement_list

val block : statement_list -> statement

val if_statement : expression -> statement -> statement option -> statement
