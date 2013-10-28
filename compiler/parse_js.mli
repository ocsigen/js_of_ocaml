



type lexer

val is_comment : Js_parser.token -> bool
val strip_comment : lexer -> lexer

val lexer_from_file : ?rm_comment:bool -> string -> lexer
val lexer_from_string : ?rm_comment:bool -> string -> lexer
val lexer_map : (Js_parser.token -> Js_parser.token) -> lexer -> lexer
val lexer_fold : ('a -> Js_parser.token -> 'a) -> 'a -> lexer -> 'a
val lexer_filter : (Js_parser.token -> bool) -> lexer -> lexer
val lexer_from_list : Js_parser.token list -> lexer

val parse : lexer -> Javascript.program

val info_of_tok : Js_parser.token -> Parse_info.t
