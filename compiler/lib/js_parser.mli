
(* The type of tokens. *)

type token = Js_token.token

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val standalone_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Javascript.expression)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Javascript.program)
