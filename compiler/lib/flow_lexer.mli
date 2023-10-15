(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Lex_mode : sig
  type t =
    | NORMAL
    | BACKQUOTE
    | REGEXP
end

module Parse_error : sig
  type t

  val to_string : t -> string
end

module Lex_env : sig
  type t

  val create : Sedlexing.lexbuf -> t
end

module Lex_result : sig
  type t

  val token : t -> Js_token.t

  val loc : t -> Loc.t

  val errors : t -> (Loc.t * Parse_error.t) list
end

val drop_line : Lex_env.t -> unit

val regexp : Lex_env.t -> Lex_env.t * Lex_result.t

val token : Lex_env.t -> Lex_env.t * Lex_result.t

val lex : Lex_env.t -> Lex_env.t * Lex_result.t

val is_valid_identifier_name : string -> bool
