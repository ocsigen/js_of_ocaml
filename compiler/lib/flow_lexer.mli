(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Loc : sig
  type t
end

module Lex_env : sig
  type t

  val create : Sedlexing.lexbuf -> t
end

module Lex_result : sig
  type t

  val token : t -> Js_token.t

  val loc : t -> Lexing.position * Lexing.position
end

val regexp : Lex_env.t -> Lex_env.t * Lex_result.t

val token : Lex_env.t -> Lex_env.t * Lex_result.t

val is_valid_identifier_name : string -> bool
