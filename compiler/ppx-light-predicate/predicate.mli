(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Shared predicate grammar used by [ppx_optcomp_light] (compile-time gating via
    [@if]/[@@if]/[@@@if]) and by [ppx_expect_light] (runtime gating via
    [%expect.when ...]).

    The grammar is a boolean expression over a few atoms combined with the usual
    comparison and boolean operators ([<= >= > < = <>], [&& || not]):

    - [ocaml_version] — the running OCaml version, compared against an [(maj, min,
      patch)] tuple.
    - [ast_version] — the ppxlib selected AST version (compile-time only).
    - [arch_sixtyfour] — whether the toolchain targets 64-bit ([Sys.word_size =
      64]); compile-time only.
    - [oxcaml] — whether the compiler is the OxCaml flavour.
    - [os_type] — [Sys.os_type] ("Unix" / "Win32" / "Cygwin").
    - [introspect] — whether the compiler was built with introspection support
      ([Sys.introspection_enabled]); compile-time only.
    - [backend] — ["native"] / ["js"] / ["wasm"]; runtime only (see {!reify}).

    plus the runtime-only shorthands [js], [wasm], [native], [win32], [unix],
    [cygwin].

    At runtime, other identifiers are extension axes: they are resolved in a
    module given to {!reify}. *)

open Ppxlib

module Version : sig
  type t

  val of_list : int list -> t

  val compare : t -> t -> int

  val current : t

  type extra_prefix =
    | Plus
    | Tilde

  val extra : (extra_prefix * string) option
end

type t

exception Invalid of Location.t

val parse : expression -> t
(** Parse a predicate expression into {!t}. Raises {!Invalid} on syntactically
    unexpected constructs. *)

val eval_compile_time : t -> bool
(** Evaluate the predicate now, at preprocessing time. Resolves [ocaml_version],
    [ast_version], [arch_sixtyfour], [oxcaml], [introspect] and [os_type]. Raises {!Invalid} on
    constructs that are not meaningful at compile time (e.g. [backend],
    the runtime shorthands or extension axes). *)

val reify : ?axes:string -> loc:Location.t -> t -> expression
(** Reify the predicate into an OCaml expression of type [bool] that evaluates it
    at runtime against [Ppx_expect_light_runtime.Axes]. Supports [ocaml_version],
    [os_type], [oxcaml], [backend] and the runtime shorthands.

    Any other identifier [x] is an extension axis, reified as [M.x] where [M] is
    the [axes] module path. Its expected type depends on how it is used: [bool]
    on its own, otherwise the type of what it is compared with ([string],
    [int], or [int list] for a version tuple).

    Raises {!Invalid} on constructs that are only meaningful at compile time
    (e.g. [ast_version]) and on unknown identifiers when [axes] is not given. *)
