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
    - [backend] / [host_engine] / [target_engine] / [effects] — runtime only
      (see {!reify}). [host_engine] is the engine the test process runs on
      ("native" for a native test); [target_engine] is the engine a generated
      program runs under (from [JSOO_TEST_ENGINE]). [effects] is the js_of_ocaml
      [--effects] mode ("disabled" / "cps" / "double-translation" / "jspi").

    plus the runtime-only shorthands [js], [wasm], [native], [node], [quickjs],
    [wasi], [win32], [unix], [cygwin] ([node]/[quickjs]/[wasi] resolve to
    [host_engine]). *)

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
    [ast_version], [arch_sixtyfour], [oxcaml] and [os_type]. Raises {!Invalid} on
    constructs that are not meaningful at compile time (e.g. [backend],
    [host_engine], [target_engine], or the runtime shorthands). *)

val reify : loc:Location.t -> t -> expression
(** Reify the predicate into an OCaml expression of type [bool] that evaluates it
    at runtime against [Ppx_expect_light_runtime.Axes]. Supports [ocaml_version],
    [os_type], [oxcaml], [backend], [host_engine], [target_engine], [effects]
    and the runtime shorthands. Raises
    {!Invalid} on constructs that are only meaningful at compile time (e.g.
    [ast_version]). *)
