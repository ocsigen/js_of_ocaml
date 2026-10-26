(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

(** js_of_ocaml-specific axes of [[@when]] predicates. Test libraries using them
    are preprocessed with [(pps ... ppx_devtools-expect jsoo_test_axes_ppx)],
    which registers them, so that, for instance, [[@when not quickjs]] reads
    [Jsoo_test_axes.quickjs]. *)

val target_engine : string
(** The engine a generated program runs under, from [JSOO_TEST_ENGINE] (or
    [JSOO_ENGINE]); ["node"] by default. Use this for a native test that runs a
    program under another engine (e.g. tests-compiler). *)

val host_engine : string
(** The engine the current test process itself runs on: ["native"] for a native
    process, otherwise [target_engine] (the js/wasm process is hosted by it). *)

val node : bool
(** [host_engine = "node"] *)

val quickjs : bool
(** [host_engine = "quickjs"] *)

val bun : bool
(** [host_engine = "bun"] *)

val wasi : bool
(** [host_engine = "wasi"] *)

val effects : string
(** The [--effects] mode the test was compiled with: ["disabled"], ["cps"],
    ["double-translation"] or ["jspi"]; ["native"] for a native test. *)
