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

(** Runtime environment axes, detected once. These values back the code emitted
    by {!Ppx_light_predicate.Predicate.reify} for [%expect.when] conditions. *)

(** ["js"] / ["wasm"] / ["native"] (from [Sys.backend_type]). *)
val backend : string

(** ["quickjs"] when [JSOO_TEST_ENGINE=quickjs], ["node"] otherwise. *)
val engine : string

(** [Sys.os_type]. *)
val os_type : string

(** The running OCaml version as a list of integers, e.g. [[5; 2; 0]]. *)
val ocaml_version : int list

(** Whether the compiler is the OxCaml flavour. *)
val oxcaml : bool

(** Lexicographic comparison of version components, like
    {!Ppx_light_predicate.Predicate.Version.compare}. *)
val version_compare : int list -> int list -> int

(** [tag_dropped t] is [true] when the standard inline-test tag [t] should cause
    a test to be skipped given the current axes (e.g. ["wasm-only"] is dropped
    unless [backend = "wasm"]). Unknown tags are never dropped here. *)
val tag_dropped : string -> bool
