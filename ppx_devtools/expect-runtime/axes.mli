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

(** Predefined axes of [[@when]] predicates. Further axes can be added with
    [Ppx_devtools_expect.register_axes]. *)

val backend : string
(** ["js"] / ["wasm"] / ["native"] (from [Sys.backend_type]). *)

val js : bool
(** [backend = "js"] *)

val wasm : bool
(** [backend = "wasm"] *)

val native : bool
(** [backend = "native"] *)

val os_type : string
(** [Sys.os_type]. *)

val win32 : bool
(** [os_type = "Win32"] *)

val unix : bool
(** [os_type = "Unix"] *)

val cygwin : bool
(** [os_type = "Cygwin"] *)

val ocaml_version : int list
(** The running OCaml version as a list of integers, e.g. [[5; 2; 0]]. *)

val oxcaml : bool
(** Whether the compiler is the OxCaml flavour. *)
