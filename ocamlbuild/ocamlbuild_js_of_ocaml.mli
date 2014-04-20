(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Jacques-Pascal Deplaix
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** {6 Dispatchers} *)

(** The main dispatcher

    The dispatcher should be used with {!Ocamlbuild_plugin.dispatch} as:
    [Ocamlbuild_plugin.dispatch Ocamlbuild_js_of_ocaml.dispatcher]

    Side note: {!Ocamlbuild_plugin.dispatch} should be used only once as
    it record only one function for an ocamlbuild module.
*)
val dispatcher : Ocamlbuild_plugin.hook -> unit

(** Same as {!dispatcher} followed by {!oasis_support} *)
val dispatcher_with_oasis_support :
  executables:string list ->
  Ocamlbuild_plugin.hook ->
  unit


(** {6 Low level functions} *)

(** Map each targets given as argument to ocamlbuild and replace each element
    that exists in [~executables] by its corresponding .js target.
*)
val oasis_support : executables:string list -> unit
