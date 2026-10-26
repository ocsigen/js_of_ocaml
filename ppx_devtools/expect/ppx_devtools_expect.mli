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

(** Inline expect tests, with conditional snapshots
    ([[%expect {| ... |} [@when COND]]]).

    The identifiers of [[@when]] predicates are axes. The predefined ones are
    defined in {!Ppx_devtools_expect_runtime.Axes}. *)

val register_axes : (string -> string option) -> unit
(** [register_axes f] adds axes: [f x] gives the fully qualified name of the
    runtime value of the identifier [x], or [None] if [f] does not know [x].
    Resolvers registered later take precedence, so they can override the
    predefined axes.

    It is meant to be called by a ppx library whose
    [(ppx_runtime_libraries ...)] provides the values, for instance:
    {[
      let () =
        Ppx_devtools_expect.register_axes (function
          | ("quickjs" | "node") as x -> Some ("My_axes." ^ x)
          | _ -> None)
    ]} *)
