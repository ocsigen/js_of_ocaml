(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Olivier Nicole
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

(** Javascript-specific effect functions. *)

external assume_no_perform : (unit -> 'a) -> 'a = "caml_assume_no_perform"
(** Passing a function [f] as argument of `assume_no_perform` guarantees that,
    when compiling with `--effects=double-translation`, the direct-style
    version of [f] is called, which is faster than the CPS version. As a
    consequence, performing an effect in a transitive callee of [f] will raise
    `Effect.Unhandled`, regardless of any effect handlers installed before the
    call to `assume_no_perform`, unless a new effect handler was installed in
    the meantime.

    This behaviour is the same when double translation is disabled. *)
