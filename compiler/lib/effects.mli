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
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type cps_calls = Code.Var.Set.t

type single_version_closures = Code.Var.Set.t

val f :
     Code.program * Deadcode.variable_uses
  -> Code.program * cps_calls * single_version_closures
(** Perform a partial CPS transform to translate the result effect handlers. In
    addition, functions are created in two versions (direct-style and CPS) and
    the generated program switches to CPS when entering the first effect
    handler, and back to direct style when exiting it. In addition to this
    dynamic behavior, the transforms performs a static analysis to detect which
    functions do not need to be CPS-transformed. As a consequence, some
    functions become pairs of functions while others remain in a single
    version. This functions returns the locations of CPS calls, and the set of
    closures that exist in a single version (which can be in direct style or
    CPS). *)
