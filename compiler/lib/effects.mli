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

type trampolined_calls = Code.Var.Set.t

type in_cps = Code.Var.Set.t

val f :
     flow_info:Global_flow.info
  -> live_vars:Deadcode.variable_uses
  -> Code.program
  -> Code.program * trampolined_calls * in_cps
(** Perform a partial CPS transform in order to translate a program that uses effect
    handler primitives to a program with only function calls, preserving the semantics.

    In addition, if double translation is enabled, some functions are defined in two
    versions (direct-style and CPS) and the generated program switches to CPS versions
    when entering the first effect handler, and back to direct style when exiting it. In
    addition to this dynamic behavior, the transform performs a static analysis to detect
    which functions do not need to be CPS-transformed. As a consequence, some functions
    become pairs of functions while others remain in a single version. This functions
    returns the set of call sites that require trampolining, as well as the set of call
    sites that require the callee to be a pair with a CPS component. *)
