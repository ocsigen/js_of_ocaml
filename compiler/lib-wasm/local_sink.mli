(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026
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

(** Local.set sinking for the Wasm backend.

    For each [local.set x e] in a function, the pass tries to push the
    write forward to the first subsequent [local.get x], turning the
    [set]/[get] pair into a single [local.tee]. The sink is applied only
    when it does not cross another write to [x] (no intervening
    [local.set x] / [local.tee x]), does not cross a control-flow
    boundary, and does not reorder effects unsafely (conservatively, we
    require [e] or the intervening code to be effect-free).

    The pass does not touch the [locals] list — it only deletes some
    [local.set]s and turns some [local.get x]s into [local.tee x ...].
    Variables that become dead as a result are cleaned up later by
    [Var_coalescing]. *)

val f : Wasm_ast.instruction list -> Wasm_ast.instruction list
