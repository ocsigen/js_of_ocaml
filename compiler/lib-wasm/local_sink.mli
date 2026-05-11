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

    When the [local.get x] is the only read of [x], it is replaced by
    [e] itself, and [x] is removed from [locals] if it is no longer
    accessed. *)

val f :
     effect_free:(Wasm_ast.expression -> bool)
  -> locals:(Wasm_ast.var * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list
(** [effect_free e] must hold only if evaluating [e] has no side effect
    and always falls through (we assume traps never happen). *)

val report_stats : unit -> unit
(** Emit aggregated time/stats logs accumulated across all [f] calls and
    reset the counters. Honours the [times] and [stats] debug flags. *)
