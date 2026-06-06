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

(** Liveness analysis and variable coalescing for the Wasm backend.

    Merges Wasm locals (including parameters) with disjoint live ranges so
    that a single local is reused. This shrinks the function's [locals]
    declaration and eliminates redundant [local.set]/[local.get] copies.

    Parameters are valid coalescing targets: a local whose live range does
    not overlap a parameter's can be rewritten to use the parameter's
    index. Two parameters never merge because they are all live at
    function entry. *)

val f :
     param_names:Code.Var.t list
  -> param_types:Wasm_ast.value_type list
  -> locals:(Code.Var.t * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> (Code.Var.t * Wasm_ast.value_type) list * Wasm_ast.instruction list

val report_stats : unit -> unit
(** Emit aggregated time/stats logs accumulated across all [f] calls and
    reset the counters. Honours the [times] and [stats] debug flags. *)
