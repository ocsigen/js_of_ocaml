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

(** Reuse of casts for the Wasm backend.

    When a local is cast to the same type several times without being
    written in between, the result of the first cast is kept in a fresh
    local and reused instead of casting again. This matters for code run
    by V8's baseline compiler, which does not remove redundant casts.

    Returns the extended [locals] list and the rewritten body. *)

val f :
     locals:(Wasm_ast.var * Wasm_ast.value_type) list
  -> Wasm_ast.instruction list
  -> (Wasm_ast.var * Wasm_ast.value_type) list * Wasm_ast.instruction list

val report_stats : unit -> unit
(** Emit aggregated time/stats logs accumulated across all [f] calls and
    reset the counters. Honours the [times] and [stats] debug flags. *)
