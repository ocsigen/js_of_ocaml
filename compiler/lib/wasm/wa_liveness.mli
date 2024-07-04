(* Wasm_of_ocaml compiler
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

type block_info =
  { initially_live : Code.Var.Set.t (* Live at start of block *)
  ; live_before_branch : Code.Var.Set.t
  }

type info =
  { instr : Code.Var.Set.t Code.Var.Map.t (* Live variables at spilling point *)
  ; block : block_info Code.Addr.Map.t
  }

val f :
     blocks:Code.block Code.Addr.Map.t
  -> context:Wa_code_generation.context
  -> closures:Wa_closure_conversion.closure Code.Var.Map.t
  -> domain:Code.Addr.Set.t
  -> env:Code.Var.t
  -> bound_vars:Code.Var.Set.t
  -> spilled_vars:Code.Var.Set.t
  -> pc:int
  -> info
