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

type graph

val get_edges : graph -> Code.Addr.t -> Code.Addr.Set.t

type control_flow_graph

val build_graph : Code.block Code.Addr.Map.t -> Code.Addr.t -> control_flow_graph

val dominator_tree : control_flow_graph -> graph

val is_loop_header : control_flow_graph -> Code.Addr.t -> bool

val is_merge_node : control_flow_graph -> Code.Addr.t -> bool

val is_backward : control_flow_graph -> Code.Addr.t -> Code.Addr.t -> bool

val sort_in_post_order : control_flow_graph -> Code.Addr.t list -> Code.Addr.t list

val blocks_in_reverse_post_order : control_flow_graph -> Code.Addr.t list
