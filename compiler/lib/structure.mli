(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Jérôme Vouillon
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

open! Stdlib
open Code

type graph

type t

val get_edges : graph -> Addr.t -> Addr.Set.t

val is_backward : t -> Addr.t -> Addr.t -> bool

val is_forward : t -> Addr.t -> Addr.t -> bool

val build_graph : block Addr.Map.t -> Addr.t -> t

val control_flow_graph : block Addr.Map.t -> Addr.t -> t
(** The control flow graph of a function, without the edges [build_graph] adds to
    limit the scope of loops and exception handlers *)

val dominator_tree : t -> graph

val immediate_dominators : t -> Addr.t Addr.Hashtbl.t
(** The immediate dominator of each block, except the entry block *)

val dominance_frontier : t -> Addr.t Addr.Hashtbl.t -> graph
(** [dominance_frontier g idom] computes the dominance frontier of each block, given
    the immediate dominators [idom] of graph [g] *)

val is_merge_node : t -> Addr.t -> bool

val is_loop_header : t -> Addr.t -> bool

val sort_in_post_order : t -> Addr.t list -> Addr.t list

val blocks_in_reverse_post_order : t -> Code.Addr.t list

val get_nodes : t -> Addr.Set.t

val norm : program -> program
(** [norm p] normalizes a program [p] to accommodate [Structure.build_graph] logic.
    In practice, it ensures that all loops have a predecessor block and allows to exit loops early. *)
