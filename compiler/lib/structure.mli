open! Stdlib
open Code

type graph

type t

val get_edges : graph -> Addr.t -> Addr.Set.t

val is_backward : t -> Addr.t -> Addr.t -> bool

val is_forward : t -> Addr.t -> Addr.t -> bool

val build_graph : block Addr.Map.t -> Addr.t -> t

val dominator_tree : t -> graph

val is_merge_node : t -> Addr.t -> bool

val is_loop_header : t -> Addr.t -> bool

val sort_in_post_order : t -> Addr.t list -> Addr.t list

val blocks_in_reverse_post_order : t -> Code.Addr.t list

val get_nodes : t -> Addr.Set.t
