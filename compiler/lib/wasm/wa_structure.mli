type graph

val get_edges : graph -> Code.Addr.t -> Code.Addr.Set.t

type control_flow_graph

val build_graph : Code.block Code.Addr.Map.t -> Code.Addr.t -> control_flow_graph

val dominator_tree : control_flow_graph -> graph

val is_loop_header : control_flow_graph -> Code.Addr.t -> bool

val is_merge_node : control_flow_graph -> Code.Addr.t -> bool

val is_backward : control_flow_graph -> Code.Addr.t -> Code.Addr.t -> bool

val sort_in_post_order : control_flow_graph -> Code.Addr.t list -> Code.Addr.t list
