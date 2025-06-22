module Integer : sig
  type kind =
    | Ref
    | Normalized
    | Unnormalized
end

type boxed_number =
  | Int32
  | Int64
  | Nativeint
  | Float

type boxed_status =
  | Boxed
  | Unboxed

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
  | Bot

val constant_type : Code.constant -> typ

val can_unbox_parameters : Call_graph_analysis.t -> Code.Var.t -> bool

type t

val var_type : t -> Code.Var.t -> typ

val f :
     global_flow_state:Global_flow.state
  -> global_flow_info:Global_flow.info
  -> fun_info:Call_graph_analysis.t
  -> deadcode_sentinal:Code.Var.t
  -> Code.program
  -> t
