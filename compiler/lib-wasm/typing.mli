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

module Bigarray : sig
  type kind =
    | Float16
    | Float32
    | Float64
    | Int8_signed
    | Int8_unsigned
    | Int16_signed
    | Int16_unsigned
    | Int32
    | Int64
    | Int
    | Nativeint
    | Complex32
    | Complex64

  type layout =
    | C
    | Fortran

  type t =
    { kind : kind
    ; layout : layout
    }
end

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
  | Bigarray of Bigarray.t
  | Bot

val constant_type : Code.constant -> typ

val can_unbox_parameters : Call_graph_analysis.t -> Code.Var.t -> bool

val bigarray_element_type : Bigarray.kind -> typ

type t

val var_type : t -> Code.Var.t -> typ

val return_type : t -> Code.Var.t -> typ

val f :
     global_flow_state:Global_flow.state
  -> global_flow_info:Global_flow.info
  -> fun_info:Call_graph_analysis.t
  -> deadcode_sentinal:Code.Var.t
  -> Code.program
  -> t
