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

module Bigarray : sig
  type kind =
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
    | Char
    | Float16

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
  | Number of boxed_number
  | Tuple of typ array
  | Bigarray of Bigarray.t
  | Bot

val constant_type : Code.constant -> typ

val f :
     state:Global_flow.state
  -> info:Global_flow.info
  -> deadcode_sentinal:Code.Var.t
  -> Code.program
  -> typ Code.Var.Tbl.t
