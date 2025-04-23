type boxed_number =
  | Int32
  | Int64
  | Nativeint
  | Float

type typ =
  | Top
  | Int
  | Number of boxed_number
  | Tuple of typ array
  | Bot

val constant_type : Code.constant -> typ

val f :
     state:Global_flow.state
  -> info:Global_flow.info
  -> deadcode_sentinal:Code.Var.t
  -> Code.program
  -> typ Code.Var.Tbl.t
