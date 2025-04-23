type number =
  | Int
  | Int32
  | Int64
  | Nativeint
  | Float

type typ =
  | Top
  | Number of number
  | Tuple of typ array
  | Bot

val constant_type : Code.constant -> typ

val f :
  state:Global_flow.state -> info:Global_flow.info -> Code.program -> typ Code.Var.Tbl.t
