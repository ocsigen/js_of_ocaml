type t

val direct_calls_only : t -> Code.Var.t -> bool

val f : Code.program -> Global_flow.info -> t
