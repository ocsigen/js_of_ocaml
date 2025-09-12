type t

val direct_calls_only : t -> Code.Var.t -> bool

val raising_functions :
  Code.program -> Global_flow.info -> t -> (Code.Var.t -> bool) -> unit Code.Var.Hashtbl.t

val f : Code.program -> Global_flow.info -> t
