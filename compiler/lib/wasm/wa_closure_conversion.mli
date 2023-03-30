type closure =
  { functions : (Code.Var.t * int) list
  ; free_variables : Code.Var.t list
  }

val f : Code.program -> Code.program * closure Code.Var.Map.t
