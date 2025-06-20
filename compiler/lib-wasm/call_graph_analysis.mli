type t = { unambiguous_non_escaping : unit Code.Var.Hashtbl.t }

val f : Code.program -> Global_flow.info -> t
