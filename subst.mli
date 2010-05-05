
open Code

val program : (Var.t -> Var.t) -> program -> program
val instrs : (Var.t -> Var.t) -> instr list -> instr list
val last : (Var.t -> Var.t) -> last -> last

val from_array : Var.t option array -> Var.t -> Var.t

val build_mapping : Var.t list -> Var.t list -> Var.t VarMap.t

val from_map : Var.t VarMap.t -> Var.t -> Var.t
