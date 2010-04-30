
val program : (Code.Var.t -> Code.Var.t) -> Code.program -> Code.program
val instrs : (Code.Var.t -> Code.Var.t) -> Code.instr list -> Code.instr list
val last : (Code.Var.t -> Code.Var.t) -> Code.last -> Code.last

val from_array : Code.Var.t option array -> Code.Var.t -> Code.Var.t

module VarMap : Map.S with type key = Code.Var.t

val build_mapping : Code.Var.t list -> Code.Var.t list -> Code.Var.t VarMap.t

val from_map : Code.Var.t VarMap.t -> Code.Var.t -> Code.Var.t
