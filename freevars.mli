
module VarSet : Set.S with type elt = Code.Var.t

val f : Code.program -> VarSet.t Util.IntMap.t
