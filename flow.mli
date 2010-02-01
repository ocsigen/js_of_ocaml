
type 'a flat = Void | Known of 'a | Unknown

type v =
  | Blk of t list
  | Cst of int

and t = Code.Var.t flat * v flat

val approx_to_string : t -> string

val get_field : t -> int -> t

val get_const : t -> int option

val get_label : t -> Code.Var.t option

(****)

val f : Code.program -> Code.program * t array
