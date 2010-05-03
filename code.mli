
module Var : sig
  type t
  val print : Format.formatter -> t -> unit
  val idx : t -> int
  val to_string : t -> string

  type stream
  val make_stream : unit -> stream
  val next : stream -> t * stream

  val fresh : unit -> t

  val count : unit -> int

  val compare : t -> t -> int
end

type addr = int

type cont = addr * Var.t list

type prim =
    Vectlength
  | Array_get
  | C_call of string
  | Not | Neg | IsInt
  | Add | Sub | Mul | Div | Mod | And | Or | Xor | Lsl | Lsr | Asr
  | Eq | Neq | Lt | Le | Ult
  | Offset of int

type expr =
    Const of int
  | Apply of Var.t * Var.t list
  | Direct_apply of Var.t * Var.t list
  | Block of int * Var.t array
  | Field of Var.t * int
  | Closure of Var.t list * cont
  | Constant of Obj.t
  | Prim of prim * Var.t list
  | Variable of Var.t

type instr =
    Let of Var.t * expr
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type cond = IsTrue | CEq of int | CLt of int | CLe of int | CUlt of int

type last =
    Return of Var.t
  | Raise of Var.t
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * Var.t * cont * addr
  | Poptrap of cont

type block = Var.t list * instr list * last

type program = addr * block Util.IntMap.t * addr

type xinstr = Instr of instr | Last of last

val print_var_list : Format.formatter -> Var.t list -> unit
val print_instr : Format.formatter -> instr -> unit
val print_block : (Util.IntMap.key -> xinstr -> string) -> int -> block -> unit
val print_program : (Util.IntMap.key -> xinstr -> string) -> program -> unit

val dummy_cont : cont
val is_dummy_cont : cont -> bool

val fold_closures :
  program -> (Var.t option -> Var.t list -> cont -> 'd -> 'd) -> 'd -> 'd
val fold_children :
  block Util.IntMap.t -> addr  -> (addr -> 'c -> 'c) -> 'c -> 'c

val add_reserved_name : string -> unit
