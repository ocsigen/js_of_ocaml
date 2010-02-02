
module Var : sig
  type t
  val print : Format.formatter -> t -> unit
  val idx : t -> int
  val to_string : t -> string

  type stream
  val make_stream : unit -> stream
  val next : stream -> t * stream

  val count : unit -> int

  val compare : t -> t -> int
end

type addr = int

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
  | Closure of Var.t list * addr
  | Constant of Obj.t
  | Prim of prim * Var.t list
  | Variable of Var.t

type instr =
    Let of Var.t * expr
  | Assign of Var.t * Var.t
  | Set_field of Var.t * int * Var.t
  | Offset_ref of Var.t * int
  | Array_set of Var.t * Var.t * Var.t

type cond = IsTrue | CEq of int | CLt of int | CLe of int | CUlt of int

type cont = addr * Var.t option

type last =
    Return of Var.t
  | Raise of Var.t
  | Stop
  | Branch of cont
  | Cond of cond * Var.t * cont * cont
  | Switch of Var.t * cont array * cont array
  | Pushtrap of cont * addr * cont
  | Poptrap of cont

type block = Var.t option * instr list * last

type program = addr * block Util.IntMap.t * addr

type xinstr = Instr of instr | Last of last

val print_instr : Format.formatter -> instr -> unit
val print_program : (Util.IntMap.key -> xinstr -> string) -> program -> unit

val dummy_cont : cont
