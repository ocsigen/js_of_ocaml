type var = Code.Var.t

type symbol =
  | V of var
  | S of string

type value_type =
  | I32
  | I64
  | F64

type 'typ mut_type =
  { mut : bool
  ; typ : 'typ
  }

type global_type = value_type mut_type

type func_type =
  { params : value_type list
  ; result : value_type list
  }

type ('i32, 'i64, 'f64) op =
  | I32 of 'i32
  | I64 of 'i64
  | F64 of 'f64

type int_un_op =
  | Clz
  | Ctz
  | Popcnt
  | Eqz

type signage =
  | S
  | U

type int_bin_op =
  | Add
  | Sub
  | Mul
  | Div of signage
  | Rem of signage
  | And
  | Or
  | Xor
  | Shl
  | Shr of signage
  | Rotl
  | Rotr
  | Eq
  | Ne
  | Lt of signage
  | Gt of signage
  | Le of signage
  | Ge of signage

type float_un_op =
  | Neg
  | Abs
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Sqrt

type float_bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Min
  | Max
  | CopySign
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge

type memarg = int32

type expression =
  | Const of (int32, int64, float) op
  | ConstSym of symbol * int
  | UnOp of (int_un_op, int_un_op, float_un_op) op * expression
  | BinOp of (int_bin_op, int_bin_op, float_bin_op) op * expression * expression
  | Load of (memarg, memarg, memarg) op * expression
  | Load8 of signage * (memarg, memarg, memarg) op * expression
  | LocalGet of int
  | LocalTee of int * expression
  | GlobalGet of symbol
  | Call_indirect of func_type * expression * expression list
  | Call of symbol * expression list
  | MemoryGrow of int * expression
  | Seq of instruction list * expression
  | Pop of value_type

and instruction =
  | Drop of expression
  | Store of (memarg, memarg, memarg) op * expression * expression
  | Store8 of signage * (memarg, memarg, memarg) op * expression * expression
  | LocalSet of int * expression
  | GlobalSet of symbol * expression
  | Loop of func_type * instruction list
  | Block of func_type * instruction list
  | If of func_type * expression * instruction list * instruction list
  | Br_table of expression * int list * int
  | Br of int * expression option
  | Return of expression option
  | CallInstr of symbol * expression list
  | Nop
  | Push of expression

type import_desc = Fun of func_type

type module_field =
  | Function of
      { name : var
      ; exported_name : string option
      ; typ : func_type
      ; locals : value_type list
      ; body : instruction list
      }
  | Global of
      { name : symbol
      ; typ : global_type
      ; init : expression
      }
  | Import of
      { name : string
      ; desc : import_desc
      }
