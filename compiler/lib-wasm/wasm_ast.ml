(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type var = Code.Var.t

type packed_type =
  | I8
  | I16

type heap_type =
  | Func
  | Extern
  | Any
  | Eq
  | Struct
  | Array
  | I31
  | None_
  | Type of var

type ref_type =
  { nullable : bool
  ; typ : heap_type
  }

type value_type =
  | I32
  | I64
  | F32
  | F64
  | Ref of ref_type

type storage_type =
  | Value of value_type
  | Packed of packed_type

type 'typ mut_type =
  { mut : bool
  ; typ : 'typ
  }

type field_type = storage_type mut_type

type global_type = value_type mut_type

type func_type =
  { params : value_type list
  ; result : value_type list
  }

type str_type =
  | Struct of field_type list
  | Array of field_type
  | Func of func_type

type ('i32, 'i64, 'f32, 'f64) op =
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

type signage =
  | S
  | U

type int_un_op =
  | Clz
  | Ctz
  | Popcnt
  | Eqz
  | TruncSatF64 of signage
  | ReinterpretF

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
  | Convert of [ `I32 | `I64 ] * signage
  | ReinterpretI

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
  | Const of (int32, int64, float, float) op
  | UnOp of (int_un_op, int_un_op, float_un_op, float_un_op) op * expression
  | BinOp of
      (int_bin_op, int_bin_op, float_bin_op, float_bin_op) op * expression * expression
  | I32WrapI64 of expression
  | I64ExtendI32 of signage * expression
  | F32DemoteF64 of expression
  | F64PromoteF32 of expression
  | LocalGet of var
  | LocalTee of var * expression
  | GlobalGet of var
  | BlockExpr of func_type * instruction list
  | Call of var * expression list
  | Seq of instruction list * expression
  | Pop of value_type
  | RefFunc of var
  | Call_ref of var * expression * expression list
  | RefI31 of expression
  | I31Get of signage * expression
  | ArrayNew of var * expression * expression
  | ArrayNewFixed of var * expression list
  | ArrayNewData of var * var * expression * expression
  | ArrayGet of signage option * var * expression * expression
  | ArrayLen of expression
  | StructNew of var * expression list
  | StructGet of signage option * var * int * expression
  | RefCast of ref_type * expression
  | RefTest of ref_type * expression
  | RefEq of expression * expression
  | RefNull of heap_type
  | Br_on_cast of int * ref_type * ref_type * expression
  | Br_on_cast_fail of int * ref_type * ref_type * expression
  | Br_on_null of int * expression
  | IfExpr of value_type * expression * expression * expression
  | Try of func_type * instruction list * (var * int * value_type) list
  | ExternConvertAny of expression

and instruction =
  | Drop of expression
  | LocalSet of var * expression
  | GlobalSet of var * expression
  | Loop of func_type * instruction list
  | Block of func_type * instruction list
  | If of func_type * expression * instruction list * instruction list
  | Br_table of expression * int list * int
  | Br of int * expression option
  | Br_if of int * expression
  | Return of expression option
  | CallInstr of var * expression list
  | Nop
  | Push of expression
  | Throw of var * expression
  | Rethrow of int
  | ArraySet of var * expression * expression * expression
  | StructSet of var * int * expression * expression
  | Return_call of var * expression list
  | Return_call_ref of var * expression * expression list
  | Unreachable
  | Event of Parse_info.t  (** Location information *)

type import_desc =
  | Fun of func_type
  | Global of global_type
  | Tag of value_type

type type_field =
  { name : var
  ; typ : str_type
  ; supertype : var option
  ; final : bool
  }

type module_field =
  | Function of
      { name : var
      ; exported_name : string option
      ; typ : var option
      ; signature : func_type
      ; param_names : var list
      ; locals : (var * value_type) list
      ; body : instruction list
      }
  | Data of
      { name : var
      ; contents : string
      }
  | Global of
      { name : var
      ; exported_name : string option
      ; typ : global_type
      ; init : expression
      }
  | Tag of
      { name : var
      ; typ : value_type
      }
  | Import of
      { import_module : string
      ; import_name : string
      ; name : var
      ; desc : import_desc
      }
  | Type of type_field list
