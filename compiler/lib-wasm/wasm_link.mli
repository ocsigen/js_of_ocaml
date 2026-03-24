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

type heaptype =
  | Func
  | Nofunc
  | Extern
  | Noextern
  | Any
  | Eq
  | I31
  | Struct
  | Array
  | None_
  | Type of int

type reftype =
  { nullable : bool
  ; typ : heaptype
  }

type valtype =
  | I32
  | I64
  | F32
  | F64
  | V128
  | Ref of reftype

type packedtype =
  | I8
  | I16

type storagetype =
  | Val of valtype
  | Packed of packedtype

type 'ty mut =
  { mut : bool
  ; typ : 'ty
  }

type fieldtype = storagetype mut

type comptype =
  | Func of
      { params : valtype array
      ; results : valtype array
      }
  | Struct of fieldtype array
  | Array of fieldtype

type subtype =
  { final : bool
  ; supertype : int option
  ; typ : comptype
  }

type rectype = subtype array

type limits =
  { min : int
  ; max : int option
  ; shared : bool
  ; index_type : [ `I32 | `I64 ]
  }

type tabletype =
  { limits : limits
  ; typ : reftype
  }

type importdesc =
  | Func of int
  | Table of tabletype
  | Mem of limits
  | Global of valtype mut
  | Tag of int

type import =
  { module_ : string
  ; name : string
  ; desc : importdesc
  }

type exportable =
  | Func
  | Table
  | Mem
  | Global
  | Tag

type 'a exportable_info =
  { mutable func : 'a
  ; mutable table : 'a
  ; mutable mem : 'a
  ; mutable global : 'a
  ; mutable tag : 'a
  }

val iter_exportable_info : (exportable -> 'a -> unit) -> 'a exportable_info -> unit

module Write : sig
  type st = { mutable type_index_count : int }

  val uint : Buffer.t -> int -> unit

  val name : Buffer.t -> string -> unit

  val types : Buffer.t -> rectype array -> st

  val imports : st -> Buffer.t -> import array -> unit

  val functions : Buffer.t -> int array -> unit

  val memories : Buffer.t -> limits array -> unit

  val tags : Buffer.t -> int array -> unit

  val export : Buffer.t -> exportable -> string -> int -> unit

  val start : Buffer.t -> int -> unit

  val data_count : Buffer.t -> int -> unit

  val nameassoc : Buffer.t -> int -> string -> unit

  val namemap : Buffer.t -> (int * string) array -> unit
end

module Read : sig
  val header : string

  val check_header : string -> string -> unit

  type ch =
    { buf : string
    ; mutable pos : int
    ; limit : int
    }

  val pos_in : ch -> int

  val seek_in : ch -> int -> unit

  val uint : ?n:int -> ch -> int

  type section =
    { id : int
    ; pos : int
    ; size : int
    }

  type index

  val index : ch -> index

  type types

  val create_types : unit -> types

  val types_last_index : types -> int

  val types_rev_list : types -> rectype list

  val add_rectype : types -> rectype -> int

  type t =
    { ch : ch
    ; mutable type_mapping : int array
    ; mutable type_index_count : int
    ; index : index
    }

  val open_in : string -> string -> t

  val find_section : t -> int -> bool

  val get_custom_section : t -> string -> section option

  val focus_on_custom_section : t -> string -> t

  val type_section : types -> t -> unit

  type interface =
    { imports : import array exportable_info
    ; exports : (string * int) list exportable_info
    }

  val interface : t -> interface

  val functions : t -> int array

  val memories : t -> limits array

  val tags : t -> int array

  val data_count : t -> int

  val start : t -> int option

  val name : ch -> string

  val namemap : t -> (int * string) array

  val globaltype : t -> ch -> valtype mut
end

module Scan : sig
  type maps =
    { typ : int array
    ; func : int array
    ; table : int array
    ; mem : int array
    ; global : int array
    ; elem : int array
    ; data : int array
    ; tag : int array
    }

  val default_maps : maps

  type resize_data = Wasm_source_map.resize_data =
    { mutable i : int
    ; mutable pos : int array
    ; mutable delta : int array
    }

  type position_data =
    { mutable i : int
    ; mutable pos : int array
    }

  val create_resize_data : unit -> resize_data

  val clear_resize_data : resize_data -> unit

  val push_resize : resize_data -> int -> int -> unit

  val create_position_data : unit -> position_data

  val clear_position_data : position_data -> unit

  val table_section :
    position_data -> maps -> Buffer.t -> string -> count:int -> int -> unit

  val global_section :
    position_data -> maps -> Buffer.t -> string -> count:int -> int -> unit

  val elem_section : maps -> Buffer.t -> string -> count:int -> int -> unit

  val data_section : maps -> Buffer.t -> string -> count:int -> int -> unit

  val func : resize_data -> maps -> Buffer.t -> string -> int -> unit
end

val add_section : out_channel -> id:int -> ?count:int -> Buffer.t -> unit

val add_subsection : Buffer.t -> id:int -> ?count:int -> Buffer.t -> unit

type input =
  { module_name : string
  ; file : string
  ; code : string option
  ; opt_source_map : Source_map.Standard.t option
  }

val f : input list -> output_file:string -> Source_map.t
