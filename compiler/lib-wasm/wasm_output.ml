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

open! Stdlib
open Wasm_ast

module Feature : sig
  type set

  val make : unit -> set

  val get : set -> string list

  type t

  val register : set -> string -> t

  val require : t -> unit

  val test : t -> bool
end = struct
  type t = string * bool ref

  type set = t list ref

  let make () = ref []

  let get l = !l |> List.filter ~f:(fun (_, b) -> !b) |> List.map ~f:fst

  let register l name =
    let f = name, ref false in
    l := f :: !l;
    f

  let require (_, b) = b := true

  let test (_, b) = !b
end

module Make (Output : sig
  type t

  val position : t -> int

  val seek : t -> int -> unit

  val byte : t -> int -> unit

  val string : t -> string -> unit

  val push_mapping : Source_map.map -> unit

  val get_file_index : string -> int
end) : sig
  val output_module : Output.t -> module_field list -> unit
end = struct
  let features = Feature.make ()

  let mutable_globals = Feature.register features "mutable-globals"

  let nontrapping_fptoint = Feature.register features "nontrapping-fptoint"

  let multivalue = Feature.register features "multivalue"

  let exception_handling = Feature.register features "exception-handling"

  let tail_call = Feature.register features "tail-call"

  let bulk_memory = Feature.register features "bulk-memory"

  let gc = Feature.register features "gc"

  let reference_types = Feature.register features "reference-types"

  let position = Output.position

  let seek = Output.seek

  let output_byte = Output.byte

  let output_string = Output.string

  let rec output_uint ch i =
    if i < 128
    then output_byte ch i
    else (
      output_byte ch (128 + (i land 127));
      output_uint ch (i lsr 7))

  let rec output_sint ch i =
    if i >= -64 && i < 64
    then output_byte ch (i land 127)
    else (
      output_byte ch (128 + (i land 127));
      output_sint ch (i asr 7))

  let output_sint32 ch i =
    if Int32.(i >= -64l && i < 64l)
    then
      let i = Int32.to_int i in
      if i >= 0 then output_byte ch i else output_byte ch (i + 128)
    else (
      output_byte ch (128 + (Int32.to_int i land 127));
      output_sint ch (Int32.to_int (Int32.shift_right i 7)))

  let rec output_sint64 ch i =
    if Int64.(i >= -64L && i < 64L)
    then
      let i = Int64.to_int i in
      if i >= 0 then output_byte ch i else output_byte ch (i + 128)
    else (
      output_byte ch (128 + (Int64.to_int i land 127));
      output_sint64 ch (Int64.shift_right i 7))

  let output_bytes32 ch v =
    let v = ref v in
    for _ = 0 to 3 do
      output_byte ch (Int32.to_int !v land 255);
      v := Int32.shift_right !v 8
    done

  let output_bytes64 ch v =
    let v = ref v in
    for _ = 0 to 7 do
      output_byte ch (Int64.to_int !v land 255);
      v := Int64.shift_right !v 8
    done

  let output_f32 ch f = output_bytes32 ch (Int32.bits_of_float f)

  let output_f64 ch f = output_bytes64 ch (Int64.bits_of_float f)

  let output_name ch name =
    output_uint ch (String.length name);
    output_string ch name

  let output_vec f ch l =
    output_uint ch (List.length l);
    List.iter ~f:(fun x -> f ch x) l

  let output_uint32_placeholder ch =
    let pos = position ch in
    output_string ch "\x80\x80\x80\x80\x00";
    pos

  let output_uint32_fixed ch ~pos v =
    let pos' = position ch in
    seek ch pos;
    let v = ref v in
    for _ = 0 to 3 do
      output_byte ch ((!v land 0x7f) + 128);
      v := !v lsr 7
    done;
    output_byte ch !v;
    seek ch pos'

  let with_size f ch x =
    let pos = output_uint32_placeholder ch in
    let res = f ch x in
    output_uint32_fixed ch ~pos (position ch - pos - 5);
    res

  (****)

  let output_heaptype type_names ch typ =
    match (typ : heap_type) with
    | None_ -> output_byte ch 0x71
    | Func -> output_byte ch 0x70
    | Extern -> output_byte ch 0x6F
    | Any -> output_byte ch 0x6E
    | Eq -> output_byte ch 0x6D
    | I31 -> output_byte ch 0x6C
    | Struct -> output_byte ch 0x6B
    | Array -> output_byte ch 0x6A
    | Type nm -> output_sint ch (Code.Var.Hashtbl.find type_names nm)

  let output_valtype type_names ch (typ : value_type) =
    match typ with
    | I32 -> output_byte ch 0x7F
    | I64 -> output_byte ch 0x7E
    | F32 -> output_byte ch 0x7D
    | F64 -> output_byte ch 0x7C
    | Ref { nullable; typ } ->
        output_byte ch (if nullable then 0x63 else 0x64);
        output_heaptype type_names ch typ

  let output_mut ch mut = output_byte ch (if mut then 0x01 else 0x00)

  let output_fieldtype type_names ch { mut; typ } =
    (match typ with
    | Value typ -> output_valtype type_names ch typ
    | Packed typ -> (
        match typ with
        | I8 -> output_byte ch 0x78
        | I16 -> output_byte ch 0x77));
    output_mut ch mut

  let output_functype type_names ch { params; result } =
    if List.length result > 1 then Feature.require multivalue;
    output_byte ch 0x60;
    output_vec (output_valtype type_names) ch params;
    output_vec (output_valtype type_names) ch result

  let output_globaltype type_names ch { typ; mut } =
    output_valtype type_names ch typ;
    output_mut ch mut

  let fold_types func_type explicit_definition acc fields =
    List.fold_left
      ~f:(fun acc field ->
        match field with
        | Function { typ = None; signature; _ } | Import { desc = Fun signature; _ } ->
            func_type acc signature
        | Import { desc = Tag typ; _ } -> func_type acc { params = [ typ ]; result = [] }
        | Type l -> explicit_definition acc l
        | Function { typ = Some _; _ }
        | Import { desc = Global _; _ }
        | Data _ | Global _ | Tag _ -> acc)
      ~init:acc
      fields

  let output_types ch fields =
    let count =
      let func_types = Poly.Hashtbl.create 16 in
      fold_types
        (fun count typ ->
          if Poly.Hashtbl.mem func_types typ
          then count
          else (
            Poly.Hashtbl.add func_types typ ();
            count + 1))
        (fun count _ -> count + 1)
        0
        fields
    in
    output_uint ch count;
    let func_types = Poly.Hashtbl.create 16 in
    let type_names = Code.Var.Hashtbl.create 16 in
    let _idx =
      fold_types
        (fun idx typ ->
          if Hashtbl.mem func_types typ
          then idx
          else (
            Hashtbl.add func_types typ idx;
            output_functype type_names ch typ;
            idx + 1))
        (fun idx l ->
          let len = List.length l in
          if List.length l > 1
          then (
            output_byte ch 0x4E;
            output_uint ch len);
          List.fold_left
            ~f:(fun idx { name; typ; supertype; final } ->
              Code.Var.Hashtbl.add type_names name idx;
              (match supertype, final with
              | None, true -> ()
              | None, false ->
                  output_byte ch 0x50;
                  output_byte ch 0
              | Some supertype, _ ->
                  output_byte ch (if final then 0X4F else 0x50);
                  output_byte ch 1;
                  output_uint ch (Code.Var.Hashtbl.find type_names supertype));
              (match typ with
              | Array field_type ->
                  output_byte ch 0x5E;
                  output_fieldtype type_names ch field_type
              | Struct l ->
                  output_byte ch 0x5F;
                  output_vec (output_fieldtype type_names) ch l
              | Func typ -> output_functype type_names ch typ);
              idx + 1)
            ~init:idx
            l)
        0
        fields
    in
    func_types, type_names

  let output_imports ch (func_types, type_names, fields) =
    let count =
      List.fold_left
        ~f:(fun count field ->
          match field with
          | Import _ -> count + 1
          | Function _ | Type _ | Data _ | Global _ | Tag _ -> count)
        ~init:0
        fields
    in
    output_uint ch count;
    let func_idx = ref 0 in
    let func_names = Code.Var.Hashtbl.create 16 in
    let global_idx = ref 0 in
    let global_names = Code.Var.Hashtbl.create 16 in
    let tag_idx = ref 0 in
    let tag_names = Code.Var.Hashtbl.create 16 in
    List.iter
      ~f:(fun field ->
        match field with
        | Function _ | Type _ | Data _ | Global _ | Tag _ -> ()
        | Import { import_module; import_name; name; desc } -> (
            output_name ch import_module;
            output_name ch import_name;
            match desc with
            | Fun typ ->
                output_byte ch 0x00;
                output_uint ch (Hashtbl.find func_types typ);
                Code.Var.Hashtbl.add func_names name !func_idx;
                incr func_idx
            | Global typ ->
                if typ.mut then Feature.require mutable_globals;
                output_byte ch 0x03;
                output_globaltype type_names ch typ;
                Code.Var.Hashtbl.add global_names name !global_idx;
                incr global_idx
            | Tag typ ->
                Feature.require exception_handling;
                output_byte ch 0x04;
                output_byte ch 0x00;
                output_uint ch (Hashtbl.find func_types { params = [ typ ]; result = [] });
                Code.Var.Hashtbl.add tag_names name !tag_idx;
                incr tag_idx))
      fields;
    !func_idx, func_names, !global_idx, global_names, !tag_idx, tag_names

  let output_functions ch (func_idx, func_names, func_types, fields) =
    let l =
      List.fold_left
        ~f:(fun acc field ->
          match field with
          | Function { signature; _ } -> signature :: acc
          | Type _ | Import _ | Data _ | Global _ | Tag _ -> acc)
        ~init:[]
        fields
    in
    let _ =
      List.fold_left
        ~f:(fun idx field ->
          match field with
          | Function { name; _ } ->
              Code.Var.Hashtbl.add func_names name idx;
              idx + 1
          | Type _ | Import _ | Data _ | Global _ | Tag _ -> idx)
        ~init:func_idx
        fields
    in
    output_vec
      (fun ch typ -> output_uint ch (Hashtbl.find func_types typ))
      ch
      (List.rev l)

  let int_un_op (arith, comp, trunc, reinterpret) ch op =
    match op with
    | Clz -> output_byte ch arith
    | Ctz -> output_byte ch (arith + 1)
    | Popcnt -> output_byte ch (arith + 2)
    | Eqz -> output_byte ch comp
    | TruncSatF64 signage ->
        Feature.require nontrapping_fptoint;
        output_byte ch 0xFC;
        output_byte
          ch
          (trunc
          +
          match signage with
          | S -> 0
          | U -> 1)
    | ReinterpretF -> output_byte ch reinterpret

  let int_bin_op (arith, comp) op =
    match (op : int_bin_op) with
    | Add -> arith + 3
    | Sub -> arith + 4
    | Mul -> arith + 5
    | Div S -> arith + 6
    | Div U -> arith + 7
    | Rem S -> arith + 8
    | Rem U -> arith + 9
    | And -> arith + 10
    | Or -> arith + 11
    | Xor -> arith + 12
    | Shl -> arith + 13
    | Shr S -> arith + 14
    | Shr U -> arith + 15
    | Rotl -> arith + 16
    | Rotr -> arith + 17
    | Eq -> comp + 1
    | Ne -> comp + 2
    | Lt S -> comp + 3
    | Lt U -> comp + 4
    | Gt S -> comp + 5
    | Gt U -> comp + 6
    | Le S -> comp + 7
    | Le U -> comp + 8
    | Ge S -> comp + 9
    | Ge U -> comp + 10

  let float_un_op (arith, convert, reinterpret) op =
    match op with
    | Abs -> arith
    | Neg -> arith + 1
    | Ceil -> arith + 2
    | Floor -> arith + 3
    | Trunc -> arith + 4
    | Nearest -> arith + 5
    | Sqrt -> arith + 6
    | Convert (size, signage) -> (
        convert
        + (match size with
          | `I32 -> 0
          | `I64 -> 2)
        +
        match signage with
        | S -> 0
        | U -> 1)
    | ReinterpretI -> reinterpret

  let float_bin_op (arith, comp) op =
    match op with
    | Add -> arith + 7
    | Sub -> arith + 8
    | Mul -> arith + 9
    | Div -> arith + 10
    | Min -> arith + 11
    | Max -> arith + 12
    | CopySign -> arith + 13
    | Eq -> comp
    | Ne -> comp + 1
    | Lt -> comp + 2
    | Gt -> comp + 3
    | Le -> comp + 4
    | Ge -> comp + 5

  let output_blocktype type_names ch typ =
    match typ with
    | { params = []; result = [] } -> output_byte ch 0x40
    | { params = []; result = [ typ ] } -> output_valtype type_names ch typ
    | _ -> assert false

  type st =
    { type_names : int Code.Var.Hashtbl.t
    ; func_names : int Code.Var.Hashtbl.t
    ; global_names : int Code.Var.Hashtbl.t
    ; data_names : int Code.Var.Hashtbl.t
    ; tag_names : int Code.Var.Hashtbl.t
    ; local_names : int Code.Var.Hashtbl.t Code.Var.Hashtbl.t
    ; current_local_names : int Code.Var.Hashtbl.t
    }

  let last_event = ref None

  let push_no_event ch =
    if Option.is_some !last_event
    then (
      Output.push_mapping (Source_map.Gen { gen_line = 1; gen_col = position ch });
      last_event := None)

  let push_event ch ~src ~line ~col =
    match !last_event with
    | Some (src', line', col') when col = col' && line = line' && String.equal src src' ->
        ()
    | _ ->
        Output.push_mapping
          (Source_map.Gen_Ori
             { gen_line = 1
             ; gen_col = position ch
             ; ori_source = Output.get_file_index src
             ; ori_line = line
             ; ori_col = col
             });
        last_event := Some (src, line, col)

  let rec output_expression st ch e =
    match e with
    | Const c -> (
        match c with
        | I32 d ->
            output_byte ch 0x41;
            output_sint32 ch d
        | I64 d ->
            output_byte ch 0x42;
            output_sint64 ch d
        | F32 d ->
            output_byte ch 0x43;
            output_f32 ch d
        | F64 d ->
            output_byte ch 0x44;
            output_f64 ch d)
    | UnOp (op, e') -> (
        output_expression st ch e';
        match op with
        | I32 op -> int_un_op (0x67, 0x45, 2, 0xBC) ch op
        | I64 op -> int_un_op (0x79, 0x50, 6, 0xBD) ch op
        | F32 op -> output_byte ch (float_un_op (0x8B, 0xB2, 0xBE) op)
        | F64 op -> output_byte ch (float_un_op (0x99, 0xB7, 0xBF) op))
    | BinOp (op, e', e'') -> (
        output_expression st ch e';
        output_expression st ch e'';
        match op with
        | I32 op -> output_byte ch (int_bin_op (0x67, 0x45) op)
        | I64 op -> output_byte ch (int_bin_op (0x79, 0x50) op)
        | F32 op -> output_byte ch (float_bin_op (0x8B, 0x5B) op)
        | F64 op -> output_byte ch (float_bin_op (0x99, 0x61) op))
    | I32WrapI64 e' ->
        output_expression st ch e';
        output_byte ch 0xA7
    | I64ExtendI32 (S, e') ->
        output_expression st ch e';
        output_byte ch 0xAC
    | I64ExtendI32 (U, e') ->
        output_expression st ch e';
        output_byte ch 0xAD
    | F32DemoteF64 e' ->
        output_expression st ch e';
        output_byte ch 0xB6
    | F64PromoteF32 e' ->
        output_expression st ch e';
        output_byte ch 0xBB
    | LocalGet i ->
        output_byte ch 0x20;
        output_uint ch (Code.Var.Hashtbl.find st.current_local_names i)
    | LocalTee (i, e') ->
        output_expression st ch e';
        output_byte ch 0x22;
        output_uint ch (Code.Var.Hashtbl.find st.current_local_names i)
    | GlobalGet g ->
        output_byte ch 0x23;
        output_uint ch (Code.Var.Hashtbl.find st.global_names g)
    | BlockExpr (typ, l) ->
        output_byte ch 0x02;
        output_blocktype st.type_names ch typ;
        List.iter ~f:(fun i' -> output_instruction st ch i') l;
        output_byte ch 0x0B
    | Call (f, l) ->
        List.iter ~f:(fun e' -> output_expression st ch e') l;
        output_byte ch 0x10;
        output_uint ch (Code.Var.Hashtbl.find st.func_names f)
    | Seq _ -> assert false
    | Pop _ -> ()
    | RefFunc f ->
        Feature.require reference_types;
        output_byte ch 0xD2;
        output_uint ch (Code.Var.Hashtbl.find st.func_names f)
    | Call_ref (typ, e', l) ->
        Feature.require gc;
        List.iter ~f:(fun e' -> output_expression st ch e') l;
        output_expression st ch e';
        output_byte ch 0x14;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | RefI31 e' ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch 0x1C
    | I31Get (s, e') -> (
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        match s with
        | S -> output_byte ch 0x1D
        | U -> output_byte ch 0x1E)
    | ArrayNew (typ, e', e'') ->
        Feature.require gc;
        output_expression st ch e';
        output_expression st ch e'';
        output_byte ch 0xFB;
        output_byte ch 6;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | ArrayNewFixed (typ, l) ->
        Feature.require gc;
        List.iter ~f:(fun e' -> output_expression st ch e') l;
        output_byte ch 0xFB;
        output_byte ch 8;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ);
        output_uint ch (List.length l)
    | ArrayNewData (typ, data, e', e'') ->
        Feature.require gc;
        output_expression st ch e';
        output_expression st ch e'';
        output_byte ch 0xFB;
        output_byte ch 9;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ);
        output_uint ch (Code.Var.Hashtbl.find st.data_names data)
    | ArrayGet (signage, typ, e', e'') ->
        Feature.require gc;
        output_expression st ch e';
        output_expression st ch e'';
        output_byte ch 0xFB;
        output_byte
          ch
          (match signage with
          | None -> 0x0B
          | Some S -> 0x0C
          | Some U -> 0x0D);
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | ArrayLen e' ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch 0x0F
    | StructNew (typ, l) ->
        Feature.require gc;
        List.iter ~f:(fun e' -> output_expression st ch e') l;
        output_byte ch 0xFB;
        output_byte ch 0;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | StructGet (signage, typ, idx, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte
          ch
          (match signage with
          | None -> 0x02
          | Some S -> 0x03
          | Some U -> 0x04);
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ);
        output_uint ch idx
    | RefCast ({ typ; nullable }, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch (if nullable then 0x17 else 0x16);
        output_heaptype st.type_names ch typ
    | RefTest ({ typ; nullable }, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch (if nullable then 0x15 else 0x14);
        output_heaptype st.type_names ch typ
    | RefEq (e', e'') ->
        Feature.require gc;
        output_expression st ch e';
        output_expression st ch e'';
        output_byte ch 0xD3
    | RefNull typ ->
        Feature.require reference_types;
        output_byte ch 0xD0;
        output_heaptype st.type_names ch typ
    | Br_on_cast (i, typ1, typ2, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch 0x18;
        output_byte ch ((if typ1.nullable then 1 else 0) + if typ2.nullable then 2 else 0);
        output_uint ch i;
        output_heaptype st.type_names ch typ1.typ;
        output_heaptype st.type_names ch typ2.typ
    | Br_on_cast_fail (i, typ1, typ2, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch 0x19;
        output_byte ch ((if typ1.nullable then 1 else 0) + if typ2.nullable then 2 else 0);
        output_uint ch i;
        output_heaptype st.type_names ch typ1.typ;
        output_heaptype st.type_names ch typ2.typ
    | Br_on_null (i, e') ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xD5;
        output_uint ch i
    | IfExpr (typ, e1, e2, e3) ->
        output_expression st ch e1;
        output_byte ch 0x04;
        output_valtype st.type_names ch typ;
        output_expression st ch e2;
        output_byte ch 0x05;
        output_expression st ch e3;
        output_byte ch 0x0B
    | Try (typ, l, catches) ->
        Feature.require exception_handling;
        output_byte ch 0x06;
        output_blocktype st.type_names ch typ;
        List.iter ~f:(fun i' -> output_instruction st ch i') l;
        List.iter
          ~f:(fun (tag, l, ty) ->
            output_byte ch 0x07;
            output_uint ch (Code.Var.Hashtbl.find st.tag_names tag);
            output_instruction st ch (Br (l + 1, Some (Pop ty))))
          catches;
        output_byte ch 0X0B
    | ExternConvertAny e' ->
        Feature.require gc;
        output_expression st ch e';
        output_byte ch 0xFB;
        output_byte ch 0x1B

  and output_instruction st ch i =
    match i with
    | Drop e ->
        output_expression st ch e;
        output_byte ch 0x1A
    | LocalSet (i, e) ->
        output_expression st ch e;
        output_byte ch 0x21;
        output_uint ch (Code.Var.Hashtbl.find st.current_local_names i)
    | GlobalSet (g, e) ->
        output_expression st ch e;
        output_byte ch 0x24;
        output_uint ch (Code.Var.Hashtbl.find st.global_names g)
    | Loop (typ, l) ->
        output_byte ch 0x03;
        output_blocktype st.type_names ch typ;
        List.iter ~f:(fun i' -> output_instruction st ch i') l;
        output_byte ch 0x0B
    | Block (typ, l) ->
        output_byte ch 0x02;
        output_blocktype st.type_names ch typ;
        List.iter ~f:(fun i' -> output_instruction st ch i') l;
        output_byte ch 0x0B
    | If (typ, e, l1, l2) ->
        output_expression st ch e;
        output_byte ch 0x04;
        output_blocktype st.type_names ch typ;
        List.iter ~f:(fun i' -> output_instruction st ch i') l1;
        if not (List.is_empty l2)
        then (
          output_byte ch 0x05;
          List.iter ~f:(fun i' -> output_instruction st ch i') l2);
        output_byte ch 0x0B
    | Br_table (e, l, i) ->
        output_expression st ch e;
        output_byte ch 0x0E;
        output_vec output_uint ch l;
        output_uint ch i
    | Br (i, None) ->
        output_byte ch 0x0C;
        output_uint ch i
    | Br (i, Some e) ->
        output_expression st ch e;
        output_byte ch 0x0C;
        output_uint ch i
    | Br_if (i, e) ->
        output_expression st ch e;
        output_byte ch 0x0D;
        output_uint ch i
    | Return None -> output_byte ch 0x0F
    | Return (Some e) ->
        output_expression st ch e;
        output_byte ch 0x0F
    | CallInstr (f, l) ->
        List.iter ~f:(fun e -> output_expression st ch e) l;
        output_byte ch 0x10;
        output_uint ch (Code.Var.Hashtbl.find st.func_names f)
    | Nop -> ()
    | Push e -> output_expression st ch e
    | Throw (tag, e) ->
        Feature.require exception_handling;
        output_expression st ch e;
        output_byte ch 0x08;
        output_uint ch (Code.Var.Hashtbl.find st.tag_names tag)
    | Rethrow i ->
        Feature.require exception_handling;
        output_byte ch 0x09;
        output_uint ch i
    | ArraySet (typ, e1, e2, e3) ->
        Feature.require gc;
        output_expression st ch e1;
        output_expression st ch e2;
        output_expression st ch e3;
        output_byte ch 0xFB;
        output_byte ch 0x0E;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | StructSet (typ, idx, e1, e2) ->
        Feature.require gc;
        output_expression st ch e1;
        output_expression st ch e2;
        output_byte ch 0xFB;
        output_byte ch 0x05;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ);
        output_uint ch idx
    | Return_call (f, l) ->
        Feature.require tail_call;
        List.iter ~f:(fun e -> output_expression st ch e) l;
        output_byte ch 0x12;
        output_uint ch (Code.Var.Hashtbl.find st.func_names f)
    | Return_call_ref (typ, e', l) ->
        Feature.require tail_call;
        List.iter ~f:(fun e' -> output_expression st ch e') l;
        output_expression st ch e';
        output_byte ch 0x15;
        output_uint ch (Code.Var.Hashtbl.find st.type_names typ)
    | Unreachable -> output_byte ch 0x00
    | Event Parse_info.{ src = None | Some ""; _ } -> push_no_event ch
    | Event Parse_info.{ src = Some src; line; col; _ } -> push_event ch ~src ~line ~col

  let output_globals ch (st, global_idx, fields) =
    let count =
      List.fold_left
        ~f:(fun count field ->
          match field with
          | Global _ -> count + 1
          | Function _ | Type _ | Import _ | Data _ | Tag _ -> count)
        ~init:0
        fields
    in
    output_uint ch count;
    let _idx =
      List.fold_left
        ~f:(fun idx field ->
          match field with
          | Global { name; typ; init; _ } ->
              Code.Var.Hashtbl.add st.global_names name idx;
              output_globaltype st.type_names ch typ;
              output_expression st ch init;
              output_byte ch 0x0B;
              idx + 1
          | Function _ | Type _ | Import _ | Data _ | Tag _ -> idx)
        ~init:global_idx
        fields
    in
    ()

  let output_exports ch (func_names, global_names, fields) =
    let count =
      List.fold_left
        ~f:(fun count field ->
          match field with
          | Function { exported_name = Some _; _ } | Global { exported_name = Some _; _ }
            -> count + 1
          | Function { exported_name = None; _ }
          | Global { exported_name = None; _ }
          | Import _ | Type _ | Data _ | Tag _ -> count)
        ~init:0
        fields
    in
    output_uint ch count;
    List.iter
      ~f:(fun field ->
        match field with
        | Function { exported_name = None; _ }
        | Type _ | Data _
        | Global { exported_name = None; _ }
        | Tag _ | Import _ -> ()
        | Function { name; exported_name = Some exported_name; _ } ->
            output_name ch exported_name;
            output_byte ch 0x00;
            output_uint ch (Code.Var.Hashtbl.find func_names name)
        | Global { name; exported_name = Some exported_name; typ; _ } ->
            if typ.mut then Feature.require mutable_globals;
            output_name ch exported_name;
            output_byte ch 0x03;
            output_uint ch (Code.Var.Hashtbl.find global_names name))
      fields

  let compute_data_names fields =
    let data_count =
      List.fold_left
        ~f:(fun count field ->
          match field with
          | Data _ -> count + 1
          | Function _ | Type _ | Import _ | Global _ | Tag _ -> count)
        ~init:0
        fields
    in
    let data_names = Code.Var.Hashtbl.create 16 in
    let _idx =
      List.fold_left
        ~f:(fun idx field ->
          match field with
          | Data { name; _ } ->
              Code.Var.Hashtbl.add data_names name idx;
              idx + 1
          | Function _ | Type _ | Import _ | Global _ | Tag _ -> idx)
        ~init:0
        fields
    in
    data_count, data_names

  let output_data_count ch data_count = output_uint ch data_count

  let output_data ch (data_count, fields) =
    output_uint ch data_count;
    ignore
      (List.fold_left
         ~f:(fun idx field ->
           match field with
           | Data { contents; _ } ->
               output_byte ch 1;
               output_name ch contents;
               idx + 1
           | Function _ | Type _ | Import _ | Global _ | Tag _ -> idx)
         ~init:0
         fields)

  let rec expr_function_references e set =
    match e with
    | Const _ | LocalGet _ | GlobalGet _ | Pop _ | RefNull _ -> set
    | UnOp (_, e')
    | I32WrapI64 e'
    | I64ExtendI32 (_, e')
    | F32DemoteF64 e'
    | F64PromoteF32 e'
    | LocalTee (_, e')
    | RefI31 e'
    | I31Get (_, e')
    | ArrayLen e'
    | StructGet (_, _, _, e')
    | RefCast (_, e')
    | RefTest (_, e')
    | Br_on_cast (_, _, _, e')
    | Br_on_cast_fail (_, _, _, e')
    | Br_on_null (_, e') -> expr_function_references e' set
    | ExternConvertAny e' -> expr_function_references e' set
    | BinOp (_, e', e'')
    | ArrayNew (_, e', e'')
    | ArrayNewData (_, _, e', e'')
    | ArrayGet (_, _, e', e'')
    | RefEq (e', e'') ->
        set |> expr_function_references e' |> expr_function_references e''
    | IfExpr (_, e1, e2, e3) ->
        set
        |> expr_function_references e1
        |> expr_function_references e2
        |> expr_function_references e3
    | BlockExpr (_, l) ->
        List.fold_left ~f:(fun set i -> instr_function_references i set) ~init:set l
    | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
        List.fold_left ~f:(fun set i -> expr_function_references i set) ~init:set l
    | Seq _ -> assert false
    | RefFunc f -> Code.Var.Set.add f set
    | Call_ref (_, e', l) ->
        List.fold_left
          ~f:(fun set i -> expr_function_references i set)
          ~init:(expr_function_references e' set)
          l
    | Try (_, l, _) ->
        List.fold_left ~f:(fun set i -> instr_function_references i set) ~init:set l

  and instr_function_references i set =
    match i with
    | Drop e
    | LocalSet (_, e)
    | GlobalSet (_, e)
    | Br (_, Some e)
    | Br_table (e, _, _)
    | Br_if (_, e)
    | Return (Some e)
    | Push e
    | Throw (_, e) -> expr_function_references e set
    | Loop (_, l) | Block (_, l) ->
        List.fold_left ~f:(fun set i -> instr_function_references i set) ~init:set l
    | If (_, e, l1, l2) ->
        set
        |> expr_function_references e
        |> (fun init ->
        List.fold_left ~f:(fun set i -> instr_function_references i set) ~init l1)
        |> fun init ->
        List.fold_left ~f:(fun set i -> instr_function_references i set) ~init l2
    | Br (_, None) | Return None | Nop | Rethrow _ -> set
    | CallInstr (_, l) ->
        List.fold_left ~f:(fun set i -> expr_function_references i set) ~init:set l
    | ArraySet (_, e1, e2, e3) ->
        set
        |> expr_function_references e1
        |> expr_function_references e2
        |> expr_function_references e3
    | StructSet (_, _, e1, e2) ->
        set |> expr_function_references e1 |> expr_function_references e2
    | Return_call (_, l) ->
        List.fold_left ~f:(fun set i -> expr_function_references i set) ~init:set l
    | Return_call_ref (_, e', l) ->
        List.fold_left
          ~f:(fun set i -> expr_function_references i set)
          ~init:(expr_function_references e' set)
          l
    | Unreachable | Event _ -> set

  let function_references fields set =
    List.fold_left
      ~f:(fun set field ->
        match field with
        | Function { body; _ } ->
            List.fold_left
              ~f:(fun set i -> instr_function_references i set)
              ~init:set
              body
        | Global _ | Import _ | Type _ | Data _ | Tag _ -> set)
      ~init:set
      fields

  let output_elem ch (st, refs) =
    output_byte ch (* declare *) 1;
    output_byte ch (* func *) 3;
    output_byte ch 0x00;
    let refs = Code.Var.Set.elements refs in
    output_vec
      (fun ch f -> output_uint ch (Code.Var.Hashtbl.find st.func_names f))
      ch
      refs

  let coalesce_locals l =
    let rec loop acc n t l =
      match l with
      | [] -> List.rev ((n, t) :: acc)
      | (_, t') :: r ->
          if Poly.equal t t' then loop acc (n + 1) t r else loop ((n, t) :: acc) 1 t' r
    in
    match l with
    | [] -> []
    | (_, t) :: rem -> loop [] 1 t rem

  let output_code ch (st, fields) =
    let l =
      List.fold_left
        ~f:(fun acc field ->
          match field with
          | Function { name; param_names; locals; body; _ } ->
              (name, param_names, locals, body) :: acc
          | Type _ | Import _ | Data _ | Global _ | Tag _ -> acc)
        ~init:[]
        fields
    in
    output_vec
      (with_size (fun ch (name, param_names, locals, body) ->
           let current_local_names = Code.Var.Hashtbl.create 8 in
           let idx =
             List.fold_left
               ~f:(fun idx x ->
                 Code.Var.Hashtbl.add current_local_names x idx;
                 idx + 1)
               ~init:0
               param_names
           in
           let _ =
             List.fold_left
               ~f:(fun idx (x, _) ->
                 Code.Var.Hashtbl.add current_local_names x idx;
                 idx + 1)
               ~init:idx
               locals
           in
           Code.Var.Hashtbl.add st.local_names name current_local_names;
           let st = { st with current_local_names } in
           output_vec
             (fun ch (n, typ) ->
               output_uint ch n;
               output_valtype st.type_names ch typ)
             ch
             (coalesce_locals locals);
           (try List.iter ~f:(fun i -> output_instruction st ch i) body
            with e ->
              let backtrace = Printexc.get_backtrace () in
              prerr_endline (Printexc.to_string e);
              prerr_endline backtrace;
              assert false);
           output_byte ch 0x0B;
           push_no_event ch))
      ch
      (List.rev l)

  let output_section id f ch x =
    output_byte ch id;
    with_size f ch x

  let assign_names f tbl =
    let names = Code.Var.Hashtbl.fold (fun name idx rem -> (idx, name) :: rem) tbl [] in
    let names = List.sort ~cmp:(fun (idx, _) (idx', _) -> compare idx idx') names in
    let used = ref StringSet.empty in
    let counts = String.Hashtbl.create 101 in
    let rec find_available_name used name =
      let i =
        try String.Hashtbl.find counts name
        with Not_found ->
          let i = ref 0 in
          String.Hashtbl.replace counts name i;
          i
      in
      incr i;
      let nm = Printf.sprintf "%s$%d" name !i in
      if StringSet.mem nm used then find_available_name used name else nm
    in
    let names =
      List.map
        ~f:(fun (idx, x) ->
          match f x with
          | None -> idx, None
          | Some nm ->
              let nm =
                if StringSet.mem nm !used then find_available_name !used nm else nm
              in
              used := StringSet.add nm !used;
              idx, Some nm)
        names
    in
    let printer = Var_printer.create Var_printer.Alphabet.javascript in
    let i = ref 0 in
    let rec first_available_name () =
      let nm = Var_printer.to_string printer !i in
      incr i;
      if StringSet.mem nm !used then first_available_name () else nm
    in
    List.map
      ~f:(fun (idx, nm) ->
        match nm with
        | Some nm -> idx, nm
        | None -> idx, first_available_name ())
      names

  let output_names ch st =
    output_name ch "name";
    let index = Code.Var.get_name in
    let out id f tbl =
      let names = assign_names f tbl in
      if not (List.is_empty names)
      then
        output_section
          id
          (output_vec (fun ch (idx, name) ->
               output_uint ch idx;
               output_name ch name))
          ch
          names
    in
    let locals =
      Code.Var.Hashtbl.fold
        (fun name tbl rem -> (Code.Var.Hashtbl.find st.func_names name, tbl) :: rem)
        st.local_names
        []
      |> List.sort ~cmp:(fun (idx, _) (idx', _) -> compare idx idx')
    in
    out 1 index st.func_names;
    output_section
      2
      (output_vec (fun ch (idx, tbl) ->
           output_uint ch idx;
           let locals = assign_names index tbl in
           output_vec
             (fun ch (idx, name) ->
               output_uint ch idx;
               output_name ch name)
             ch
             locals))
      ch
      locals;
    out 4 index st.type_names;
    out 7 index st.global_names;
    out 9 index st.data_names;
    out 11 index st.tag_names

  let output_features ch () =
    output_name ch "target_features";
    output_vec
      (fun ch f ->
        output_byte ch 0x2b;
        output_name ch f)
      ch
      (Feature.get features)

  let output_module ch fields =
    output_string ch "\x00\x61\x73\x6D\x01\x00\x00\x00";
    let func_types, type_names = output_section 1 output_types ch fields in
    let func_idx, func_names, global_idx, global_names, _, tag_names =
      output_section 2 output_imports ch (func_types, type_names, fields)
    in
    output_section 3 output_functions ch (func_idx, func_names, func_types, fields);
    let st =
      { type_names
      ; func_names
      ; global_names
      ; data_names = Code.Var.Hashtbl.create 1
      ; tag_names
      ; local_names = Code.Var.Hashtbl.create 8
      ; current_local_names = Code.Var.Hashtbl.create 8
      }
    in
    output_section 6 output_globals ch (st, global_idx, fields);
    output_section 7 output_exports ch (func_names, global_names, fields);
    let refs = function_references fields Code.Var.Set.empty in
    output_section 9 output_elem ch (st, refs);
    let data_count, data_names = compute_data_names fields in
    if data_count > 0
    then (
      Feature.require bulk_memory;
      output_section 12 output_data_count ch data_count);
    let st = { st with data_names } in
    output_section 10 output_code ch (st, fields);
    output_section 11 output_data ch (data_count, fields);
    if Config.Flag.pretty () then output_section 0 output_names ch st;
    if Feature.test gc then Feature.require reference_types;
    output_section 0 output_features ch ()
end

let f ~opt_source_map_file ch fields =
  let mappings = ref [] in
  let files = String.Hashtbl.create 16 in
  let module O = Make (struct
    type t = out_channel

    let position = pos_out

    let seek = seek_out

    let byte = output_byte

    let string = output_string

    let push_mapping m = mappings := m :: !mappings

    let get_file_index file =
      try String.Hashtbl.find files file
      with Not_found ->
        let pos = String.Hashtbl.length files in
        String.Hashtbl.add files file pos;
        pos
  end) in
  O.output_module ch fields;
  Option.iter opt_source_map_file ~f:(fun source_map_file ->
      let hashtbl_to_list htb =
        String.Hashtbl.fold (fun k v l -> (k, v) :: l) htb []
        |> List.sort ~cmp:(fun (_, a) (_, b) -> compare a b)
        |> List.map ~f:fst
      in
      let sm =
        { (Source_map.Standard.empty ~inline_source_content:false) with
          sources = hashtbl_to_list files
        ; mappings = Source_map.Mappings.encode (List.rev !mappings)
        }
      in
      Source_map.to_file ~rewrite_paths:false (Standard sm) source_map_file)
