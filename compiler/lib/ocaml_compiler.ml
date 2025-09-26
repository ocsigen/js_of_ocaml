(* Js_of_ocaml compiler
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

let rec constant_of_const c : Code.constant =
  let open Lambda in
  let open! Asttypes in
  match c with
  | Const_base (Const_int i) -> Int (Targetint.of_int_warning_on_overflow i)
  | ((Const_base
        ( Const_int8 i
        | Const_int16 i
        | Const_untagged_int i
        | Const_untagged_int8 i
        | Const_untagged_int16 i ))
     [@if oxcaml]) -> Int (Targetint.of_int_warning_on_overflow i)
  | Const_base (Const_char c) -> Int (Targetint.of_int_exn (Char.code c))
  | ((Const_base (Const_untagged_char c)) [@if oxcaml]) ->
      Int (Targetint.of_int_warning_on_overflow (Char.code c))
  | Const_base (Const_string (s, _, _)) -> String s
  | Const_base (Const_float s) -> Float (Int64.bits_of_float (float_of_string s))
  | ((Const_base (Const_unboxed_float s)) [@if oxcaml]) ->
      Float (Int64.bits_of_float (float_of_string s))
  | ((Const_base (Const_float32 s | Const_unboxed_float32 s)) [@if oxcaml]) ->
      Float32 (Int64.bits_of_float (Float32.of_string s |> Float32.to_float))
  | Const_base (Const_int32 i) -> Int32 i
  | ((Const_base (Const_unboxed_int32 i)) [@if oxcaml]) -> Int32 i
  | Const_base (Const_int64 i) -> Int64 i
  | ((Const_base (Const_unboxed_int64 i)) [@if oxcaml]) -> Int64 i
  | Const_base (Const_nativeint i) -> NativeInt (Int32.of_nativeint_warning_on_overflow i)
  | ((Const_base (Const_unboxed_nativeint i)) [@if oxcaml]) ->
      NativeInt (Int32.of_nativeint_warning_on_overflow i)
  | Const_immstring s -> String s
  | Const_float_array sl ->
      let l = List.map ~f:(fun f -> Int64.bits_of_float (float_of_string f)) sl in
      Float_array (Array.of_list l)
  | ((Const_float_block sl) [@if oxcaml]) ->
      let l = List.map ~f:(fun f -> Int64.bits_of_float (float_of_string f)) sl in
      Float_array (Array.of_list l)
  | Const_block (tag, l) ->
      let l = Array.of_list (List.map l ~f:constant_of_const) in
      Tuple (tag, l, Unknown)
  | ((Const_mixed_block (tag, _, l)) [@if oxcaml]) ->
      let l = Array.of_list (List.map l ~f:constant_of_const) in
      Tuple (tag, l, Unknown)
  | (Const_null [@if oxcaml]) -> Null_

type module_or_not =
  | Module
  | Not_module
  | Unknown

let rec is_module_in_summary deep ident' summary =
  match summary with
  (* Unknown *)
  | Env.Env_empty -> deep, Unknown
  (* Module *)
  | Env.Env_functor_arg (summary, ident) | Env.Env_persistent (summary, ident) ->
      if Ident.same ident ident'
      then deep, Module
      else is_module_in_summary (deep + 1) ident' summary
  | ((Env.Env_module (summary, ident, _, _)) [@if not oxcaml]) ->
      if Ident.same ident ident'
      then deep, Module
      else is_module_in_summary (deep + 1) ident' summary
  | ((Env.Env_module (summary, ident, _, _, _, _)) [@if oxcaml]) ->
      if Ident.same ident ident'
      then deep, Module
      else is_module_in_summary (deep + 1) ident' summary
  (* Not_module *)
  | Env.Env_modtype (summary, ident, _) | Env.Env_extension (summary, ident, _) ->
      if Ident.same ident ident'
      then deep, Not_module
      else is_module_in_summary (deep + 1) ident' summary
  (* Lowercase ident *)
  | Env.Env_type (summary, ident, _)
  | Env.Env_class (summary, ident, _)
  | Env.Env_cltype (summary, ident, _) ->
      ignore (ident : Ident.t);
      is_module_in_summary (deep + 1) ident' summary
  | ((Env.Env_value (summary, ident, _)) [@if not oxcaml]) ->
      ignore (ident : Ident.t);
      is_module_in_summary (deep + 1) ident' summary
  | ((Env.Env_value (summary, ident, _, _)) [@if oxcaml]) ->
      ignore (ident : Ident.t);
      is_module_in_summary (deep + 1) ident' summary
  (* Other, no ident *)
  | Env.Env_open (summary, _)
  | Env.Env_constraints (summary, _)
  | Env.Env_copy_types summary
  | Env.Env_value_unbound (summary, _, _)
  | Env.Env_module_unbound (summary, _, _) ->
      is_module_in_summary (deep + 1) ident' summary

let is_module_in_summary ident summary =
  let _deep, b = is_module_in_summary 0 ident summary in
  b

module Compilation_unit = struct
  type t = Cmo_format.compunit

  let name_as_string (Compunit x : t) = x

  let of_string x : t = Compunit x
end
[@@if (not oxcaml) && ocaml_version >= (5, 2, 0)]

module Compilation_unit = Compilation_unit [@@if oxcaml]

module Symtable = struct
  (* Copied from ocaml/bytecomp/symtable.ml *)
  module type MAP = sig
    type key

    type 'a t

    val empty : 'a t

    val find : key -> 'a t -> 'a

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val add : key -> 'a -> 'a t -> 'a t
  end

  module Num_tbl (M : MAP) = struct
    [@@@ocaml.warning "-32"]

    type t =
      { cnt : int
      ; (* The next number *)
        tbl : int M.t (* The table of already numbered objects *)
      }

    let empty = { cnt = 0; tbl = M.empty }

    let find key nt = M.find key nt.tbl

    let iter f nt = M.iter f nt.tbl

    let fold f nt a = M.fold f nt.tbl a

    let enter nt key =
      let n = !nt.cnt in
      nt := { cnt = n + 1; tbl = M.add key n !nt.tbl };
      n

    let incr nt =
      let n = !nt.cnt in
      nt := { cnt = n + 1; tbl = !nt.tbl };
      n
  end

  module Global = struct
    type t =
      | Glob_compunit of string
      | Glob_predef of string

    let name = function
      | Glob_compunit cu -> cu
      | Glob_predef exn -> exn

    let is_global = Ident.global [@@if not oxcaml]

    let is_global = Ident.is_global [@@if oxcaml]

    let of_ident id =
      let name = Ident.name id in
      if Ident.is_predef id
      then Some (Glob_predef name)
      else if is_global id
      then Some (Glob_compunit name)
      else None

    let to_ident = function
      | Glob_compunit x -> Ident.create_persistent x
      | Glob_predef x -> Ident.create_predef x
    [@@if ocaml_version < (5, 2, 0)]
  end

  module GlobalMap = struct
    module GlobalMap = Num_tbl (Ident.Map)
    include GlobalMap

    let to_local x =
      match Global.of_ident x with
      | None -> assert false
      | Some x -> x

    let of_local = Global.to_ident

    let filter (p : Global.t -> bool) (gmap : t) =
      let newtbl = ref Ident.Map.empty in
      Ident.Map.iter
        (fun id num -> if p (to_local id) then newtbl := Ident.Map.add id num !newtbl)
        gmap.tbl;
      { cnt = gmap.cnt; tbl = !newtbl }

    let find id t = find (of_local id) t

    let iter ~f t = iter (fun id pos -> f (to_local id) pos) t

    let fold f t acc = fold (fun id pos acc -> f (to_local id) pos acc) t acc

    let enter t id = enter t (of_local id)
  end
  [@@if ocaml_version < (5, 2, 0)]

  module GlobalMap = struct
    module GlobalMap = Num_tbl (Symtable.Global.Map)
    include GlobalMap

    let to_local = function
      | Symtable.Global.Glob_compunit x ->
          Global.Glob_compunit (Compilation_unit.name_as_string x)
      | Symtable.Global.Glob_predef (Predef_exn x) -> Global.Glob_predef x

    let of_local = function
      | Global.Glob_compunit x ->
          Symtable.Global.Glob_compunit (Compilation_unit.of_string x)
      | Global.Glob_predef x -> Symtable.Global.Glob_predef (Predef_exn x)

    let filter (p : Global.t -> bool) (gmap : t) =
      let newtbl = ref Symtable.Global.Map.empty in
      Symtable.Global.Map.iter
        (fun id num ->
          if p (to_local id) then newtbl := Symtable.Global.Map.add id num !newtbl)
        gmap.tbl;
      { cnt = gmap.cnt; tbl = !newtbl }

    let find id t = find (of_local id) t

    let iter ~f t = iter (fun id pos -> f (to_local id) pos) t

    let fold f t acc = fold (fun id pos acc -> f (to_local id) pos acc) t acc

    let enter t id = enter t (of_local id)
  end
  [@@if ocaml_version >= (5, 2, 0)]

  let reloc_get_of_string name = Cmo_format.Reloc_getglobal (Ident.create_persistent name)
  [@@if ocaml_version < (5, 2, 0)]

  let reloc_set_of_string name = Cmo_format.Reloc_setglobal (Ident.create_persistent name)
  [@@if ocaml_version < (5, 2, 0)]

  let reloc_get_of_string name =
    Cmo_format.Reloc_getcompunit (Compilation_unit.of_string name)
  [@@if ocaml_version >= (5, 2, 0)]

  let reloc_set_of_string name =
    Cmo_format.Reloc_setcompunit (Compilation_unit.of_string name)
  [@@if ocaml_version >= (5, 2, 0)]

  let reloc_ident name =
    let buf = Bytes.create 4 in
    let () =
      try Symtable.patch_object [| buf |] [ reloc_get_of_string name, 0 ]
      with _ -> Symtable.patch_object [| buf |] [ reloc_set_of_string name, 0 ]
    in

    let get i = Char.code (Bytes.get buf i) in
    let n = get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24) in
    n
  [@@if oxcaml || ocaml_version < (5, 2, 0)]

  let reloc_ident name =
    let buf = Bigarray.(Array1.create char c_layout 4) in
    let () =
      try Symtable.patch_object buf [ reloc_get_of_string name, 0 ]
      with _ -> Symtable.patch_object buf [ reloc_set_of_string name, 0 ]
    in

    let get i = Char.code (Bigarray.Array1.get buf i) in
    let n = get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24) in
    n
  [@@if (not oxcaml) && ocaml_version >= (5, 2, 0)]

  let current_state () : GlobalMap.t =
    let x : Symtable.global_map = Symtable.current_state () in
    Obj.magic x

  let all_primitives () : string list =
    let split_primitives p =
      let len = String.length p in
      let rec split beg cur =
        if cur >= len
        then []
        else if Char.equal p.[cur] '\000'
        then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
        else split beg (cur + 1)
      in
      split 0 0
    in
    split_primitives (Symtable.data_primitive_names ())
  [@@if ocaml_version < (5, 2)]

  let all_primitives () : string list = Symtable.data_primitive_names ()
  [@@if ocaml_version >= (5, 2)]
end

module Import_info = struct
  type t = string * Digest.t option

  type table = t list

  let to_list l = l

  let of_list l = l

  let name (n, _) = n

  let crc (_, c) = c
end
[@@if not oxcaml]

module Import_info = struct
  type t = Import_info.t

  type table = t array

  let to_list = Array.to_list

  let of_list = Array.of_list

  let name i = Import_info.name i |> Compilation_unit.Name.to_string

  let crc = Import_info.crc
end
[@@if oxcaml]

module Compilation_unit_descr = struct
  type t = Cmo_format.compilation_unit
end
[@@if not oxcaml]

module Compilation_unit_descr = struct
  type t = Cmo_format.compilation_unit_descr
end
[@@if oxcaml]

module Cmo_format = struct
  type t = Cmo_format.compilation_unit

  let name (t : t) = t.cu_name [@@if ocaml_version < (5, 2, 0)]

  let name (t : t) =
    let (Compunit name) = t.cu_name in
    name
  [@@if ocaml_version >= (5, 2, 0)]

  let requires (t : t) = List.map ~f:Ident.name t.cu_required_globals
  [@@if ocaml_version < (5, 2, 0)]

  let requires (t : t) = List.map t.cu_required_compunits ~f:(fun (Compunit u) -> u)
  [@@if ocaml_version >= (5, 2, 0)]

  let provides (t : t) =
    List.filter_map t.cu_reloc ~f:(fun ((reloc : Cmo_format.reloc_info), _) ->
        match reloc with
        | Reloc_setglobal i -> Some (Ident.name i)
        | Reloc_getglobal _ | Reloc_literal _ | Reloc_primitive _ -> None)
  [@@if ocaml_version < (5, 2, 0)]

  let provides (t : t) =
    List.filter_map t.cu_reloc ~f:(fun ((reloc : Cmo_format.reloc_info), _) ->
        match reloc with
        | Reloc_setcompunit (Compunit u) -> Some u
        | Reloc_getcompunit _ | Reloc_getpredef _ | Reloc_literal _ | Reloc_primitive _ ->
            None)
  [@@if ocaml_version >= (5, 2, 0)]

  let primitives (t : t) = t.cu_primitives

  let imports (t : t) = t.cu_imports

  let force_link (t : t) = t.cu_force_link
end
[@@if not oxcaml]

module Cmo_format = struct
  type t = Cmo_format.compilation_unit_descr

  let name (t : t) = Compilation_unit.name_as_string t.cu_name

  let requires (t : t) =
    List.map t.cu_required_compunits ~f:Compilation_unit.name_as_string

  let provides (t : t) =
    List.filter_map t.cu_reloc ~f:(fun ((reloc : Cmo_format.reloc_info), _) ->
        match reloc with
        | Reloc_setcompunit u -> Some (Compilation_unit.name_as_string u)
        | Reloc_getcompunit _ | Reloc_getpredef _ | Reloc_literal _ | Reloc_primitive _ ->
            None)

  let primitives (t : t) = t.cu_primitives

  let imports (t : t) = Array.to_list t.cu_imports

  let force_link (t : t) = t.cu_force_link
end
[@@if oxcaml]
