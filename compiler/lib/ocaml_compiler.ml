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
  let open Asttypes in
  match c with
  | Const_base (Const_int i) -> Int (Targetint.of_int_warning_on_overflow i)
  | Const_base (Const_char c) -> Int (Targetint.of_int_exn (Char.code c))
  | Const_base (Const_string (s, _, _)) -> String s
  | Const_base (Const_float s) -> Float (Int64.bits_of_float (float_of_string s))
  | Const_base (Const_int32 i) -> Int32 i
  | Const_base (Const_int64 i) -> Int64 i
  | Const_base (Const_nativeint i) -> NativeInt (Int32.of_nativeint_warning_on_overflow i)
  | Const_immstring s -> String s
  | Const_float_array sl ->
      let l = List.map ~f:(fun f -> Int64.bits_of_float (float_of_string f)) sl in
      Float_array (Array.of_list l)
  | Const_block (tag, l) ->
      let l = Array.of_list (List.map l ~f:constant_of_const) in
      Tuple (tag, l, Unknown)

module Symtable = struct
  (* Copied from ocaml/bytecomp/symtable.ml *)
  module Num_tbl (M : Map.S) = struct
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

    let of_ident id =
      let name = Ident.name id in
      if Ident.is_predef id
      then Some (Glob_predef name)
      else if Ident.global id
      then Some (Glob_compunit name)
      else None

    let to_ident = function
      | Glob_compunit x -> Ident.create_persistent x
      | Glob_predef x -> Ident.create_predef x
    [@@ocaml.warning "-32"]
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

    let fold f t acc = fold (fun id acc -> f (to_local id) acc) t acc

    let enter t id = enter t (of_local id)
  end
  [@@if ocaml_version < (5, 2, 0)]

  module GlobalMap = struct
    module GlobalMap = Num_tbl (Symtable.Global.Map)
    include GlobalMap

    let to_local = function
      | Symtable.Global.Glob_compunit (Compunit x) -> Global.Glob_compunit x
      | Symtable.Global.Glob_predef (Predef_exn x) -> Global.Glob_predef x

    let of_local = function
      | Global.Glob_compunit x -> Symtable.Global.Glob_compunit (Compunit x)
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

    let fold f t acc = fold (fun id acc -> f (to_local id) acc) t acc

    let enter t id = enter t (of_local id)
  end
  [@@if ocaml_version >= (5, 2, 0)]

  let reloc_get_of_string name = Cmo_format.Reloc_getglobal (Ident.create_persistent name)
  [@@if ocaml_version < (5, 2, 0)]

  let reloc_set_of_string name = Cmo_format.Reloc_setglobal (Ident.create_persistent name)
  [@@if ocaml_version < (5, 2, 0)]

  let reloc_get_of_string name = Cmo_format.Reloc_getcompunit (Compunit name)
  [@@if ocaml_version >= (5, 2, 0)]

  let reloc_set_of_string name = Cmo_format.Reloc_setcompunit (Compunit name)
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
  [@@if ocaml_version < (5, 2, 0)]

  let reloc_ident name =
    let buf = Bigarray.(Array1.create char c_layout 4) in
    let () =
      try Symtable.patch_object buf [ reloc_get_of_string name, 0 ]
      with _ -> Symtable.patch_object buf [ reloc_set_of_string name, 0 ]
    in

    let get i = Char.code (Bigarray.Array1.get buf i) in
    let n = get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24) in
    n
  [@@if ocaml_version >= (5, 2, 0)]

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
