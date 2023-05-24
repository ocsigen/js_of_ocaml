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

let rec constant_of_const ~target c : Code.constant =
  let open Lambda in
  let open Asttypes in
  match c with
  | Const_base (Const_int i) ->
      Int
        ( Regular
        , match target with
          | `JavaScript -> Int32.of_int_warning_on_overflow i
          | `Wasm -> Int31.of_int_warning_on_overflow i )
  | Const_base (Const_char c) -> Int (Regular, Int32.of_int (Char.code c))
  | ((Const_base (Const_string (s, _))) [@if ocaml_version < (4, 11, 0)])
  | ((Const_base (Const_string (s, _, _))) [@if ocaml_version >= (4, 11, 0)]) -> String s
  | Const_base (Const_float s) -> Float (float_of_string s)
  | Const_base (Const_int32 i) -> Int (Int32, i)
  | Const_base (Const_int64 i) -> Int64 i
  | Const_base (Const_nativeint i) ->
      Int
        ( Native
        , match target with
          | `JavaScript -> Int32.of_nativeint_warning_on_overflow i
          | `Wasm -> Int31.of_nativeint_warning_on_overflow i )
  | Const_immstring s -> String s
  | Const_float_array sl ->
      let l = List.map ~f:(fun f -> Code.Float (float_of_string f)) sl in
      Tuple (Obj.double_array_tag, Array.of_list l, Unknown)
  | ((Const_pointer i) [@if ocaml_version < (4, 12, 0)]) ->
      Int
        (match target with
        | `JavaScript -> Int32.of_int_warning_on_overflow i
        | `Wasm -> Int31.of_int_warning_on_overflow i)
  | Const_block (tag, l) ->
      let l = Array.of_list (List.map l ~f:(fun c -> constant_of_const ~target c)) in
      Tuple (tag, l, Unknown)

let rec find_loc_in_summary ident' = function
  | Env.Env_empty -> None
  | Env.Env_value (_summary, ident, description) when Poly.(ident = ident') ->
      Some description.Types.val_loc
  | Env.Env_value (summary, _, _)
  | Env.Env_type (summary, _, _)
  | Env.Env_extension (summary, _, _)
  | Env.Env_module (summary, _, _, _)
  | Env.Env_modtype (summary, _, _)
  | Env.Env_class (summary, _, _)
  | Env.Env_cltype (summary, _, _)
  | Env.Env_open (summary, _)
  | Env.Env_functor_arg (summary, _)
  | Env.Env_constraints (summary, _)
  | ((Env.Env_copy_types (summary, _)) [@if ocaml_version < (4, 10, 0)])
  | ((Env.Env_copy_types summary) [@if ocaml_version >= (4, 10, 0)])
  | Env.Env_persistent (summary, _)
  | ((Env.Env_value_unbound (summary, _, _)) [@if ocaml_version >= (4, 10, 0)])
  | ((Env.Env_module_unbound (summary, _, _)) [@if ocaml_version >= (4, 10, 0)]) ->
      find_loc_in_summary ident' summary

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

  let current_state () : GlobalMap.t =
    let x : Symtable.global_map = Symtable.current_state () in
    Obj.magic x
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

  let primitives (t : t) = t.cu_primitives

  let imports (t : t) = t.cu_imports

  let force_link (t : t) = t.cu_force_link
end

module Ident = struct
  [@@@ocaml.warning "-unused-field"]

  (* Copied from ocaml/typing/ident.ml *)
  type 'a tbl' =
    | Empty
    | Node of 'a tbl' * 'a data * 'a tbl' * int

  and 'a data =
    { ident : Ident.t
    ; data : 'a
    ; previous : 'a data option
    }

  type 'a tbl = 'a Ident.tbl

  let rec table_contents_rec t rem =
    match t with
    | Empty -> rem
    | Node (l, v, r, _) ->
        table_contents_rec l ((v.data, v.ident) :: table_contents_rec r rem)

  let table_contents (t : 'a tbl) =
    table_contents_rec (Obj.magic (t : 'a tbl) : 'a tbl') []
end
