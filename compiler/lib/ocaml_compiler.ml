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

let rec constant_of_const : _ -> Code.constant =
  let open Lambda in
  let open Asttypes in
  function
  | Const_base (Const_int i) -> Int (Int32.of_int_warning_on_overflow i)
  | Const_base (Const_char c) -> Int (Int32.of_int (Char.code c))
  | ((Const_base (Const_string (s, _))) [@if ocaml_version < (4, 11, 0)])
  | ((Const_base (Const_string (s, _, _))) [@if ocaml_version >= (4, 11, 0)]) -> String s
  | Const_base (Const_float s) -> Float (float_of_string s)
  | Const_base (Const_int32 i) -> Int i
  | Const_base (Const_int64 i) -> Int64 i
  | Const_base (Const_nativeint i) -> Int (Int32.of_nativeint_warning_on_overflow i)
  | Const_immstring s -> String s
  | Const_float_array sl ->
      let l = List.map ~f:(fun f -> Code.Float (float_of_string f)) sl in
      Tuple (Obj.double_array_tag, Array.of_list l, Unknown)
  | ((Const_pointer (i, _)) [@if BUCKLESCRIPT]) ->
      Int (Int32.of_int_warning_on_overflow i)
  | ((Const_block (tag, _, l)) [@if BUCKLESCRIPT]) ->
      let l = Array.of_list (List.map l ~f:constant_of_const) in
      Tuple (tag, l, Unknown)
  | ((Const_pointer i) [@ifnot BUCKLESCRIPT] [@if ocaml_version < (4, 12, 0)]) ->
      Int (Int32.of_int_warning_on_overflow i)
  | ((Const_block (tag, l)) [@ifnot BUCKLESCRIPT]) ->
      let l = Array.of_list (List.map l ~f:constant_of_const) in
      Tuple (tag, l, Unknown)

let rec find_loc_in_summary ident' = function
  | Env.Env_empty -> None
  | Env.Env_value (_summary, ident, description)
    when Poly.(ident = ident') ->
    Some description.Types.val_loc
  | Env.Env_value (summary,_,_)
  | Env.Env_type (summary, _, _)
  | Env.Env_extension (summary, _, _)
  | (Env.Env_module (summary, _, _,_) [@if ocaml_version >= (4,8,0)])
  | (Env.Env_module (summary, _, _)   [@if ocaml_version <  (4,8,0)])
  | Env.Env_modtype (summary, _, _)
  | Env.Env_class (summary, _, _)
  | Env.Env_cltype (summary, _, _)
  | (Env.Env_open (summary, _)        [@if ocaml_version >= (4,8,0)])
  | (Env.Env_open (summary, _, _)     [@if ocaml_version <  (4,8,0)] [@if ocaml_version >= (4,7,0)])
  | (Env.Env_open (summary, _)        [@if ocaml_version <  (4,7,0)])
  | Env.Env_functor_arg (summary, _)
  | (Env.Env_constraints (summary, _) [@if ocaml_version >= (4,4,0)])
  | (Env.Env_copy_types (summary, _)  [@if ocaml_version >= (4,6,0)] [@if ocaml_version <  (4,10,0)])
  | (Env.Env_copy_types (summary)     [@if ocaml_version >= (4,10,0)])
  | (Env.Env_persistent (summary, _)  [@if ocaml_version >= (4,8,0)])
  | (Env.Env_value_unbound (summary, _, _)  [@if ocaml_version >= (4,10,0)])
  | (Env.Env_module_unbound (summary, _, _) [@if ocaml_version >= (4,10,0)])
    -> find_loc_in_summary ident' summary
[@@ocamlformat "disable"]

(* Copied from ocaml/utils/tbl.ml *)
module Tbl = struct
  [@@@ocaml.warning "-unused-field"]

  open Poly

  type ('a, 'b) t =
    | Empty
    | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

  let empty = Empty

  let height = function
    | Empty -> 0
    | Node (_, _, _, _, h) -> h

  let create l x d r =
    let hl = height l and hr = height r in
    Node (l, x, d, r, if hl >= hr then hl + 1 else hr + 1)

  let bal l x d r =
    let hl = height l and hr = height r in
    if hl > hr + 1
    then
      match l with
      | Node (ll, lv, ld, lr, _) when height ll >= height lr ->
          create ll lv ld (create lr x d r)
      | Node (ll, lv, ld, Node (lrl, lrv, lrd, lrr, _), _) ->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      | _ -> assert false
    else if hr > hl + 1
    then
      match r with
      | Node (rl, rv, rd, rr, _) when height rr >= height rl ->
          create (create l x d rl) rv rd rr
      | Node (Node (rll, rlv, rld, rlr, _), rv, rd, rr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      | _ -> assert false
    else create l x d r

  let rec add x data = function
    | Empty -> Node (Empty, x, data, Empty, 1)
    | Node (l, v, d, r, h) ->
        let c = compare x v in
        if c = 0
        then Node (l, x, data, r, h)
        else if c < 0
        then bal (add x data l) v d r
        else bal l v d (add x data r)

  let rec iter f = function
    | Empty -> ()
    | Node (l, v, d, r, _) ->
        iter f l;
        f v d;
        iter f r

  let rec find compare x = function
    | Empty -> raise Not_found
    | Node (l, v, d, r, _) ->
        let c = compare x v in
        if c = 0 then d else find compare x (if c < 0 then l else r)

  let rec fold f m accu =
    match m with
    | Empty -> accu
    | Node (l, v, d, r, _) -> fold f r (f v d (fold f l accu))
end
[@@if ocaml_version < (4, 8, 0)]

module Symtable = struct
  type 'a numtable =
    { num_cnt : int
    ; num_tbl : ('a, int) Tbl.t
    }

  module GlobalMap = struct
    type t = Ident.t numtable

    let filter_global_map (p : Ident.t -> bool) gmap =
      let newtbl = ref Tbl.empty in
      Tbl.iter (fun id num -> if p id then newtbl := Tbl.add id num !newtbl) gmap.num_tbl;
      { num_cnt = gmap.num_cnt; num_tbl = !newtbl }

    let find nn t =
      Tbl.find (fun x1 x2 -> String.compare (Ident.name x1) (Ident.name x2)) nn t.num_tbl

    let iter nn t = Tbl.iter nn t.num_tbl

    let fold f t acc = Tbl.fold f t.num_tbl acc
  end

  let reloc_ident name =
    let buf = Bytes.create 4 in
    Symtable.patch_object buf [ Reloc_setglobal (Ident.create_persistent name), 0 ];
    let get i = Char.code (Bytes.get buf i) in
    get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24)
end
[@@if ocaml_version < (4, 8, 0)]

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

  module GlobalMap = struct
    module GlobalMap = Num_tbl (Ident.Map)
    include GlobalMap

    let filter_global_map (p : Ident.t -> bool) (gmap : t) =
      let newtbl = ref Ident.Map.empty in
      Ident.Map.iter
        (fun id num -> if p id then newtbl := Ident.Map.add id num !newtbl)
        gmap.tbl;
      { cnt = gmap.cnt; tbl = !newtbl }
  end

  let reloc_ident name =
    let buf = Bytes.create 4 in
    Symtable.patch_object [| buf |] [ Reloc_setglobal (Ident.create_persistent name), 0 ];
    let get i = Char.code (Bytes.get buf i) in
    get 0 + (get 1 lsl 8) + (get 2 lsl 16) + (get 3 lsl 24)
end
[@@if ocaml_version >= (4, 8, 0)]

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

  let rec table_contents_rec sz t rem =
    match t with
    | Empty -> rem
    | Node (l, v, r, _) ->
        table_contents_rec
          sz
          l
          ((sz - v.data, Ident.name v.ident, v.ident) :: table_contents_rec sz r rem)

  let table_contents sz (t : 'a tbl) =
    List.sort
      ~cmp:(fun (i, _, _) (j, _, _) -> compare i j)
      (table_contents_rec sz (Obj.magic (t : 'a tbl) : 'a tbl') [])
end
