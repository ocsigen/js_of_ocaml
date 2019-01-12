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

let rec obj_of_const =
  let open Lambda in
  let open Asttypes in
  function
  | Const_base (Const_int i) -> Obj.repr i
  | Const_base (Const_char c) -> Obj.repr c
  | Const_base (Const_string (s,_)) -> Obj.repr s
  | Const_base (Const_float s) -> Obj.repr (float_of_string s)
  | Const_base (Const_int32 i) -> Obj.repr i
  | Const_base (Const_int64 i) -> Obj.repr i
  | Const_base (Const_nativeint i) -> Obj.repr i
  | Const_immstring s -> Obj.repr s
  | Const_float_array sl ->
    let l = List.map float_of_string sl in
    Obj.repr (Array.of_list l)
#ifdef BUCKLESCRIPT
  | Const_pointer (i,_) ->
    Obj.repr i
  | Const_block (tag,_,l) ->
    let b = Obj.new_block tag (List.length l) in
    List.iteri (fun i x ->
      Obj.set_field b i (obj_of_const x)
    ) l;
    b
#else
  | Const_pointer i ->
    Obj.repr i
  | Const_block (tag,l) ->
    let b = Obj.new_block tag (List.length l) in
    List.iteri (fun i x ->
      Obj.set_field b i (obj_of_const x)
    ) l;
    b
#endif

let rec find_loc_in_summary ident' = function
  | Env.Env_empty -> None
  | Env.Env_value (_summary, ident, description)
    when ident = ident' ->
    Some description.Types.val_loc
  | Env.Env_value (summary,_,_)
  | Env.Env_type (summary, _, _)
  | Env.Env_extension (summary, _, _)
#if OCAML_VERSION >= (4,8,0)
  | Env.Env_module (summary, _, _,_)
#else
  | Env.Env_module (summary, _, _)
#endif
  | Env.Env_modtype (summary, _, _)
  | Env.Env_class (summary, _, _)
  | Env.Env_cltype (summary, _, _)
#if OCAML_VERSION >= (4,7,0)
  | Env.Env_open (summary, _, _)
#else
  | Env.Env_open (summary, _)
#endif
  | Env.Env_functor_arg (summary, _)
#if OCAML_VERSION >= (4,4,0)
  | Env.Env_constraints (summary, _)
#endif
#if OCAML_VERSION >= (4,6,0)
  | Env.Env_copy_types (summary, _)
#endif
   -> find_loc_in_summary ident' summary
