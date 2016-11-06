(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2013 Hugo Heuzard
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

open Code
open Flow
module Primitive = Jsoo_primitive

let specialize_instr info i rem =
  match i with
  | Let (x, Prim (Extern "caml_format_int", [y;z])) ->
    begin match the_string_of info y with
      | Some "%d" ->
        begin match the_int info z with
          | Some i -> Let(x,Constant(String (Int32.to_string i)))
          | None -> Let (x, Prim (Extern "%caml_format_int_special", [z]))
        end
      | _ -> i
    end :: rem
  | Let (x, Prim (Extern "%caml_format_int_special", [z])) ->
    begin match the_int info z with
      | Some i -> Let(x,Constant(String (Int32.to_string i)))
      | None -> i
    end :: rem
  | Let (x, Prim (Extern ("caml_js_var"|"caml_js_expr"|"caml_pure_js_expr" as prim), [y])) ->
      begin match the_string_of info y with
        Some s ->
          Let (x, Prim (Extern prim, [Pc (String s)]))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern ("caml_register_named_value" as prim), [y;z])) ->
      begin match the_string_of info y with
        | Some s when Primitive.need_named_value s ->
          Let (x, Prim (Extern prim, [Pc (String s);z]))
        | Some _ ->
          (* Format.eprintf "Do not register named value %S@." s; *)
          Let(x, Constant (Int 0l))
        | None -> i
      end :: rem
  | Let (x, Prim (Extern "caml_js_call", [f; o; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "%caml_js_opt_call",
                        f :: o :: Array.to_list a))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_fun_call", [f; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "%caml_js_opt_fun_call",
                        f :: Array.to_list a))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_meth_call", [o; m; a])) ->
      begin match the_string_of info m with
        Some m ->
          begin match the_def_of info a with
            Some (Block (_, a)) ->
              let a = Array.map (fun x -> Pv x) a in
              Let (x, Prim (Extern "%caml_js_opt_meth_call",
                            o :: Pc (String m) :: Array.to_list a))
          | _ ->
              i
          end
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_new", [c; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "%caml_js_opt_new",
                        c :: Array.to_list a))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_object", [a])) ->
      begin try
        let a =
          match the_def_of info a with
            Some (Block (_, a)) -> a
          | _                   -> raise Exit
        in
        let a =
          Array.map
            (fun x ->
               match the_def_of info (Pv x) with
                 Some (Block (_, [|k; v|])) ->
                   let k =
                     match the_string_of info (Pv k) with
                     | Some s -> Pc (String s)
                     | _                               -> raise Exit
                   in
                   [k; Pv v]
               | _ ->
                   raise Exit)
            a
        in
        Let (x, Prim (Extern "%caml_js_opt_object",
                      List.flatten (Array.to_list a)))
      with Exit ->
        i
      end :: rem
  | Let (x, Prim (Extern "caml_js_get", [o; f])) ->
      begin match the_string_of info f with
        Some s ->
          Let (x, Prim (Extern "caml_js_get", [o; Pc (String s)]))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_set", [o; f; v])) ->
      begin match the_string_of info f with
        Some s ->
          Let (x, Prim (Extern "caml_js_set", [o; Pc (String s); v]))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_delete", [o; f])) ->
      begin match the_string_of info f with
        Some s ->
          Let (x, Prim (Extern "caml_js_delete", [o; Pc (String s)]))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "caml_js_from_string", [y])) ->
      begin match the_string_of info y with
        Some s when Util.is_ascii s ->
          Let (x, Constant (IString s))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "%int_mul", [y; z])) ->
      begin match the_int info y, the_int info z with
      | Some j, _ when Int32.abs j < 0x200000l ->
        Let (x, Prim (Extern "%direct_int_mul", [y; z]))
      | _, Some j when Int32.abs j < 0x200000l ->
        Let (x, Prim (Extern "%direct_int_mul", [y; z]))
      | _ ->
        i
      end :: rem
  | Let (x, Prim (Extern "%int_div", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0l ->
          Let (x, Prim (Extern "%direct_int_div", [y; z]))
      | _ ->
          i
      end :: rem
  | Let (x, Prim (Extern "%int_mod", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0l ->
          Let (x, Prim (Extern "%direct_int_mod", [y; z]))
      | _ ->
          i
      end :: rem
  | _ -> i :: rem

let rec specialize_instrs info checks l =
  match l with
    [] ->
      []
  | i :: r ->
      (* We make bound checking explicit. Then, we can remove duplicated
         bound checks. Also, it appears to be more efficient to inline
         the array access. The bound checking function returns the array,
         which allows to produce more compact code. *)
      match i with
      | Let (x, Prim (Extern "caml_array_get", [y;z]))
      | Let (x, Prim (Extern "caml_array_get_float", [y;z]))
      | Let (x, Prim (Extern "caml_array_get_addr", [y;z])) ->
          let idx =
            match the_int info z with Some idx -> `Cst idx | None -> `Var z in
          if List.mem (y, idx) checks then
            Let (x, Prim (Extern "caml_array_unsafe_get", [y;z])) ::
            specialize_instrs info checks r
          else
            let y' = Code.Var.fresh () in
            Let (y', Prim (Extern "caml_check_bound", [y;z])) ::
            Let (x, Prim (Extern "caml_array_unsafe_get", [Pv y';z])) ::
            specialize_instrs info ((y, idx) :: checks) r
      | Let (x, Prim (Extern "caml_array_set", [y;z;t]))
      | Let (x, Prim (Extern "caml_array_set_float", [y;z;t]))
      | Let (x, Prim (Extern "caml_array_set_addr", [y;z;t])) ->
          let idx =
            match the_int info z with Some idx -> `Cst idx | None -> `Var z in
          if List.mem (y, idx) checks then
            Let (x, Prim (Extern "caml_array_unsafe_set", [y;z;t])) ::
            specialize_instrs info checks r
          else
            let y' = Code.Var.fresh () in
            Let (y', Prim (Extern "caml_check_bound", [y;z])) ::
            Let (x, Prim (Extern "caml_array_unsafe_set", [Pv y';z;t])) ::
            specialize_instrs info ((y, idx) :: checks) r
      | _ ->
          specialize_instr info i (specialize_instrs info checks r)

let specialize_all_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body = specialize_instrs info [] block.body })
      blocks
  in
  (pc, blocks, free_pc)

(****)

let f info p = specialize_all_instrs info p
