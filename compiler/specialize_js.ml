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


let specialize_instr info i =
  match i with
  | Let (x, Prim (Extern "caml_format_int", [y;z])) ->
    begin match the_def_of info y with
      | Some (Constant (String "%d")) ->
        Let (x, Prim (Extern "%caml_format_int_special", [z]))
      | _ -> i
    end
  | Let (x, Prim (Extern "caml_js_var", [y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_var", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_const", [y])) ->
      begin match the_def_of info y with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_const", [Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_call", [f; o; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_call",
                        f :: o :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_fun_call", [f; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_fun_call",
                        f :: Array.to_list a))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_meth_call", [o; m; a])) ->
      begin match the_def_of info m with
        Some (Constant (String _ as m)) ->
          begin match the_def_of info a with
            Some (Block (_, a)) ->
              let a = Array.map (fun x -> Pv x) a in
              Let (x, Prim (Extern "caml_js_opt_meth_call",
                            o :: Pc m :: Array.to_list a))
          | _ ->
              i
          end
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_new", [c; a])) ->
      begin match the_def_of info a with
        Some (Block (_, a)) ->
          let a = Array.map (fun x -> Pv x) a in
          Let (x, Prim (Extern "caml_js_opt_new",
                        c :: Array.to_list a))
      | _ ->
          i
      end
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
                     match the_def_of info (Pv k) with
                       Some (Constant (String _ as s)) -> Pc s
                     | _                               -> raise Exit
                   in
                   [k; Pv v]
               | _ ->
                   raise Exit)
            a
        in
        Let (x, Prim (Extern "caml_js_opt_object",
                      List.flatten (Array.to_list a)))
      with Exit ->
        i
      end
  | Let (x, Prim (Extern "caml_js_get", [o; f])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_get", [o; Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_set", [o; f; v])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_set", [o; Pc c; v]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "caml_js_delete", [o; f])) ->
      begin match the_def_of info f with
        Some (Constant (String _ as c)) ->
          Let (x, Prim (Extern "caml_js_delete", [o; Pc c]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mul", [y; z])) ->
      begin match the_int info y, the_int info z with
        Some j, _ | _, Some j when abs j < 0x200000 ->
          Let (x, Prim (Extern "%direct_int_mul", [y; z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_div", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_div", [y; z]))
      | _ ->
          i
      end
  | Let (x, Prim (Extern "%int_mod", [y; z])) ->
      begin match the_int info z with
        Some j when j <> 0 ->
          Let (x, Prim (Extern "%direct_int_mod", [y; z]))
      | _ ->
          i
      end
  | _ -> i

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with
           Code.body =
             List.map (specialize_instr info) block.body;
           })
      blocks
  in
  (pc, blocks, free_pc)

(****)

let f p info =
  let p = specialize_instrs info p in
  p
