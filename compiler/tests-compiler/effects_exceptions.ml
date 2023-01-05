(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

open Util

let%expect_test "test-compiler/lib-effects/test1.ml" =
  let code =
    compile_and_parse
      ~effects:true
      {|
         let exceptions s =
           (* Compiled using 'try ... catch',
              and 'throw' within the try block *)
           let n = try int_of_string s with Failure _ -> 0 in
           let m =
             try if s = "" then raise Not_found else 7 with Not_found -> 0 in
           (* Uses caml_{push,pop}_trap. *)
           try
             if s = "" then raise Not_found;
             Some (open_in "toto", n, m)
            with Not_found ->
             None
       |}
  in
  print_fun_decl code (Some "exceptions");
  [%expect
    {|

    function exceptions(s,cont)
     {function _b_(n)
       {function _e_(m)
         {caml_push_trap
           (function(_j_)
             {if(_j_ === Stdlib[8])return caml_cps_exact_call1(cont,0);
              var raise=caml_pop_trap();
              return caml_cps_exact_call1(raise,_j_)});
          if(caml_string_equal(s,cst))
           {var _g_=Stdlib[8],raise=caml_pop_trap();
            return caml_cps_exact_call1(raise,_g_)}
          var _h_=Stdlib[79];
          return caml_cps_call2
                  (_h_,
                   cst_toto,
                   function(_i_)
                    {caml_pop_trap();
                     return caml_cps_exact_call1(cont,[0,[0,_i_,n,m]])})}
        caml_push_trap
         (function(_f_)
           {if(_f_ === Stdlib[8])return caml_cps_exact_call1(_e_,0);
            var raise=caml_pop_trap();
            return caml_cps_exact_call1(raise,_f_)});
        if(caml_string_equal(s,cst$0))
         {var _d_=Stdlib[8],raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_d_)}
        caml_pop_trap();
        return caml_cps_exact_call1(_e_,7)}
      caml_push_trap
       (function(_c_)
         {if(_c_[1] === Stdlib[7])return caml_cps_exact_call1(_b_,0);
          var raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_c_)});
      var _a_=runtime.caml_int_of_string(s);
      caml_pop_trap();
      return caml_cps_exact_call1(_b_,_a_)}
    //end |}]
