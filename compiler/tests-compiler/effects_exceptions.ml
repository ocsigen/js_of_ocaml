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
     {try
       {var _f_=runtime.caml_int_of_string(s),n=_f_}
      catch(_j_)
       {_j_ = caml_wrap_exception(_j_);
        if(_j_[1] !== Stdlib[7])
         {var raise$1=caml_pop_trap();return caml_cps_exact_call1(raise$1,_j_)}
        var n=0,_a_=0}
      try
       {if(caml_string_equal(s,cst$0))throw Stdlib[8];var _e_=7,m=_e_}
      catch(_i_)
       {_i_ = caml_wrap_exception(_i_);
        if(_i_ !== Stdlib[8])
         {var raise$0=caml_pop_trap();return caml_cps_exact_call1(raise$0,_i_)}
        var m=0,_b_=0}
      runtime.caml_push_trap
       (function(_h_)
         {if(_h_ === Stdlib[8])return cont(0);
          var raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_h_)});
      if(caml_string_equal(s,cst))
       {var _c_=Stdlib[8],raise=caml_pop_trap();
        return caml_cps_exact_call1(raise,_c_)}
      var _d_=Stdlib[79];
      return caml_cps_call2
              (_d_,
               cst_toto,
               function(_g_){caml_pop_trap();return cont([0,[0,_g_,n,m]])})}
    //end |}]
