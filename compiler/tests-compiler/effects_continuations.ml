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

         (* Conditional whose result is used *)
         let cond1 b =
           let ic = if b then open_in "toto" else open_in "titi" in
           (ic , 7)

         (* Conditional whose result is not used *)
         let cond2 b =
           if b then Printf.eprintf "toto" else Printf.eprintf "toto";
           7

         (* A dummy argument is used to call the continuation in the
            [then] clause *)
         let cond3 b =
           let x= ref 0 in if b then x := 1 else Printf.eprintf "toto";
           !x

         (* Two continuation functions are created. One to bind [ic] before
            entering the loop, and one for the loop. We use a dummy argument
            to go back to the begining of the loop if [b] is false *)
         let loop1 b =
           let all = ref [] in
           let ic = open_in "/static/examples.ml" in
           while true do
             let line = input_line ic in
             all := line :: !all;
             if b then prerr_endline line
           done

         (* There is a single continuation for the loop since the result of
            [Printf.eprintf] is ignored. *)
         let loop2 () =
           let all = ref [] in
           let ic = open_in "/static/examples.ml" in
           Printf.eprintf "titi";
           while true do
             let line = input_line ic in
             all := line :: !all;
             prerr_endline line
           done
       |}
  in
  print_fun_decl code (Some "exceptions");
  print_fun_decl code (Some "cond1");
  print_fun_decl code (Some "cond2");
  print_fun_decl code (Some "cond3");
  print_fun_decl code (Some "loop1");
  print_fun_decl code (Some "loop2");
  [%expect
    {|

    function exceptions(s,cont)
     {function _F_(n)
       {function _I_(m)
         {caml_push_trap
           (function(_N_)
             {if(_N_ === Stdlib[8])return caml_cps_exact_call1(cont,0);
              var raise=caml_pop_trap();
              return caml_cps_exact_call1(raise,_N_)});
          if(caml_string_equal(s,cst))
           {var _K_=Stdlib[8],raise=caml_pop_trap();
            return caml_cps_exact_call1(raise,_K_)}
          var _L_=Stdlib[79];
          return caml_cps_call2
                  (_L_,
                   cst_toto,
                   function(_M_)
                    {caml_pop_trap();
                     return caml_cps_exact_call1(cont,[0,[0,_M_,n,m]])})}
        caml_push_trap
         (function(_J_)
           {if(_J_ === Stdlib[8])return caml_cps_exact_call1(_I_,0);
            var raise=caml_pop_trap();
            return caml_cps_exact_call1(raise,_J_)});
        if(caml_string_equal(s,cst$0))
         {var _H_=Stdlib[8],raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_H_)}
        caml_pop_trap();
        return caml_cps_exact_call1(_I_,7)}
      caml_push_trap
       (function(_G_)
         {if(_G_[1] === Stdlib[7])return caml_cps_exact_call1(_F_,0);
          var raise=caml_pop_trap();
          return caml_cps_exact_call1(raise,_G_)});
      var _E_=runtime.caml_int_of_string(s);
      caml_pop_trap();
      return caml_cps_exact_call1(_F_,_E_)}
    //end
    function cond1(b,cont)
     {function _B_(ic){return caml_cps_exact_call1(cont,[0,ic,7])}
      if(b)
       {var _z_=Stdlib[79];
        return caml_cps_call2
                (_z_,
                 cst_toto$0,
                 function(_D_){return caml_cps_exact_call1(_B_,_D_)})}
      var _A_=Stdlib[79];
      return caml_cps_call2
              (_A_,cst_titi,function(_C_){return caml_cps_exact_call1(_B_,_C_)})}
    //end
    function cond2(b,cont)
     {function _w_(){return caml_cps_exact_call1(cont,7)}
      if(b)
       {var _u_=Stdlib_Printf[3];
        return caml_cps_call2
                (_u_,_a_,function(_y_){return caml_cps_exact_call0(_w_)})}
      var _v_=Stdlib_Printf[3];
      return caml_cps_call2
              (_v_,_b_,function(_x_){return caml_cps_exact_call0(_w_)})}
    //end
    function cond3(b,cont)
     {var x=[0,0];
      function _s_(){return caml_cps_exact_call1(cont,x[1])}
      if(b){x[1] = 1;return caml_cps_exact_call0(_s_)}
      var _r_=Stdlib_Printf[3];
      return caml_cps_call2
              (_r_,_c_,function(_t_){return caml_cps_exact_call0(_s_)})}
    //end
    function loop1(b,cont)
     {var all=[0,0],_l_=Stdlib[79];
      return caml_cps_call2
              (_l_,
               cst_static_examples_ml,
               function(ic)
                {function _m_()
                  {var _n_=Stdlib[83];
                   return caml_cps_call2
                           (_n_,
                            ic,
                            function(line)
                             {all[1] = [0,line,all[1]];
                              function _p_(){return caml_cps_exact_call0(_m_)}
                              if(! b)return caml_cps_exact_call0(_p_);
                              var _o_=Stdlib[53];
                              return caml_cps_call2
                                      (_o_,line,function(_q_){return caml_cps_exact_call0(_p_)})})}
                 return caml_cps_exact_call0(_m_)})}
    //end
    function loop2(param,cont)
     {var all=[0,0],_e_=Stdlib[79];
      return caml_cps_call2
              (_e_,
               cst_static_examples_ml$0,
               function(ic)
                {var _f_=Stdlib_Printf[3];
                 return caml_cps_call2
                         (_f_,
                          _d_,
                          function(_g_)
                           {function _h_()
                             {var _i_=Stdlib[83];
                              return caml_cps_call2
                                      (_i_,
                                       ic,
                                       function(line)
                                        {all[1] = [0,line,all[1]];
                                         var _j_=Stdlib[53];
                                         return caml_cps_call2
                                                 (_j_,line,function(_k_){return caml_cps_exact_call0(_h_)})})}
                            return caml_cps_exact_call0(_h_)})})}
    //end |}]
