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

         let list_rev = List.rev
         (* Avoid to expose the offset of stdlib modules *)
         let () = ignore (list_rev [])

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

         let loop3 () =
           let l = list_rev [1;2;3] in
           let rec f x =
             match x with
             | [] -> l
             | _ :: r -> f r
           in
           f l
       |}
  in
  print_fun_decl code (Some "exceptions");
  print_fun_decl code (Some "cond1");
  print_fun_decl code (Some "cond2");
  print_fun_decl code (Some "cond3");
  print_fun_decl code (Some "loop1");
  print_fun_decl code (Some "loop2");
  print_fun_decl code (Some "loop3");
  [%expect
    {|

    function exceptions(s, cont){
     try{var _t_ = runtime.caml_int_of_string(s), n = _t_;}
     catch(_x_){
      var _p_ = caml_wrap_exception(_x_);
      if(_p_[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap();
       return raise$1(caml_maybe_attach_backtrace(_p_, 0));
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _s_ = 7, m = _s_;
     }
     catch(_w_){
      var _q_ = caml_wrap_exception(_w_);
      if(_q_ !== Stdlib[8]){
       var raise$0 = caml_pop_trap();
       return raise$0(caml_maybe_attach_backtrace(_q_, 0));
      }
      var m = 0;
     }
     runtime.caml_push_trap
      (function(_v_){
        if(_v_ === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap();
        return raise(caml_maybe_attach_backtrace(_v_, 0));
       });
     if(! caml_string_equal(s, cst))
      return caml_cps_call2
              (Stdlib[79],
               cst_toto,
               function(_u_){caml_pop_trap(); return cont([0, [0, _u_, n, m]]);});
     var _r_ = Stdlib[8], raise = caml_pop_trap();
     return raise(caml_maybe_attach_backtrace(_r_, 1));
    }
    //end
    function cond1(b, cont){
     function _o_(ic){return cont([0, ic, 7]);}
     return b
             ? caml_cps_call2(Stdlib[79], cst_toto$0, _o_)
             : caml_cps_call2(Stdlib[79], cst_titi, _o_);
    }
    //end
    function cond2(b, cont){
     function _m_(_n_){return cont(7);}
     return b
             ? caml_cps_call2(Stdlib_Printf[3], _a_, _m_)
             : caml_cps_call2(Stdlib_Printf[3], _b_, _m_);
    }
    //end
    function cond3(b, cont){
     var x = [0, 0];
     function _k_(_l_){return cont(x[1]);}
     return b ? (x[1] = 1, _k_(0)) : caml_cps_call2(Stdlib_Printf[3], _c_, _k_);
    }
    //end
    function loop1(b, cont){
     return caml_cps_call2
             (Stdlib[79],
              cst_static_examples_ml,
              function(ic){
               function _i_(_j_){
                return caml_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return b
                                  ? caml_cps_call2(Stdlib[53], line, _i_)
                                  : caml_cps_exact_call1(_i_, 0);
                         });
               }
               return _i_(0);
              });
    }
    //end
    function loop2(param, cont){
     return caml_cps_call2
             (Stdlib[79],
              cst_static_examples_ml$0,
              function(ic){
               function _g_(_h_){
                return caml_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return caml_cps_call2(Stdlib[53], line, _g_);
                         });
               }
               return caml_cps_call2(Stdlib_Printf[3], _d_, _g_);
              });
    }
    //end
    function loop3(param, cont){
     return caml_cps_call2
             (list_rev,
              _e_,
              function(l){
               function _f_(x){
                if(! x) return cont(l);
                var r = x[2];
                return caml_cps_exact_call1(_f_, r);
               }
               return _f_(l);
              });
    }
    //end |}]
