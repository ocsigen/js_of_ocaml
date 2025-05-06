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
      ~effects:`Cps
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
     try{var _y_ = runtime.caml_int_of_string(s), n = _y_;}
     catch(exn$0){
      var exn = caml_wrap_exception(exn$0);
      if(exn[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap(), exn$2 = caml_maybe_attach_backtrace(exn, 0);
       return raise$1(exn$2);
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _x_ = 7, m = _x_;
     }
     catch(exn){
      var exn$0 = caml_wrap_exception(exn);
      if(exn$0 !== Stdlib[8]){
       var
        raise$0 = caml_pop_trap(),
        exn$1 = caml_maybe_attach_backtrace(exn$0, 0);
       return raise$0(exn$1);
      }
      var m = 0;
     }
     runtime.caml_push_trap
      (function(exn){
        if(exn === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap(), exn$0 = caml_maybe_attach_backtrace(exn, 0);
        return raise(exn$0);
       });
     if(! caml_string_equal(s, cst))
      return caml_trampoline_cps_call2
              (Stdlib[79],
               cst_toto,
               function(_z_){caml_pop_trap(); return cont([0, [0, _z_, n, m]]);});
     var _w_ = Stdlib[8], raise = caml_pop_trap();
     return raise(caml_maybe_attach_backtrace(_w_, 1));
    }
    //end
    function cond1(b, cont){
     function _v_(ic){return cont([0, ic, 7]);}
     return b
             ? caml_trampoline_cps_call2(Stdlib[79], cst_toto$0, _v_)
             : caml_trampoline_cps_call2(Stdlib[79], cst_titi, _v_);
    }
    //end
    function cond2(b, cont){
     function _t_(_u_){return cont(7);}
     return b
             ? caml_trampoline_cps_call2(Stdlib_Printf[3], _h_, _t_)
             : caml_trampoline_cps_call2(Stdlib_Printf[3], _i_, _t_);
    }
    //end
    function cond3(b, cont){
     var x = [0, 0];
     function _r_(_s_){return cont(x[1]);}
     return b
             ? (x[1] = 1, _r_(0))
             : caml_trampoline_cps_call2(Stdlib_Printf[3], _j_, _r_);
    }
    //end
    function loop1(b, cont){
     return caml_trampoline_cps_call2
             (Stdlib[79],
              cst_static_examples_ml,
              function(ic){
               function _p_(_q_){
                return caml_trampoline_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return b
                                  ? caml_trampoline_cps_call2(Stdlib[53], line, _p_)
                                  : caml_exact_trampoline_call1(_p_, 0);
                         });
               }
               return _p_(0);
              });
    }
    //end
    function loop2(param, cont){
     return caml_trampoline_cps_call2
             (Stdlib[79],
              cst_static_examples_ml$0,
              function(ic){
               function _n_(_o_){
                return caml_trampoline_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return caml_trampoline_cps_call2(Stdlib[53], line, _n_);
                         });
               }
               return caml_trampoline_cps_call2(Stdlib_Printf[3], _k_, _n_);
              });
    }
    //end
    function loop3(param, cont){
     return caml_trampoline_cps_call2
             (list_rev,
              _l_,
              function(l){
               function _m_(x){
                if(! x) return cont(l);
                var r = x[2];
                return caml_exact_trampoline_call1(_m_, r);
               }
               return _m_(l);
              });
    }
    //end
    |}]
