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
      ~doubletranslate:true
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

         let handler_is_loop f g l =
           try f ()
           with exn ->
             let rec loop l =
               match g l with
               | `Fallback l' -> loop l'
               | `Raise exn -> raise exn
             in
             loop l

         let handler_is_merge_node g =
           let s = try g () with _ -> "" in
           s ^ "aaa"
       |}
  in
  print_double_fun_decl code "exceptions";
  [%expect
    {|

    function exceptions$0(s){
     try{var _E_ = caml_int_of_string(s), n = _E_;}
     catch(_H_){
      var _z_ = caml_wrap_exception(_H_);
      if(_z_[1] !== Stdlib[7]) throw caml_maybe_attach_backtrace(_z_, 0);
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _D_ = 7, m = _D_;
     }
     catch(_G_){
      var _A_ = caml_wrap_exception(_G_);
      if(_A_ !== Stdlib[8]) throw caml_maybe_attach_backtrace(_A_, 0);
      var m = 0;
     }
     try{
      if(caml_string_equal(s, cst))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _C_ = [0, [0, caml_doublecall1(Stdlib[79], cst_toto), n, m]];
      return _C_;
     }
     catch(_F_){
      var _B_ = caml_wrap_exception(_F_);
      if(_B_ === Stdlib[8]) return 0;
      throw caml_maybe_attach_backtrace(_B_, 0);
     }
    }
    //end
    function exceptions$1(s, cont){
     try{var _u_ = caml_int_of_string(s), n = _u_;}
     catch(_y_){
      var _p_ = caml_wrap_exception(_y_);
      if(_p_[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap();
       return raise$1(caml_maybe_attach_backtrace(_p_, 0));
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _t_ = 7, m = _t_;
     }
     catch(_x_){
      var _q_ = caml_wrap_exception(_x_);
      if(_q_ !== Stdlib[8]){
       var raise$0 = caml_pop_trap();
       return raise$0(caml_maybe_attach_backtrace(_q_, 0));
      }
      var m = 0;
     }
     caml_push_trap
      (function(_w_){
        if(_w_ === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap();
        return raise(caml_maybe_attach_backtrace(_w_, 0));
       });
     if(caml_string_equal(s, cst)){
      var _r_ = Stdlib[8], raise = caml_pop_trap();
      return raise(caml_maybe_attach_backtrace(_r_, 1));
     }
     var _s_ = Stdlib[79];
     return caml_cps_call2
             (_s_,
              cst_toto,
              function(_v_){caml_pop_trap(); return cont([0, [0, _v_, n, m]]);});
    }
    //end
    var exceptions = caml_cps_closure(exceptions$0, exceptions$1);
    //end |}];
  print_double_fun_decl code "handler_is_loop";
  [%expect
    {|
    function handler_is_loop$0(f, g, l){
     try{var _n_ = caml_doublecall1(f, 0); return _n_;}
     catch(_o_){
      var l$0 = l;
      for(;;){
       var match = caml_doublecall1(g, l$0);
       if(72330306 > match[1]){
        var exn = match[2];
        throw caml_maybe_attach_backtrace(exn, 1);
       }
       var l$1 = match[2], l$0 = l$1;
      }
     }
    }
    //end
    function handler_is_loop$1(f, g, l, cont){
     caml_push_trap
      (function(_l_){
        function _m_(l){
         return caml_cps_call2
                 (g,
                  l,
                  function(match){
                   if(72330306 <= match[1]){
                    var l = match[2];
                    return caml_cps_exact_call1(_m_, l);
                   }
                   var
                    exn = match[2],
                    raise = caml_pop_trap(),
                    exn$0 = caml_maybe_attach_backtrace(exn, 1);
                   return raise(exn$0);
                  });
        }
        return _m_(l);
       });
     var _j_ = 0;
     return caml_cps_call2
             (f, _j_, function(_k_){caml_pop_trap(); return cont(_k_);});
    }
    //end
    var handler_is_loop = caml_cps_closure(handler_is_loop$0, handler_is_loop$1);
    //end |}];
  print_double_fun_decl code "handler_is_merge_node";
  [%expect
    {|
    function handler_is_merge_node$0(g){
     try{var _h_ = caml_doublecall1(g, 0), s = _h_;}catch(_i_){var s = cst$1;}
     return caml_doublecall2(Stdlib[28], s, cst_aaa);
    }
    //end
    function handler_is_merge_node$1(g, cont){
     function _e_(s){return caml_cps_call3(Stdlib[28], s, cst_aaa, cont);}
     caml_push_trap(function(_g_){return _e_(cst$1);});
     var _d_ = 0;
     return caml_cps_call2
             (g, _d_, function(_f_){caml_pop_trap(); return _e_(_f_);});
    }
    //end
    var
     handler_is_merge_node =
       caml_cps_closure(handler_is_merge_node$0, handler_is_merge_node$1);
    //end |}]
