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
      ~effects:`Double_translation
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
     try{var _B_ = caml_int_of_string(s), n = _B_;}
     catch(_E_){
      var _w_ = caml_wrap_exception(_E_);
      if(_w_[1] !== Stdlib[7]) throw caml_maybe_attach_backtrace(_w_, 0);
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _A_ = 7, m = _A_;
     }
     catch(_D_){
      var _x_ = caml_wrap_exception(_D_);
      if(_x_ !== Stdlib[8]) throw caml_maybe_attach_backtrace(_x_, 0);
      var m = 0;
     }
     try{
      if(caml_string_equal(s, cst))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _z_ = [0, [0, caml_call1(Stdlib[79], cst_toto), n, m]];
      return _z_;
     }
     catch(_C_){
      var _y_ = caml_wrap_exception(_C_);
      if(_y_ === Stdlib[8]) return 0;
      throw caml_maybe_attach_backtrace(_y_, 0);
     }
    }
    //end
    function exceptions$1(s, cont){
     try{var _q_ = caml_int_of_string(s), n = _q_;}
     catch(_v_){
      var _r_ = caml_wrap_exception(_v_);
      if(_r_[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap();
       return raise$1(caml_maybe_attach_backtrace(_r_, 0));
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _o_ = 7, m = _o_;
     }
     catch(_u_){
      var _p_ = caml_wrap_exception(_u_);
      if(_p_ !== Stdlib[8]){
       var raise$0 = caml_pop_trap();
       return raise$0(caml_maybe_attach_backtrace(_p_, 0));
      }
      var m = 0;
     }
     caml_push_trap
      (function(_t_){
        if(_t_ === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap();
        return raise(caml_maybe_attach_backtrace(_t_, 0));
       });
     if(! caml_string_equal(s, cst))
      return caml_trampoline_cps_call2
              (Stdlib[79],
               cst_toto,
               function(_s_){caml_pop_trap(); return cont([0, [0, _s_, n, m]]);});
     var _n_ = Stdlib[8], raise = caml_pop_trap();
     return raise(caml_maybe_attach_backtrace(_n_, 1));
    }
    //end
    var exceptions = caml_cps_closure(exceptions$0, exceptions$1);
    //end
    |}];
  print_double_fun_decl code "handler_is_loop";
  [%expect
    {|
    function handler_is_loop$0(f, g, l){
     try{var _l_ = caml_call1(f, 0); return _l_;}
     catch(_m_){
      var l$0 = l;
      for(;;){
       var match = caml_call1(g, l$0);
       if(72330306 > match[1]){
        var exn = match[2];
        throw caml_maybe_attach_backtrace(exn, 1);
       }
       var l$1 = match[2];
       l$0 = l$1;
      }
     }
    }
    //end
    function handler_is_loop$1(f, g, l, cont){
     caml_push_trap
      (function(_k_){
        function _j_(l){
         return caml_trampoline_cps_call2
                 (g,
                  l,
                  function(match){
                   if(72330306 <= match[1]){
                    var l = match[2];
                    return caml_exact_trampoline_call1(_j_, l);
                   }
                   var
                    exn$0 = match[2],
                    raise = caml_pop_trap(),
                    exn = caml_maybe_attach_backtrace(exn$0, 1);
                   return raise(exn);
                  });
        }
        return _j_(l);
       });
     return caml_trampoline_cps_call2
             (f, 0, function(_i_){caml_pop_trap(); return cont(_i_);});
    }
    //end
    var handler_is_loop = caml_cps_closure(handler_is_loop$0, handler_is_loop$1);
    //end
    |}];
  print_double_fun_decl code "handler_is_merge_node";
  [%expect
    {|
    function handler_is_merge_node$0(g){
     try{var _g_ = caml_call1(g, 0), s = _g_;}catch(_h_){var s = cst$1;}
     return caml_call2(Stdlib[28], s, cst_aaa);
    }
    //end
    function handler_is_merge_node$1(g, cont){
     function _d_(s){
      return caml_trampoline_cps_call3(Stdlib[28], s, cst_aaa, cont);
     }
     caml_push_trap(function(_f_){return _d_(cst$1);});
     return caml_trampoline_cps_call2
             (g, 0, function(_e_){caml_pop_trap(); return _d_(_e_);});
    }
    //end
    var
     handler_is_merge_node =
       caml_cps_closure(handler_is_merge_node$0, handler_is_merge_node$1);
    //end
    |}]
