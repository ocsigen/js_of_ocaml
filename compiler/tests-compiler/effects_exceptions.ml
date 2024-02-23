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
  print_fun_decl code (Some "exceptions");
  [%expect
    {|

    function exceptions(s, cont){
     try{var _k_ = runtime.caml_int_of_string(s), n = _k_;}
     catch(_o_){
      var _g_ = caml_wrap_exception(_o_);
      if(_g_[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap();
       return raise$1(caml_maybe_attach_backtrace(_g_, 0));
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var _j_ = 7, m = _j_;
     }
     catch(_n_){
      var _h_ = caml_wrap_exception(_n_);
      if(_h_ !== Stdlib[8]){
       var raise$0 = caml_pop_trap();
       return raise$0(caml_maybe_attach_backtrace(_h_, 0));
      }
      var m = 0;
     }
     caml_push_trap
      (function(_m_){
        if(_m_ === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap();
        return raise(caml_maybe_attach_backtrace(_m_, 0));
       });
     if(! caml_string_equal(s, cst))
      return caml_cps_call2
              (Stdlib[79],
               cst_toto,
               function(_l_){caml_pop_trap(); return cont([0, [0, _l_, n, m]]);});
     var _i_ = Stdlib[8], raise = caml_pop_trap();
     return raise(caml_maybe_attach_backtrace(_i_, 1));
    }
    //end |}];
  print_fun_decl code (Some "handler_is_loop");
  [%expect
    {|
    function handler_is_loop(f, g, l, cont){
     caml_push_trap
      (function(_e_){
        function _f_(l){
         return caml_cps_call2
                 (g,
                  l,
                  function(match){
                   if(72330306 <= match[1]){
                    var l = match[2];
                    return caml_cps_exact_call1(_f_, l);
                   }
                   var
                    exn = match[2],
                    raise = caml_pop_trap(),
                    exn$0 = caml_maybe_attach_backtrace(exn, 1);
                   return raise(exn$0);
                  });
        }
        return _f_(l);
       });
     return caml_cps_call2
             (f, 0, function(_d_){caml_pop_trap(); return cont(_d_);});
    }
    //end |}];
  print_fun_decl code (Some "handler_is_merge_node");
  [%expect
    {|
    function handler_is_merge_node(g, cont){
     function _a_(s){return caml_cps_call3(Stdlib[28], s, cst_aaa, cont);}
     caml_push_trap(function(_c_){return _a_(cst$1);});
     return caml_cps_call2(g, 0, function(_b_){caml_pop_trap(); return _a_(_b_);});
    }
    //end |}]
