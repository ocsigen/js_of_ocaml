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
     try{var f = caml_int_of_string(s), n = f;}
     catch(b){
      var a = caml_wrap_exception(b);
      if(a[1] !== Stdlib[7]) throw caml_maybe_attach_backtrace(a, 0);
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var e = 7, m = e;
     }
     catch(a){
      var b = caml_wrap_exception(a);
      if(b !== Stdlib[8]) throw caml_maybe_attach_backtrace(b, 0);
      var m = 0;
     }
     try{
      if(caml_string_equal(s, cst))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var d = [0, [0, caml_call1(Stdlib[79], cst_toto), n, m]];
      return d;
     }
     catch(a){
      var c = caml_wrap_exception(a);
      if(c === Stdlib[8]) return 0;
      throw caml_maybe_attach_backtrace(c, 0);
     }
    }
    //end
    function exceptions$1(s, cont){
     try{var e = caml_int_of_string(s), n = e;}
     catch(a){
      var b = caml_wrap_exception(a);
      if(b[1] !== Stdlib[7]){
       var raise$1 = caml_pop_trap();
       return raise$1(caml_maybe_attach_backtrace(b, 0));
      }
      var n = 0;
     }
     try{
      if(caml_string_equal(s, cst$0))
       throw caml_maybe_attach_backtrace(Stdlib[8], 1);
      var d = 7, m = d;
     }
     catch(b){
      var a = caml_wrap_exception(b);
      if(a !== Stdlib[8]){
       var raise$0 = caml_pop_trap();
       return raise$0(caml_maybe_attach_backtrace(a, 0));
      }
      var m = 0;
     }
     caml_push_trap
      (function(a){
        if(a === Stdlib[8]) return cont(0);
        var raise = caml_pop_trap();
        return raise(caml_maybe_attach_backtrace(a, 0));
       });
     if(! caml_string_equal(s, cst))
      return caml_trampoline_cps_call2
              (Stdlib[79],
               cst_toto,
               function(a){caml_pop_trap(); return cont([0, [0, a, n, m]]);});
     var c = Stdlib[8], raise = caml_pop_trap();
     return raise(caml_maybe_attach_backtrace(c, 1));
    }
    //end
    var exceptions = caml_cps_closure(exceptions$0, exceptions$1);
    //end
    |}];
  print_double_fun_decl code "handler_is_loop";
  [%expect
    {|
    function handler_is_loop$0(f, g, l){
     try{var a = caml_call1(f, 0); return a;}
     catch(a){
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
      (function(b){
        function a(l){
         return caml_trampoline_cps_call2
                 (g,
                  l,
                  function(match){
                   if(72330306 <= match[1]){
                    var l = match[2];
                    return caml_exact_trampoline_call1(a, l);
                   }
                   var
                    exn$0 = match[2],
                    raise = caml_pop_trap(),
                    exn = caml_maybe_attach_backtrace(exn$0, 1);
                   return raise(exn);
                  });
        }
        return a(l);
       });
     return caml_trampoline_cps_call2
             (f, 0, function(a){caml_pop_trap(); return cont(a);});
    }
    //end
    var handler_is_loop = caml_cps_closure(handler_is_loop$0, handler_is_loop$1);
    //end
    |}];
  print_double_fun_decl code "handler_is_merge_node";
  [%expect
    {|
    function handler_is_merge_node$0(g){
     try{var a = caml_call1(g, 0), s = a;}catch(a){var s = cst$1;}
     return caml_call2(Stdlib[28], s, cst_aaa);
    }
    //end
    function handler_is_merge_node$1(g, cont){
     function a(s){
      return caml_trampoline_cps_call3(Stdlib[28], s, cst_aaa, cont);
     }
     caml_push_trap(function(b){return a(cst$1);});
     return caml_trampoline_cps_call2
             (g, 0, function(b){caml_pop_trap(); return a(b);});
    }
    //end
    var
     handler_is_merge_node =
       caml_cps_closure(handler_is_merge_node$0, handler_is_merge_node$1);
    //end
    |}]
