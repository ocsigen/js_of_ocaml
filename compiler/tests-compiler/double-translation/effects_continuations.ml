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
  print_double_fun_decl code "exceptions";
  print_double_fun_decl code "cond1";
  print_double_fun_decl code "cond2";
  print_double_fun_decl code "cond3";
  print_double_fun_decl code "loop1";
  print_double_fun_decl code "loop2";
  print_double_fun_decl code "loop3";
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
     runtime.caml_push_trap
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
    function cond1$0(b){
     var
      ic =
        b ? caml_call1(Stdlib[79], cst_toto$0) : caml_call1(Stdlib[79], cst_titi);
     return [0, ic, 7];
    }
    //end
    function cond1$1(b, cont){
     function a(ic){return cont([0, ic, 7]);}
     return b
             ? caml_trampoline_cps_call2(Stdlib[79], cst_toto$0, a)
             : caml_trampoline_cps_call2(Stdlib[79], cst_titi, a);
    }
    //end
    var cond1 = caml_cps_closure(cond1$0, cond1$1);
    //end
    function cond2$0(b$0){
     if(b$0)
      caml_call1(Stdlib_Printf[3], a);
     else
      caml_call1(Stdlib_Printf[3], b);
     return 7;
    }
    //end
    function cond2$1(b$0, cont){
     function c(a){return cont(7);}
     return b$0
             ? caml_trampoline_cps_call2(Stdlib_Printf[3], a, c)
             : caml_trampoline_cps_call2(Stdlib_Printf[3], b, c);
    }
    //end
    var cond2 = caml_cps_closure(cond2$0, cond2$1);
    //end
    function cond3$0(b){
     var x = [0, 0];
     if(b) x[1] = 1; else caml_call1(Stdlib_Printf[3], c);
     return x[1];
    }
    //end
    function cond3$1(b, cont){
     var x = [0, 0];
     function a(a){return cont(x[1]);}
     return b
             ? (x[1] = 1, a(0))
             : caml_trampoline_cps_call2(Stdlib_Printf[3], c, a);
    }
    //end
    var cond3 = caml_cps_closure(cond3$0, cond3$1);
    //end
    function loop1$0(b){
     var ic = caml_call1(Stdlib[79], cst_static_examples_ml);
     for(;;){
      var line = caml_call1(Stdlib[83], ic);
      if(b) caml_call1(Stdlib[53], line);
     }
    }
    //end
    function loop1$1(b, cont){
     return caml_trampoline_cps_call2
             (Stdlib[79],
              cst_static_examples_ml,
              function(ic){
               function a(c){
                return caml_trampoline_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return b
                                  ? caml_trampoline_cps_call2(Stdlib[53], line, a)
                                  : caml_exact_trampoline_call1(a, 0);
                         });
               }
               return a(0);
              });
    }
    //end
    var loop1 = caml_cps_closure(loop1$0, loop1$1);
    //end
    function loop2$0(param){
     var ic = caml_call1(Stdlib[79], cst_static_examples_ml$0);
     caml_call1(Stdlib_Printf[3], d);
     for(;;){var line = caml_call1(Stdlib[83], ic); caml_call1(Stdlib[53], line);}
    }
    //end
    function loop2$1(param, cont){
     return caml_trampoline_cps_call2
             (Stdlib[79],
              cst_static_examples_ml$0,
              function(ic){
               function a(b){
                return caml_trampoline_cps_call2
                        (Stdlib[83],
                         ic,
                         function(line){
                          return caml_trampoline_cps_call2(Stdlib[53], line, a);
                         });
               }
               return caml_trampoline_cps_call2(Stdlib_Printf[3], d, a);
              });
    }
    //end
    var loop2 = caml_cps_closure(loop2$0, loop2$1);
    //end
    function loop3$0(param){
     var l = caml_call1(list_rev, e), x = l;
     for(;;){if(! x) return l; var r = x[2]; x = r;}
    }
    //end
    function loop3$1(param, cont){
     return caml_trampoline_cps_call2
             (list_rev,
              e,
              function(l){
               function a(x){
                if(! x) return cont(l);
                var r = x[2];
                return caml_exact_trampoline_call1(a, r);
               }
               return a(l);
              });
    }
    //end
    var loop3 = caml_cps_closure(loop3$0, loop3$1);
    //end
    |}]
