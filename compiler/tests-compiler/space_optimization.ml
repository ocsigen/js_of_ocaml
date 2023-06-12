(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2023 Micah Cantor
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

let to_js_file js_prog =
  js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"

let%expect_test "if statement to conditional expression" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; else e3;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 ? e2 : e3;
        //end|}])

let%expect_test "one branch negated if to || expression" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (!e1) e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 || e2;
        //end|}])

let%expect_test "one branch if to && expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1 && e2;
        //end|}])

let%expect_test "expression statement; var declaration" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; var x = e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        var x = (e1, e2);
        //end|}])

let%expect_test "expression statement; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; return e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1, e2;
        //end|}])

let%expect_test "expression statement; if statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) 0; else 1;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2 ? 0 : 1;
        //end|}])

let%expect_test "expression statement; if statement with returns" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) return 0; else return 1;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1, e2 ? 0 : 1;
        //end|}])

let%expect_test "expression statement; if statement without else" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) 0;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2 && 0;
        //end|}])

let%expect_test "expression statement; if statement without else with return" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; if (e2) return 0;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        if(e1, e2) return 0;
        //end|}])

let%expect_test "if statement without else; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; return e3;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        return e1 && e2, e3;
        //end|}])

let%expect_test "expression statement; expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "e1; e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        e1, e2;
        //end|}])

let%expect_test "if statement; if statement; return statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; if (e3) e4; else e5; return e6;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
      return e1 && e2, e3 ? e4 : e5, e6;
      //end|}])

let%expect_test "if statement; if statement; var statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "if (e1) e2; if (e3) e4; else e5; var x = e6;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        var x = (e1 && e2, e3 ? e4 : e5, e6);
        //end|}])

let%expect_test "\"use strict\"; expression statement" =
  with_temp_dir ~f:(fun () ->
      let js_file = to_js_file "\"use strict\"; e2;" in
      print_endline (Util.optimize_space js_file);
      [%expect {|
        "use strict"; e2;
        //end|}])

(* Test on an arbitrary larger OCaml program to see the effect of simplification. *)
let%expect_test "end-to-end" =
  let prog =
    {|
  module M : sig
    val run : unit -> unit
  end = struct
    let delayed = ref []
    let even i =
      let rec odd = function
      | 0 ->
        let f () = Printf.printf "in odd, called with %d\n" i in
        delayed := f :: !delayed;
        f ();
        false
      | 1 -> not (not (even 0))
      | 2 -> not (not (even 1))
      | n -> not (not (even (n - 1)))
      and even = function
      | 0 ->
        let f () = Printf.printf "in even, called with %d\n" i in
        delayed := f :: !delayed;
        f ();
        true
      | 1 -> not (not (odd 0))
      | 2 -> not (not (odd 1))
      | n -> not (not (odd (n - 1)))
      in even i

  let run () =
    for i = 0 to 4 do
      ignore (even (i) : bool)
    done;
    List.iter (fun f -> f ()) (List.rev !delayed)
  end

  let ()  = M.run ()
  |}
  in
  let js_simpl_es6 =
    Util.compile_and_parse ~flags:[ "--enable"; "es6"; "--enable"; "simplify" ] prog
  in
  let js_pretty =
    Util.compile_and_parse ~flags:[ "--enable"; "es6"; "--disable"; "simplify" ] prog
  in
  Util.print_fun_decl js_simpl_es6 (Some "run");
  [%expect
    {|
      function run(param){
       var i = 0;
       for(;;){
        var
         closures =
           i=>{
            var
             even =
               n=>{
                if(2 < n >>> 0) return 1 - (1 - odd(n - 1 | 0));
                switch(n){
                  case 0:
                   var f = param=>caml_call2(Stdlib_Printf[2], _b_, i);
                   return delayed[1] = [0, f, delayed[1]], f(0), 1;
                  case 1:
                   return 1 - (1 - odd(0));
                  default: return 1 - (1 - odd(1));
                }},
             odd =
               n=>{
                if(2 < n >>> 0) return 1 - (1 - even(n - 1 | 0));
                switch(n){
                  case 0:
                   var f = param=>caml_call2(Stdlib_Printf[2], _a_, i);
                   return delayed[1] = [0, f, delayed[1]], f(0), 0;
                  case 1:
                   return 1 - (1 - even(0));
                  default: return 1 - (1 - even(1));
                }},
             block = [0, even, odd];
            return block;},
         closures$0 = closures(i),
         even = closures$0[1],
         _e_ = (even(i), i + 1 | 0);
        if(4 !== i){var i = _e_; continue;}
        var _c_ = caml_call1(Stdlib_List[9], delayed[1]), _d_ = f=>caml_call1(f, 0);
        return caml_call2(Stdlib_List[17], _d_, _c_);
       }
      }
      //end
    |}];
  Util.print_var_decl js_pretty "run";
  [%expect
    {|
    var run = (function(param){
      var i = 0;
      for(;;){
       var
        closures =
          function(i){
           var
            even =
              function(n){
               if(2 < n >>> 0) return 1 - (1 - odd(n + - 1 | 0));
               switch(n){
                 case 0:
                  var
                   f =
                     function(param){return caml_call2(Stdlib_Printf[2], _b_, i);};
                  delayed[1] = [0, f, delayed[1]];
                  f(0);
                  return 1;
                 case 1:
                  return 1 - (1 - odd(0));
                 default: return 1 - (1 - odd(1));
               }
              },
            odd =
              function(n){
               if(2 < n >>> 0) return 1 - (1 - even(n + - 1 | 0));
               switch(n){
                 case 0:
                  var
                   f =
                     function(param){return caml_call2(Stdlib_Printf[2], _a_, i);};
                  delayed[1] = [0, f, delayed[1]];
                  f(0);
                  return 0;
                 case 1:
                  return 1 - (1 - even(0));
                 default: return 1 - (1 - even(1));
               }
              },
            block = [0, even, odd];
           return block;
          },
        closures$0 = closures(i),
        even = closures$0[1];
       even(i);
       var _e_ = i + 1 | 0;
       if(4 !== i){var i = _e_; continue;}
       var
        _c_ = caml_call1(Stdlib_List[9], delayed[1]),
        _d_ = function(f){return caml_call1(f, 0);};
       return caml_call2(Stdlib_List[17], _d_, _c_);
      }
     });
    //end |}]
