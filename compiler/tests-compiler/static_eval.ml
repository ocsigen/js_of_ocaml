(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      ~use_js_string:true
      {|
    let lr = ref []
    let black_box v = lr := (Obj.repr v) :: !lr

    let constant = "abcdefghijklmnopqrstuvwxyz"

    let call_with_char c = black_box c

    let ex = call_with_char constant.[-10] ;;
    black_box ex
    let ax = call_with_char constant.[6]  ;;
    black_box ax
    let bx = call_with_char constant.[30] ;;
    black_box bx ;;
  |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz, - 10));
    //end
    var ax = call_with_char(103);
    //end
    var bx = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz, 30));
    //end |}]

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      ~use_js_string:false
      {|
    let lr = ref []
    let black_box v = lr := (Obj.repr v) :: !lr

    let constant = "abcdefghijklmnopqrstuvwxyz"

    let call_with_char c = black_box c

    let ex = call_with_char constant.[-10] ;;
    black_box ex
    let ax = call_with_char constant.[6]  ;;
    black_box ax
    let bx = call_with_char constant.[30] ;;
    black_box bx ;;
  |}
  in
  print_var_decl program "ex";
  print_var_decl program "ax";
  print_var_decl program "bx";
  [%expect
    {|
    var ex = call_with_char(caml_string_get(constant, - 10));
    //end
    var ax = call_with_char(103);
    //end
    var bx = call_with_char(caml_string_get(constant, 30));
    //end |}]

let%expect_test "static eval of Sys.backend_type" =
  let program =
    compile_and_parse_whole_program
      {|
    exception Myfun of (unit -> int)
    let myfun () =
      let constant = match Sys.backend_type with
      | Other "js_of_ocaml" -> 42
      | Native -> 1
      | Bytecode -> 2
      | Other _ -> 3
      in
      constant
    let () = raise (Myfun myfun)
  |}
  in
  print_fun_decl program (Some "myfun");
  [%expect {|
    function myfun(param){return 42;}
    //end |}]

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      {|

      type ('a, 'b) bucketlist =
        | Empty
        | Cons of { mutable key: 'a;
                    mutable data: 'b;
                    mutable next: ('a, 'b) bucketlist }

      let copy_bucketlist = function
        | Empty -> Empty
        | Cons {key; data; next} ->
            let rec loop prec = function
              | Empty -> ()
              | Cons {key; data; next} ->
                  let r = Cons {key; data; next} in
                  begin match prec with
                  | Empty -> assert false
                  | Cons prec ->  prec.next <- r
                  end;
                  loop r next
            in
            let r = Cons {key; data; next} in
            loop r next;
            r
  |}
  in
  print_fun_decl program (Some "copy_bucketlist");
  [%expect
    {|
    function copy_bucketlist(param){
     if(! param) return 0;
     var
      key = param[1],
      data = param[2],
      next = param[3],
      prec$1 = [0, key, data, next],
      prec = prec$1,
      param$0 = next;
     for(;;){
      if(! param$0) return prec$1;
      var
       key$0 = param$0[1],
       data$0 = param$0[2],
       next$0 = param$0[3],
       prec$0 = [0, key$0, data$0, next$0];
      prec[3] = prec$0;
      var prec = prec$0, param$0 = next$0;
     }
    }
    //end |}]

let%expect_test "static eval of tags" =
  let program =
    compile_and_parse
      {|

      type t = A | B | C of t | D of t | E of t

      let foobar =
        let x = if Random.int 3 > 1 then C (D A) else D (A) in
        match x with
        | A -> 1
        | B -> 2
        | C _
        | D _ -> 3
        | E _ -> 5

      let export = [|foobar;foobar|]
  |}
  in
  print_program program;
  [%expect
    {|
    (function(globalThis){
       "use strict";
       var runtime = globalThis.jsoo_runtime;
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) == 1
                ? f(a0)
                : runtime.caml_call_gen(f, [a0]);
       }
       var
        global_data = runtime.caml_get_global_data(),
        Stdlib_Random = global_data.Stdlib__Random,
        _a_ = [0, [1, 0]],
        _b_ = [1, 0],
        x = 1 < caml_call1(Stdlib_Random[5], 3) ? _a_ : _b_;
       x[0];
       var
        foobar = 3,
        export$0 = [0, foobar, foobar],
        Test = [0, foobar, export$0];
       runtime.caml_register_global(3, Test, "Test");
       return;
      }
      (globalThis));
    //end |}]
