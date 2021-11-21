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
      ~flags:[ "--enable"; "use-js-string" ]
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
    var ex = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz,- 10));
    var ax = call_with_char(103);
    var bx = call_with_char(caml_string_get(cst_abcdefghijklmnopqrstuvwxyz,30)); |}]

let%expect_test "static eval of string get" =
  let program =
    compile_and_parse
      ~flags:[ "--disable"; "use-js-string" ]
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
    var ex = call_with_char(caml_string_get(constant,- 10));
    var ax = call_with_char(103);
    var bx = call_with_char(caml_string_get(constant,30)); |}]

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
    function myfun(param){return 42} |}]

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
    function copy_bucketlist(param)
     {if(param)
       {var
         key=param[1],
         data=param[2],
         next=param[3],
         prec$0=[0,key,data,next],
         prec=prec$0,
         param$0=next;
        for(;;)
         {if(param$0)
           {var
             key$0=param$0[1],
             data$0=param$0[2],
             next$0=param$0[3],
             r=[0,key$0,data$0,next$0];
            prec[3] = r;
            var prec=r,param$0=next$0;
            continue}
          return prec$0}}
      return 0} |}]
