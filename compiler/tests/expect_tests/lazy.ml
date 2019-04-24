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
  let cmo =
    compile_ocaml_to_bytecode
      {|
        let lz = lazy ( List.map (fun x -> x * x) [8;9] )

        let rec do_the_lazy_rec n =
            if n = 0 then [] else (Lazy.force lz) :: do_the_lazy_rec (n-1)

        let _ =
            do_the_lazy_rec 8
  |}
  in
  let program = parse_js (print_compiled_js ~pretty:true cmo) in
  print_fun_decl program "do_the_lazy_rec";
  [%expect
    {|
    function do_the_lazy_rec(n)
     {if(0 === n)return 0;
      var
       _b_=do_the_lazy_rec(n - 1 | 0),
       _c_=caml_obj_tag(lz),
       _d_=250 === _c_?lz[1]:num_246 === _c_?caml_call1(CamlinternalLazy[2],lz):lz;
      return [0,_d_,_b_]} |}]
