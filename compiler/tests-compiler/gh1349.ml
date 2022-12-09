(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
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

(* https://github.com/ocsigen/js_of_ocaml/issues/1349 *)

let%expect_test _ =
  let prog =
    {|
  let myfun a b =
    let f x = x / 0 in
    let x = if a < b then 1 else 2 in
    let b = try f a with _ -> f x in
    b
|}
  in
  let program =
    Util.compile_and_parse
      ~flags:
        (List.concat
           [ [ "--disable"; "inline" ]
           ; [ "--disable"; "deadcode" ]
           ; [ "--disable"; "staticeval" ]
           ; [ "--enable"; "shortvar" ]
           ; [ "--debug"; "shortvar" ]
           ])
      prog
  in
  Util.print_program program;
  [%expect
    {|
    Function parameter properly assigned: 1/5
    short variable count: 12/12
    short variable occurrences: 26/26
    (function(d)
       {"use strict";
        var e=d.jsoo_runtime;
        function b(b,f)
         {function c(b){return e.caml_div(b,0)}
          var a=b < f?1:2;
          try {var g=c(b),d=g}catch(a){var d=c(a)}
          return d}
        var c=[0,b];
        e.caml_register_global(0,c,"Test");
        0;
        return}
      (globalThis));
    //end |}]
