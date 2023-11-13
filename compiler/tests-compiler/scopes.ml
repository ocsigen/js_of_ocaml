(* Js_of_ocaml compiler
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
open Util

let test js_prog =
  let js_file =
    js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
  in
  let js_min_file =
    js_file
    |> jsoo_minify
         ~flags:[ "--enable"; "stable_var"; "--enable"; "shortvar" ]
         ~pretty:true
  in
  print_file (Filetype.path_of_js_file js_min_file);
  run_javascript js_file |> print_endline

let%expect_test "let inside forloop" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  for(let x = 0; x < 2; x ++) {
    y += x;
  }
  console.log(y);
  console.log(x);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v1 = 2;
      3:    var v2 = 0;
      4:    for(let v1 = 0; v1 < 2; v1++) v2 += v1;
      5:    console.log(v2);
      6:    console.log(v1);
      7:   }
      8:   ());
    1
    2 |}]

let%expect_test "let inside forin" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  var arr = [1,2,3];
  for(let x in arr) {
    console.log(x);
    y += arr[x];
  }
  console.log(y);
  console.log(x);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v2 = 2;
      3:    var v3 = 0, v1 = [1, 2, 3];
      4:    for(let v2 in v1){console.log(v2); v3 += v1[v2];}
      5:    console.log(v3);
      6:    console.log(v2);
      7:   }
      8:   ());
    0
    1
    2
    6
    2 |}]

let%expect_test "let inside forof" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  for(let x of [1,2,3]) {
    console.log(x);
    y += x;
  }
  console.log(y);
  console.log(x);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v1 = 2;
      3:    var v2 = 0;
      4:    for(let v1 of [1, 2, 3]){console.log(v1); v2 += v1;}
      5:    console.log(v2);
      6:    console.log(v1);
      7:   }
      8:   ());
    1
    2
    3
    6
    2 |}]

let%expect_test "let inside switch" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  switch(y){
    case 0:
      let x = 3;
      console.log(x);
  }
  console.log(x);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v1 = 2;
      3:    var v2 = 0;
      4:    switch(v2){case 0: let v1 = 3; console.log(v1);
      5:    }
      6:    console.log(v1);
      7:   }
      8:   ());
    3
    2 |}]

let%expect_test "let and var inside class static block" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  class z {
    static z = 2;
    static {
      let x = 3;
      var y = x;
      this.z = y
    }
    getZ(){ return this.z }
  }
  var t = new z;
  console.log(y, x, z.z);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v2 = 2;
      3:    var v3 = 0;
      4:    class
      5:    v4{static
      6:    z
      7:    =
      8:    2
      9:    static{let v2 = 3; var v3 = v2; this.z = v3;}
     10:    getZ(){return this.z;}
     11:    }
     12:    var v1 = new v4;
     13:    console.log(v3, v2, v4.z);
     14:   }
     15:   ());
    0 2 3 |}]

let%expect_test "let inside block" =
  test
    {|
(function () {
  let x = 2
  var y = 0;
  {
      let x = 3;
      var y = 4;
  }
  console.log(y, x);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    let v1 = 2;
      3:    var v2 = 0;
      4:    {let v3 = 3; var v2 = 4;}
      5:    console.log(v2, v1);
      6:   }
      7:   ());
    4 2 |}]
