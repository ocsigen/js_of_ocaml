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
      4:    for(let v3 = 0; v3 < 2; v3++) v2 += v3;
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
      4:    for(let v4 in v1){console.log(v4); v3 += v1[v4];}
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
      4:    for(let v3 of [1, 2, 3]){console.log(v3); v2 += v3;}
      5:    console.log(v2);
      6:    console.log(v1);
      7:   }
      8:   ());
    1
    2
    3
    6
    2 |}]

let%expect_test "let inside forawaitof" =
  test
    {|
async function f () {
  let x = 2
  var y = 0;
  for await(let x of [1,2,3]) {
    console.log(x);
    y += x;
  }
  console.log(y);
  console.log(x);
}
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: async function f(){
      2:  let v1 = 2;
      3:  var v2 = 0;
      4:  for await(let v3 of [1, 2, 3]){console.log(v3); v2 += v3;}
      5:  console.log(v2);
      6:  console.log(v1);
      7: } |}]

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
      4:    switch(v2){case 0: let v3 = 3; console.log(v3);
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
      4:    class v4{
      5:       static z = 2;
      6:       static {let v5 = 3; var v6 = v5; this.z = v6;}
      7:       getZ(){return this.z;}
      8:     }
      9:    var v1 = new v4;
     10:    console.log(v3, v2, v4.z);
     11:   }
     12:   ());
    0 2 3 |}]

let%expect_test "named class expression" =
  test
    {|
(function () {
  var y = 0;
  class z {
    static z = 0;
  }
  const x = class z {
    static z = 2;
    static {
      let x = 3;
      var y = x;
      this.z = y;
    }
    create(){ return new z }
    getZ(){ return this.z }
  }
  var t = new x;
  console.log(y, x.z, z.z);
})()
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: (function(){
      2:    var v3 = 0;
      3:    class v4{static z = 0;}
      4:    const
      5:     v2 =
      6:       class v5{
      7:          static z = 2;
      8:          static {let v6 = 3; var v7 = v6; this.z = v7;}
      9:          create(){return new v5;}
     10:          getZ(){return this.z;}
     11:        };
     12:    var v1 = new v2;
     13:    console.log(v3, v2.z, v4.z);
     14:   }
     15:   ());
    0 3 0 |}]

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

let%expect_test "functions have local scope" =
  test {|
function f (p) {
  return e
  if (p) {
    function e () {}
    e ();
  }
}
|};
  [%expect
    {|
        $ cat "test.min.js"
          1: function f(v1){return e; if(v1){function v2(){} v2();}} |}]

let%expect_test "import" =
  let test ?(module_ = false) js_prog =
    let name = if module_ then "test.mjs" else "test.js" in
    let js_file = js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name in
    let js_min_file =
      js_file
      |> jsoo_minify
           ~flags:[ "--enable"; "stable_var"; "--enable"; "shortvar" ]
           ~pretty:true
    in
    print_file (Filetype.path_of_js_file js_min_file);
    let js_min_file2 =
      js_file
      |> jsoo_minify
           ~flags:[ "--enable"; "stable_var"; "--disable"; "shortvar" ]
           ~pretty:true
    in
    print_file (Filetype.path_of_js_file js_min_file2);
    check_javascript js_file |> print_endline
  in
  let t = test ~module_:true in
  t {|
import defaultExport from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import v1 from "./module-name.mjs";
    $ cat "test.min.js"
      1: import defaultExport from "./module-name.mjs"; |}];
  t {|
import * as name from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import * as v1 from "./module-name.mjs";
    $ cat "test.min.js"
      1: import * as name from "./module-name.mjs"; |}];
  t {|
import { export1 } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { export1 as v1 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import { export1 } from "./module-name.mjs"; |}];
  t {|
import { export1 as alias1 } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { export1 as v1 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import { export1 as alias1 } from "./module-name.mjs"; |}];
  t {|
import { default as alias } from "module-name";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { default as v1 } from "module-name";
    $ cat "test.min.js"
      1: import { default as alias } from "module-name"; |}];
  t {|
import { export1, export2 } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { export1 as v1, export2 as v2 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import { export1, export2 } from "./module-name.mjs"; |}];
  t {|
import { export1, export2 as alias2, /* … */ } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { export1 as v2, export2 as v1 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import { export1, export2 as alias2 } from "./module-name.mjs"; |}];
  t {|
import { "string name" as alias } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import { "string name" as v1 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import { "string name" as alias } from "./module-name.mjs"; |}];
  t {|
import defaultExport, { export1, /* … */ } from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import v1, { export1 as v2 } from "./module-name.mjs";
    $ cat "test.min.js"
      1: import defaultExport, { export1 } from "./module-name.mjs"; |}];
  t {|
import defaultExport, * as name from "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import v1, * as v2 from "./module-name.mjs";
    $ cat "test.min.js"
      1: import defaultExport, * as name from "./module-name.mjs"; |}];
  t {|
import "./module-name.mjs";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: import "./module-name.mjs";
    $ cat "test.min.js"
      1: import "./module-name.mjs"; |}]

let%expect_test "export" =
  let test ?(module_ = false) js_prog =
    try
      let name = if module_ then "test.mjs" else "test.js" in
      let js_file = js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name in
      let js_min_file =
        js_file
        |> jsoo_minify
             ~flags:[ "--enable"; "stable_var"; "--enable"; "shortvar" ]
             ~pretty:true
      in
      print_file (Filetype.path_of_js_file js_min_file);
      check_javascript js_file |> print_endline
    with e -> print_endline (Printexc.to_string e)
  in
  let t = test ~module_:true in
  t {|
var name1, nameN;
export { name1, /* …, */ nameN };
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: var name1, nameN; export { name1, nameN }; |}];
  t
    {|
var variable1, variable2, nameN;
export { variable1 as name1, variable2 as name2, /* …, */ nameN };
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: var variable1, variable2, nameN;
      2: export { variable1 as name1, variable2 as name2, nameN }; |}];
  t {|
var variable1;
export { variable1 as "string name" };
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: var variable1; export { variable1 as "string name" }; |}];
  t {|
var name1
export { name1 as default /*, … */ };
|};
  [%expect {|
    $ cat "test.min.js"
      1: var name1; export { name1 as default }; |}];
  t {|
export * from "module-name";
|};
  [%expect {|
    $ cat "test.min.js"
      1: export * from "module-name"; |}];
  t {|
export * as name1 from "module-name";
|};
  [%expect {|
    $ cat "test.min.js"
      1: export * as name1 from "module-name"; |}];
  t {|
export { name1, /* …, */ nameN } from "module-name";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: export { name1, nameN } from "module-name"; |}];
  t
    {|
export { import1 as name1, import2 as name2, /* …, */ nameN } from "module-name";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: export { import1 as name1, import2 as name2, nameN } from "module-name"; |}];
  t {|
export { default, /* …, */ } from "module-name";
|};
  [%expect {|
    $ cat "test.min.js"
      1: export { default } from "module-name"; |}];
  t {|
export { default as name1 } from "module-name";
|};
  [%expect
    {|
    $ cat "test.min.js"
      1: export { default as name1 } from "module-name"; |}];
  t {|
export default expression;
|};
  [%expect {|
    $ cat "test.min.js"
      1: export default expression; |}];
  t {|
export default ({obj : 2});
|};
  [%expect {|
    $ cat "test.min.js"
      1: export default ({obj: 2}); |}];
  t {|
export default function functionName() { /* … */ }
|};
  [%expect {|
    $ cat "test.min.js"
      1: export default function v1(){}
    |}];
  t {|
export default class ClassName { /* … */ }|};
  [%expect {|
    $ cat "test.min.js"
      1: export default class v1{} |}];
  t {|
export default function* generatorFunctionName() { /* … */ }|};
  [%expect {|
    $ cat "test.min.js"
      1: export default function* v1(){}
    |}];
  t {|
export default function () { /* … */ }|};
  [%expect {|
    $ cat "test.min.js"
      1: export default function(){} |}];
  t {|
export default class { /* … */ }|};
  [%expect {|
    $ cat "test.min.js"
      1: export default class{} |}];
  t {|
export default function* () { /* … */ }
|};
  [%expect {|
    $ cat "test.min.js"
      1: export default function*(){} |}];
  t {| export let name1, name2/*, … */; // also var |};
  [%expect {|
    $ cat "test.min.js"
      1: export let name1, name2; |}];
  t {| export const name1 = 1, name2 = 2/*, … */; // also var, let |};
  [%expect {|
    $ cat "test.min.js"
      1: export const name1 = 1, name2 = 2; |}];
  t {| export function functionName() { /* … */ } |};
  [%expect {|
    $ cat "test.min.js"
      1: export function functionName(){} |}];
  t {| export class ClassName { /* … */ } |};
  [%expect {|
    $ cat "test.min.js"
      1: export class ClassName{} |}];
  t {| export function* generatorFunctionName() { /* … */ } |};
  [%expect
    {|
    $ cat "test.min.js"
      1: export function* generatorFunctionName(){} |}];
  t {| export const { name1, name2: bar } = o; |};
  [%expect {|
    $ cat "test.min.js"
      1: export const {name1, name2: bar} = o; |}];
  t {| export const [ name1, name2 ] = array; |};
  [%expect {|
    $ cat "test.min.js"
      1: export const [name1, name2] = array; |}]
