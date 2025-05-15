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

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
var xx = 1;

function f () {
    xx = 2;
    try {
        throw 1
    } catch (xx) {
        var xx = 3
    };
    return xx;
}

function g () {
    var xx = 2;
    return xx;
}
console.log("xx =", xx);
console.log("f() =", f());
console.log("xx =", xx);
console.log("g() =", g());
console.log("xx =", xx);
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      run_javascript js_file |> print_endline;
      [%expect
        {|
        $ cat "test.js"
          1:
          2: var xx = 1;
          3:
          4: function f () {
          5:     xx = 2;
          6:     try {
          7:         throw 1
          8:     } catch (xx) {
          9:         var xx = 3
         10:     };
         11:     return xx;
         12: }
         13:
         14: function g () {
         15:     var xx = 2;
         16:     return xx;
         17: }
         18: console.log("xx =", xx);
         19: console.log("f() =", f());
         20: console.log("xx =", xx);
         21: console.log("g() =", g());
         22: console.log("xx =", xx);
        xx = 1
        f() = 2
        xx = 1
        g() = 2
        xx = 1 |}];
      print_file (Filetype.path_of_js_file js_min_file);
      run_javascript js_min_file |> print_endline;
      [%expect
        {|
        $ cat "test.min.js"
          1: var
          2: xx=1;function
          3: f(){f=2;try{throw 1}catch(f){var
          4: f=3}return f}function
          5: g(){var
          6: a=2;return a}console.log("xx =",xx);console.log("f() =",f());console.log("xx =",xx);console.log("g() =",g());console.log("xx =",xx);
        xx = 1
        f() = 2
        xx = 1
        g() = 2
        xx = 1
        |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog = {|
var a = "toto";
function f (e){ return a; }
|} in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: var a = "toto";
          3: function f (e){ return a; }
        $ cat "test.min.js"
          1: var
          2: a="toto";function
          3: f(b){return a} |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
a = function () { return 0 }
try { throw 1; } catch (xx) { a(0) }
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: a = function () { return 0 }
          3: try { throw 1; } catch (xx) { a(0) }
        $ cat "test.min.js"
          1: a=function(){return 0};try{throw 1}catch(f){a(0)}
        |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
a = function (yyyy) {
try { var xxxxx = 3; var bbb = 2; throw 1; } catch (xx) { const bbb = a(0) } }
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: a = function (yyyy) {
          3: try { var xxxxx = 3; var bbb = 2; throw 1; } catch (xx) { const bbb = a(0) } }
        $ cat "test.min.js"
          1: a=function(d){try{var
          2: c=3,b=2;throw 1}catch(f){const
          3: b=a(0)}};
        |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
a = function (aaa,b,c,yyy) {
        if (true) { let xxx = 2; var y = 3; return xxx + xxx }
        else { let xxx = 3; let aaa = xxx; return xxx * yyy }
        }
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: a = function (aaa,b,c,yyy) {
          3:         if (true) { let xxx = 2; var y = 3; return xxx + xxx }
          4:         else { let xxx = 3; let aaa = xxx; return xxx * yyy }
          5:         }
        $ cat "test.min.js"
          1: a=function(a,b,c,d){if(true){let
          2: a=2;var
          3: e=3;return a+a}else{let
          4: a=3,b=a;return a*d}};
        |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
        var long1 = 1;
        let long2 = 2;
        const long3 = 3;
        function f () {
          var long1 = 1;
          let long2 = 2;
          const long3 = 3;
        }
        |}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2:         var long1 = 1;
          3:         let long2 = 2;
          4:         const long3 = 3;
          5:         function f () {
          6:           var long1 = 1;
          7:           let long2 = 2;
          8:           const long3 = 3;
          9:         }
         10:
        $ cat "test.min.js"
          1: var
          2: long1=1;let
          3: a=2;const
          4: b=3;function
          5: f(){var
          6: a=1;let
          7: b=2;const
          8: c=3} |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
  function f() {
    const v = 2;
    if(true) {
      var x = v;
      { const v = x + 1 }
    }
  }
        |}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2:   function f() {
          3:     const v = 2;
          4:     if(true) {
          5:       var x = v;
          6:       { const v = x + 1 }
          7:     }
          8:   }
          9:
        $ cat "test.min.js"
          1: function
          2: f(){const
          3: a=2;if(true){var
          4: b=a;const
          5: c=b+1}} |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
(function () {
  class longname {
    longname() {
      const y = 2;
      return v
    }
  };
  const w = y
})()
        |}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: (function () {
          3:   class longname {
          4:     longname() {
          5:       const y = 2;
          6:       return v
          7:     }
          8:   };
          9:   const w = y
         10: })()
         11:
        $ cat "test.min.js"
          1: (function(){class
          2: a{longname(){const
          3: a=2;return v}}const
          4: b=y}()); |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
function f () {
  const c = 2;
  return function () {
    var c = c + 2;
    return c
  }
}        |}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: function f () {
          3:   const c = 2;
          4:   return function () {
          5:     var c = c + 2;
          6:     return c
          7:   }
          8: }
        $ cat "test.min.js"
          1: function
          2: f(){const
          3: a=2;return function(){var
          4: a=a+2;return a}} |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let js_prog =
        {|
function test() {
  var x = {a:1,b:2}
  function f (a, b = x.b) {
    return (a + b);
  }
  console.log(f(1));
}
test()
|}
      in
      let js_file =
        js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
      in
      let js_min_file =
        js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
      in
      print_file (Filetype.path_of_js_file js_file);
      print_file (Filetype.path_of_js_file js_min_file);
      [%expect
        {|
        $ cat "test.js"
          1:
          2: function test() {
          3:   var x = {a:1,b:2}
          4:   function f (a, b = x.b) {
          5:     return (a + b);
          6:   }
          7:   console.log(f(1));
          8: }
          9: test()
        $ cat "test.min.js"
          1: function
          2: test(){var
          3: c={a:1,b:2};function
          4: a(a,b=c.b){return a+b}console.log(a(1))}test(); |}];
      print_endline (run_javascript js_min_file);
      [%expect {| 3 |}])

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let minify js_prog =
        let js_file =
          js_prog |> Filetype.js_text_of_string |> Filetype.write_js ~name:"test.js"
        in
        let js_min_file =
          js_file |> jsoo_minify ~flags:[ "--enable"; "shortvar" ] ~pretty:false
        in
        print_file (Filetype.path_of_js_file js_min_file)
      in
      minify {|
function f (x) {
  let {toto} = x;
  return toto;
}
|};
      [%expect
        {|
        $ cat "test.min.js"
          1: function
          2: f(a){let{toto:b}=a;return b} |}];
      minify
        {|
function g(x) {
  var toto, test, tata; 
  for( { toto : tata = test } in x ) {
    console.log(tata);
  }
}
|};
      [%expect
        {|
        $ cat "test.min.js"
          1: function
          2: g(a){var
          3: d,c,b;for({toto:b=c}in
          4: a)console.log(b)} |}];
      minify
        {|
function h(x) {
  var toto;
  for( { toto } in x ) {
    console.log(toto);
  }
}
|};
      [%expect
        {|
        $ cat "test.min.js"
          1: function
          2: h(a){var
          3: b;for({toto:b}in
          4: a)console.log(b)} |}];
      minify
        {|
function h(f) {
  var { toto : c} =  f();
  console.log({ toto : c });
}
|};
      [%expect
        {|
        $ cat "test.min.js"
          1: function
          2: h(a){var{toto:b}=a();console.log({toto:b})} |}])
