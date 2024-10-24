(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

open Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler

let remove_loc p =
  (object
     inherit Js_traverse.map

     method! loc _ = N
  end)
    #program
    p

let p_to_string p =
  let buffer = Buffer.create 17 in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp false;
  let _ = Js_output.program pp p in
  let s = Buffer.contents buffer in
  s

let print ?(debuginfo = true) ?(report = false) ?(invalid = false) ~compact source =
  let stdout = Util.check_javascript_source source in
  (match invalid, stdout with
  | false, _ -> print_endline stdout
  | true, "" -> print_endline "invalid file but node --check didn't complain"
  | true, _ -> ());
  let buffer = Buffer.create (String.length source) in
  let pp = Pretty_print.to_buffer buffer in
  Pretty_print.set_compact pp compact;
  let lexed = Parse_js.Lexer.of_string ~filename:"fake" source in
  try
    let parsed = Parse_js.parse lexed in
    (if debuginfo then Config.Flag.enable else Config.Flag.disable) "debuginfo";
    let _ = Js_output.program pp parsed in
    let s = Buffer.contents buffer in
    print_endline s;
    (let lexed = Parse_js.Lexer.of_string ~filename:"fake" s in
     let parsed2 = Parse_js.parse lexed in
     let p1 = remove_loc parsed in
     let p2 = remove_loc parsed2 in
     if not (Poly.equal p1 p2) then print_endline "<roundtrip issue>");
    let stdout = Util.check_javascript_source s in
    (match invalid, stdout with
    | false, _ -> print_endline stdout
    | true, "" -> print_endline "invalid file but node --check didn't complain"
    | true, _ -> ());
    print_endline stdout
  with Parse_js.Parsing_error pi as e ->
    if report
    then
      Printf.printf
        "cannot parse js (from l:%d, c:%d)@."
        pi.Parse_info.line
        pi.Parse_info.col
    else raise e

let%expect_test "spread operator survives round-trip" =
  print ~compact:true "f(...[1, 2, 3])";
  [%expect {| /*<<fake:1:0>>*/f(...[1,2,3]); |}]

let%expect_test "no postfix addition coalesce" =
  print ~compact:true "a + +b";
  [%expect {|
    /*<<fake:1:0>>*/a+
    +b; |}]

let%expect_test "no postfix subtraction coalesce" =
  print ~compact:true "a - -b";
  [%expect {|
    /*<<fake:1:0>>*/a-
    -b; |}]

let%expect_test "reserved words as fields" =
  print
    ~compact:false
    {|
    x.debugger;
    x.catch;
    x.for;
    x.continue;
    var y = { debugger : 2 }
    var y = { catch : 2 }
    var y = { for : 2 }
    var y = { continue : 2 }
  |};
  [%expect
    {|
     /*<<fake:2:4>>*/ x.debugger;
     /*<<fake:3:4>>*/ x.catch;
     /*<<fake:4:4>>*/ x.for;
     /*<<fake:5:4>>*/ x.continue;
    var y =  /*<<fake:6:10>>*/ {debugger: 2};
    var y =  /*<<fake:7:10>>*/ {catch: 2};
    var y =  /*<<fake:8:10>>*/ {for: 2};
    var y =  /*<<fake:9:10>>*/ {continue: 2}; |}]

let%expect_test "preserve number literals" =
  print
    ~compact:false
    {|
     var x = 0xf_fff;
     var x = 0Xffff;
     var y = 0o7_1223;
     var y = 0O7123;
     var y = 0b1_1001;
     var y = 0B11001;
     var y = 071923;
     var y = 07123;
     var z = 0.0;
     var z = 0.;
     var t = 1.0e-3;
     var t = 1.0E+3;
     var t = 1e-3;
     var t = 1E+3; |};
  [%expect
    {|
    var x =  /*<<fake:2:11>>*/ 0xf_fff;
    var x =  /*<<fake:3:11>>*/ 0Xffff;
    var y =  /*<<fake:4:11>>*/ 0o7_1223;
    var y =  /*<<fake:5:11>>*/ 0O7123;
    var y =  /*<<fake:6:11>>*/ 0b1_1001;
    var y =  /*<<fake:7:11>>*/ 0B11001;
    var y =  /*<<fake:8:11>>*/ 071923;
    var y =  /*<<fake:9:11>>*/ 07123;
    var z =  /*<<fake:10:11>>*/ 0.0;
    var z =  /*<<fake:11:11>>*/ 0.;
    var t =  /*<<fake:12:11>>*/ 1.0e-3;
    var t =  /*<<fake:13:11>>*/ 1.0E+3;
    var t =  /*<<fake:14:11>>*/ 1e-3;
    var t =  /*<<fake:15:11>>*/ 1E+3; |}]

let%expect_test "preserve number literals in property_name" =
  print
    ~compact:false
    {|
    var number_as_key = { 100000000000000000000 : 2 };
    var number_as_key = { 100000000000000000000n : 2 };
 |};
  [%expect
    {|
    var number_as_key =  /*<<fake:2:22>>*/ {100000000000000000000: 2};
    var number_as_key =  /*<<fake:3:22>>*/ {100000000000000000000n: 2}; |}]

let%expect_test "ops" =
  print
    ~report:true
    ~compact:false
    {|
    a += a;
    b ||= true;
    c **= b ** 2;
    1 ** 2;
    (-1) ** 2;
    -(1 ** 2);
    f ??= fw;
    g = c || (a ?? b) || c;
    g = (c || a) ?? (b || c);
    g = c && (a ?? b) && c;
    g = (c && a) ?? (b && c);
    y = a ?? b ?? c ?? d

    y = a?.b?.s?.[a] ?? c ?? d

    a?.b
    a?.[b]
    a?.(b)
 |};
  (* FIXME: parsing & parens  *)
  [%expect
    {|
    /*<<fake:2:4>>*/ a += a;
    /*<<fake:3:4>>*/ b ||= true;
    /*<<fake:4:4>>*/ c **= b ** 2;
    /*<<fake:5:4>>*/ 1 ** 2;
    /*<<fake:6:4>>*/ (- 1) ** 2;
    /*<<fake:7:4>>*/ - (1 ** 2);
    /*<<fake:8:4>>*/ f ??= fw;
    /*<<fake:9:4>>*/ g = c || (a ?? b) || c;
    /*<<fake:10:4>>*/ g = (c || a) ?? (b || c);
    /*<<fake:11:4>>*/ g = c && (a ?? b) && c;
    /*<<fake:12:4>>*/ g = (c && a) ?? (b && c);
    /*<<fake:13:4>>*/ y = a ?? b ?? c ?? d;
    /*<<fake:15:4>>*/ y = a?.b?.s?.[a] ?? c ?? d;
    /*<<fake:17:4>>*/ a?.b;
    /*<<fake:18:4>>*/ a?.[b];
    /*<<fake:19:4>>*/ a?.(b); |}]

let%expect_test "arrow" =
  print
    ~report:true
    ~compact:false
    {|
    var a = (x => x + 2)
    var a = (() => 2);
    var a = ((x) => x + 2);
    var a = ((x,y) => x + y);

    var a = (x => { x + 2 });
    var a = (() => { 2 });
    var a = ((x) => { x + 2 });

    var a = ((x = 1 / 2) => x + 10 );

    var a = ((x = /qwe/g ) => x + 10 );


    var a = x => y => x + y
    var a = x => (y => x + y)

    var a = async x => y
    var a = async (a,b) => a + b
 |};

  [%expect
    {|
    var a =  /*<<fake:2:10>>*/ x=> /*<<fake:2:18>>*/ x + 2 /*<<fake:2:23>>*/ ;
    var a =  /*<<fake:3:10>>*/ ()=> /*<<fake:3:19>>*/ 2 /*<<fake:3:20>>*/ ;
    var a =  /*<<fake:4:10>>*/ x=> /*<<fake:4:20>>*/ x + 2 /*<<fake:4:25>>*/ ;
    var
     a =  /*<<fake:5:10>>*/ (x, y)=> /*<<fake:5:22>>*/ x + y /*<<fake:5:27>>*/ ;
    var a =  /*<<fake:7:10>>*/ x=>{ /*<<fake:7:20>>*/ x + 2; /*<<fake:7:13>>*/ };
    var a =  /*<<fake:8:10>>*/ ()=>{ /*<<fake:8:21>>*/ 2; /*<<fake:8:13>>*/ };
    var a =  /*<<fake:9:10>>*/ x=>{ /*<<fake:9:22>>*/ x + 2; /*<<fake:9:13>>*/ };
    var
     a =
        /*<<fake:11:10>>*/ (x =  /*<<fake:11:16>>*/ 1 / 2)=> /*<<fake:11:28>>*/ x + 10 /*<<fake:11:34>>*/ ;
    var
     a =
        /*<<fake:13:10>>*/ (x =  /*<<fake:13:16>>*/ /qwe/g)=> /*<<fake:13:30>>*/ x + 10 /*<<fake:13:36>>*/ ;
    var
     a =
        /*<<fake:16:10>>*/ x=>
           /*<<fake:16:17>>*/ y=> /*<<fake:16:22>>*/ x + y /*<<fake:16:27>>*/ ;
    var
     a =
        /*<<fake:17:10>>*/ x=>
           /*<<fake:17:17>>*/ y=> /*<<fake:17:23>>*/ x + y /*<<fake:17:28>>*/  /*<<fake:17:29>>*/ ;
    var
     a =  /*<<fake:19:10>>*/ async x=> /*<<fake:19:23>>*/ y /*<<fake:19:24>>*/ ;
    var
     a =
        /*<<fake:20:10>>*/ async (a, b)=>
           /*<<fake:20:27>>*/ a + b /*<<fake:20:32>>*/ ; |}]

let%expect_test "trailing comma" =
  (* GH#989 *)
  print
    ~report:true
    ~compact:false
    {|

// Provides: rehb_new_face
function rehb_new_face(
  _fontName /*: string */,
) {
  return undefined;
}

// Provides: rehb_shape
// Requires: caml_to_js_string
function rehb_shape(_face /*: fk_face */, text /*: string */) {
  var str = caml_to_js_string(text);
  var ret = str.split("").map(function mapper(_char) {
      return [/* <jsoo_empty> */ 0, /* glyphId */ 0, /* cluster */ 0];
    });

  // Adding the leading `0` to make it a jsoo compatible array
  ret.unshift(0);
  return ret;
}
 |};

  [%expect
    {|
    function rehb_new_face(_fontName){
      /*<<fake:7:2>>*/ return undefined /*<<fake:7:18>>*/ ;
     /*<<fake:8:0>>*/ }
    function rehb_shape(_face, text){
     var str =  /*<<fake:13:10>>*/  /*<<fake:13:12>>*/ caml_to_js_string(text);
     var
      ret =
         /*<<fake:14:10>>*/  /*<<fake:14:12>>*/ str.split("").map
         (function mapper(_char){
            /*<<fake:15:6>>*/ return [0, 0, 0] /*<<fake:15:69>>*/ ;
           /*<<fake:14:30>>*/ });
      /*<<fake:19:2>>*/ ret.unshift(0);
      /*<<fake:20:2>>*/ return ret /*<<fake:20:12>>*/ ;
     /*<<fake:21:0>>*/ } |}]

let%expect_test "rest parameters" =
  (* GH#1031 *)
  print
    ~report:true
    ~compact:false
    {|
      api_obj[key_module][key_func] = function(...args) {
        return checkIfInitialized().then(function() {
          return callWithProto(api_json[key_module][key_func], args);
        });
      };
 |};

  [%expect
    {|
    /*<<fake:2:6>>*/ api_obj[key_module][key_func] =
    function(...args){
      /*<<fake:3:8>>*/ return  /*<<fake:3:15>>*/ checkIfInitialized().then
             (function(){
                /*<<fake:4:10>>*/ return  /*<<fake:4:17>>*/ callWithProto
                       (api_json[key_module][key_func], args) /*<<fake:4:68>>*/ ;
               /*<<fake:3:41>>*/ }) /*<<fake:5:10>>*/ ;
     /*<<fake:2:38>>*/ }; |}]

let%expect_test "async/await" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
         async function compile(src)
         {
           const glslangModule = await import(
             "https://unpkg.com/@webgpu/glslang@0.0.7/web/glslang.js"
           );
           const glslang = await glslangModule.default();
           return glslang.compileGLSL(src, "compute");
         }


    async
    function test() { }

    async function test() { }

    async
    function* test() { }

    async function * test() { }

    1 + async function * test() { }

 |};

  [%expect
    {|
    async function compile(src){
     const
      glslangModule =
         /*<<fake:4:31>>*/ await
         /*<<fake:4:39>>*/ import
         ("https://unpkg.com/@webgpu/glslang@0.0.7/web/glslang.js");
     const
      glslang =
         /*<<fake:7:25>>*/ await  /*<<fake:7:33>>*/ glslangModule.default();
      /*<<fake:8:11>>*/ return  /*<<fake:8:18>>*/ glslang.compileGLSL
             (src, "compute") /*<<fake:8:53>>*/ ;
     /*<<fake:2:9>>*/ }
     /*<<fake:12:4>>*/ async;
    function test(){ /*<<fake:13:22>>*/ }
    async function test(){ /*<<fake:15:4>>*/ }
     /*<<fake:17:4>>*/ async;
    function* test(){ /*<<fake:18:4>>*/ }
    async function* test(){ /*<<fake:20:4>>*/ }
     /*<<fake:22:4>>*/ 1 + async function* test(){ /*<<fake:22:8>>*/ }; |}]

let%expect_test "get/set property" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
     var x = {
       get prop() { return 3 },
       set prop(x) { return x == 2 },
       a : 4,
       b() { return 5},
       *e() { return 5},
       async e() { return 5},
       async* e() { return 5},
       ["field" + 1]: 3
     };

 |};

  [%expect
    {|
    var
     x =
        /*<<fake:2:11>>*/ {get prop(){
          /*<<fake:3:20>>*/ return 3 /*<<fake:3:28>>*/ ;
         /*<<fake:3:7>>*/ },
        set prop(x){
          /*<<fake:4:21>>*/ return x == 2 /*<<fake:4:34>>*/ ;
         /*<<fake:4:7>>*/ },
        a: 4,
        b(){ /*<<fake:6:13>>*/ return 5 /*<<fake:6:21>>*/ ; /*<<fake:6:7>>*/ },
        * e(){ /*<<fake:7:14>>*/ return 5 /*<<fake:7:22>>*/ ; /*<<fake:7:7>>*/ },
        async e(){
          /*<<fake:8:19>>*/ return 5 /*<<fake:8:27>>*/ ;
         /*<<fake:8:7>>*/ },
        async* e(){
          /*<<fake:9:20>>*/ return 5 /*<<fake:9:28>>*/ ;
         /*<<fake:9:7>>*/ },
        ["field" + 1]: 3}; |}]

let%expect_test "assignment pattern" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
    var x, y, rest;
    var [x,y] = [1,2]
    var [x,y,...rest] = [1,2, ...o]

    var {x,y} = {x:1,y:2}
    var {x,y,...rest} = {x:1,y:2,...o};

    [x,y] = [1,2];
    [x,y,...rest] = [1,2];

    ({x,y} = {x:1,y:2});
    ({x,y,...rest} = {x:1,y:2});

    for([a,b,{c,d=e,[f]:[g,h,a,i,j]}] in 3);

    for([a,b,{c,d=e,[f]:[g,h,a,i,j]}] of 3);

 |};

  [%expect
    {|
    var x, y, rest;
    var [x, y] = /*<<fake:3:14>>*/  [1, 2];
    var [x, y, ...rest] = /*<<fake:4:22>>*/  [1, 2, ...o];
    var {x, y} = /*<<fake:6:14>>*/  {x: 1, y: 2};
    var {x, y, ...rest} = /*<<fake:7:22>>*/  {x: 1, y: 2, ...o};
     /*<<fake:9:4>>*/ [x, y] = [1, 2];
     /*<<fake:10:4>>*/ [x, y, ...rest] = [1, 2];
     /*<<fake:12:4>>*/ ({x, y} = {x: 1, y: 2});
     /*<<fake:13:4>>*/ ({x, y, ...rest} = {x: 1, y: 2});
     /*<<fake:15:4>>*/ for([a, b, {c, d = e, [f]: [g, h, a, i, j]}] in 3) ;
     /*<<fake:17:4>>*/ for([a, b, {c, d = e, [f]: [g, h, a, i, j]}] of 3) ; |}]

let%expect_test "for loops" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
    for(x in 3);
    for(x of 3);
    async function f(x) {
    for await(x of 3);
    }
 |};

  [%expect
    {|
     /*<<fake:2:4>>*/ for(x in 3) ;
     /*<<fake:3:4>>*/ for(x of 3) ;
    async function f(x){ /*<<fake:5:4>>*/ for await(x of 3) ; /*<<fake:4:4>>*/ } |}]

let%expect_test "string template" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
    var s = `asdte`
    var s = `asd ${ test } te`

    var s = tag`asd ${ test } te`

    var s = `asd ${ f(`space ${test} space`, 32) } te`
 |};

  [%expect
    {|
    var s =  /*<<fake:2:10>>*/ `asdte`;
    var s =  /*<<fake:3:10>>*/ `asd ${test} te`;
    var s =  /*<<fake:5:10>>*/  /*<<fake:5:12>>*/ tag`asd ${test} te`;
    var
     s =
        /*<<fake:7:10>>*/ `asd ${ /*<<fake:7:20>>*/ f(`space ${test} space`, 32)} te`; |}]

let%expect_test "from keyword" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
({key:"from",
 value:
   function from(field,get)
     {if(!get)get=function get(x){return x;};
      return this.compute([field],function(state){return get(state.field(field));});}}) |};
  [%expect
    {|
    /*<<fake:2:0>>*/ ({key: "from",
     value:
     function from(field, get){
       /*<<fake:5:6>>*/ if(! get)
        /*<<fake:5:14>>*/ get =
        function get(x){
          /*<<fake:5:34>>*/ return x /*<<fake:5:42>>*/ ;
         /*<<fake:5:18>>*/ };
       /*<<fake:6:6>>*/ return  /*<<fake:6:13>>*/ this.compute
              ([field],
               function(state){
                 /*<<fake:6:50>>*/ return  /*<<fake:6:57>>*/ get
                        ( /*<<fake:6:61>>*/ state.field(field)) /*<<fake:6:80>>*/ ;
                /*<<fake:6:34>>*/ }) /*<<fake:6:83>>*/ ;
      /*<<fake:4:3>>*/ }}); |}]

let%expect_test "new.target" =
  (* GH#1017 *)
  print ~report:true ~compact:false {|
    var s = new.target
 |};

  [%expect {|
    var s =  /*<<fake:2:10>>*/ new.target; |}]

let%expect_test "super" =
  (* GH#1017 *)
  print
    ~report:true
    ~compact:false
    {|
class x extends p {
    constructor() {
      super(a,b,c);
    }
    foo() {

      var s = super[d]
      var s = super.d
    }

    static bar() {

      var s = super[d]
      var s = super.d
    }
   x = 3

   static y = 5

   #z = 6

   static #t = 2

   static { var x = 3 }
}
 |};

  [%expect
    {|
    class x extends p {
       constructor(){ /*<<fake:4:6>>*/ super(a, b, c); /*<<fake:3:4>>*/ }
       foo(){
        var s =  /*<<fake:8:12>>*/ super[d];
        var s =  /*<<fake:9:12>>*/ super.d;
        /*<<fake:6:4>>*/ }
       static
       bar(){
        var s =  /*<<fake:14:12>>*/ super[d];
        var s =  /*<<fake:15:12>>*/ super.d;
        /*<<fake:12:11>>*/ }
       x =  /*<<fake:17:5>>*/ 3;
       static y =  /*<<fake:19:12>>*/ 5;
       #z =  /*<<fake:21:6>>*/ 6;
       static #t =  /*<<fake:23:13>>*/ 2;
       static {var x =  /*<<fake:25:18>>*/ 3;}
     } |}]

let%expect_test "ite" =
  print
    ~debuginfo:false
    ~compact:false
    {|
if(a) {
  this(is,not,small)
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
} else if (b) {
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
  this(is,not,small)
} else if (c) {
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
} else {
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
  this(is,not,small) + this(is,bigger);
}
|};
  [%expect
    {|
    if(a){
     this(is, not, small);
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
    }
    else if(b){
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
     this(is, not, small);
    }
    else if(c){
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
    }
    else{
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
     this(is, not, small) + this(is, bigger);
    } |}]

let%expect_test "consise body can be anything that doesn't start with curly brackets" =
  print
    ~debuginfo:false
    ~compact:false
    ~report:true
    {|
var e = function () { } ()

var e = f(x => function () { } ())

var e = f(x => new class f {})
|};
  [%expect
    {|
    var e = function(){}();
    var e = f(x=>function(){}());
    var e = f(x=>new class f{}); |}]

let%expect_test "new kw with no arguments should be preserve" =
  print
    ~debuginfo:false
    ~compact:false
    ~report:true
    {|
var e = new f
var e = new f()
var e = new class f {}
var e = new (class f {})
|};
  [%expect
    {|
    var e = new f; var e = new f(); var e = new class f{}; var e = new class f{}; |}]

let%expect_test "assignment targets" =
  print
    ~debuginfo:false
    ~compact:false
    ~report:true
    {|
 [a,b,c, {a,b}] = [];
 [[[x = 5]], {a,b}, ...rest] = [];
 ({a: [a,b] = f(), b = 3, ...rest} = {});
|};
  [%expect
    {|
    [a, b, c, {a, b}] = [];
    [[[x = 5]], {a, b}, ...rest] = [];
    ({a: [a, b] = f(), b = 3, ...rest} = {}); |}]

let%expect_test "as keyword" =
  print
    ~debuginfo:false
    ~compact:false
    ~report:true
    {|
const as = () => () => ts(void 0, void 0, void 0, function* () {})
|};
  [%expect {|
    const as = ()=>()=>ts(void 0, void 0, void 0, function*(){}); |}]

let%expect_test "error reporting" =
  (try
     print ~invalid:true ~compact:false {|
    var x = 2;
    {
    var = 5;
    }
    |}
   with Parse_js.Parsing_error pi ->
     Printf.printf
       "cannot parse js (from l:%d, c:%d)@."
       pi.Parse_info.line
       pi.Parse_info.col);
  [%expect {|
    cannot parse js (from l:4, c:8)@. |}]

(* check that the locations are correct and that the lexer is captures all the token *)
let check_vs_string s toks =
  let rec space a b =
    if a >= b
    then ()
    else
      match s.[a] with
      | ' ' | '\n' | '\t' -> space (succ a) b
      | c -> Printf.printf "pos:%d, expecting space until %d, found %C\n" a b c
  in
  let text pos str =
    let strlen = String.length str in
    if strlen + pos > String.length s
    then
      Printf.printf
        "pos: %d, expecting %S, found %S\n"
        pos
        str
        (String.sub s ~pos ~len:(String.length s - pos))
    else
      let sub = String.sub s ~pos ~len:strlen in
      if String.equal str sub
      then ()
      else Printf.printf "pos: %d, expecting %S, found %S\n" pos str sub
  in
  let rec loop offset pos = function
    | [] -> space pos (String.length s)
    | (Js_token.(T_VIRTUAL_SEMICOLON | T_VIRTUAL_SEMICOLON_DO_WHILE), _) :: rest ->
        loop offset pos rest
    | ((Js_token.T_STRING (_, codepoint_len) as x), loc) :: rest ->
        let p1 = Loc.p1 loc in
        let { Parse_info.idx = codepoint_idx; _ } = Parse_info.t_of_pos p1 in
        let bytes_idx = codepoint_idx - offset in
        let bytes_len =
          let bytes_len = ref 0 in
          for _ = 0 to codepoint_len - 1 do
            let r = String.get_utf_8_uchar s (bytes_idx + !bytes_len) in
            bytes_len := !bytes_len + Uchar.utf_decode_length r
          done;
          !bytes_len
        in
        let offset = offset + (codepoint_len - bytes_len) in
        let _str = Js_token.to_string x in
        space pos bytes_idx;
        let quote_start = s.[bytes_idx] in
        let quote_end = s.[bytes_idx + bytes_len] in
        (match quote_start, quote_end with
        | '"', '"' | '\'', '\'' -> ()
        | a, b ->
            Printf.printf
              "pos:%d+%d, expecting quotes, found %C+%C\n"
              bytes_idx
              (bytes_idx + bytes_len)
              a
              b);
        loop offset (bytes_idx + bytes_len + 1) rest
    | (x, loc) :: rest ->
        let p1 = Loc.p1 loc in
        let { Parse_info.idx; _ } = Parse_info.t_of_pos p1 in
        let idx = idx - offset in
        let str = Js_token.to_string x in
        space pos idx;
        text idx str;
        loop offset (idx + String.length str) rest
  in
  loop 0 0 toks

let parse_print_token ?(invalid = false) ?(extra = false) s =
  let stdout = Util.check_javascript_source s in
  (match invalid, stdout with
  | false, _ -> print_endline stdout
  | true, "" -> print_endline "invalid file but node --check didn't complain"
  | true, _ -> ());
  let lex = Parse_js.Lexer.of_string ~filename:"fake" s in
  let _p, tokens =
    try Parse_js.parse' lex
    with Parse_js.Parsing_error pi as e ->
      Printf.eprintf "cannot parse l:%d:%d@." pi.Parse_info.line pi.Parse_info.col;
      raise e
  in
  check_vs_string s tokens;
  let prev = ref 0 in
  let rec loop tokens =
    match tokens with
    | [ (Js_token.T_EOF, _) ] | [] -> Printf.printf "\n"
    | (tok, loc) :: xs ->
        let p1 = Loc.p1 loc in
        let pos = Parse_info.t_of_pos p1 in
        let s = if extra then Js_token.to_string_extra tok else Js_token.to_string tok in
        (match !prev <> pos.Parse_info.line && pos.Parse_info.line <> 0 with
        | true -> Printf.printf "\n%2d: " pos.Parse_info.line
        | false -> ());
        if pos.Parse_info.line <> 0 then prev := pos.Parse_info.line;
        Printf.printf "%d:%s, " pos.Parse_info.col s;
        loop xs
  in
  loop tokens

let%expect_test "tokens" =
  parse_print_token
    {|
    var a = 42;
    var \u{1ee62} = 42;
    var a = x => x + 2
    var a = () => 2

    var s = `asdte`
    var s = `asd ${ test } te`
    var s = tag`asd ${ test } te`

    var s = `asd ${ f(`space ${test} space`, 32) } te`
|};
  [%expect
    {|
     2: 4:var, 8:a, 10:=, 12:42, 14:;,
     3: 4:var, 8:\u{1ee62}, 18:=, 20:42, 22:;,
     4: 4:var, 8:a, 10:=, 12:x, 14:=>, 17:x, 19:+, 21:2, 0:;,
     5: 4:var, 8:a, 10:=, 12:(, 13:), 15:=>, 18:2, 0:;,
     7: 4:var, 8:s, 10:=, 12:`, 13:asdte, 18:`, 0:;,
     8: 4:var, 8:s, 10:=, 12:`, 13:asd , 17:${, 20:test, 25:}, 26: te, 29:`, 0:;,
     9: 4:var, 8:s, 10:=, 12:tag, 15:`, 16:asd , 20:${, 23:test, 28:}, 29: te, 32:`, 0:;,
    11: 4:var, 8:s, 10:=, 12:`, 13:asd , 17:${, 20:f, 21:(, 22:`, 23:space , 29:${, 31:test, 35:}, 36: space, 42:`, 43:,, 45:32, 47:), 49:}, 50: te, 53:`, 0:;, |}]

let%expect_test "invalid ident" =
  parse_print_token
    ~invalid:true
    {|
    var \uD83B\uDE62 = 42; // invalid surrogate escape sequence
    var \u{1F42B} = 2; // U+1F42B is not a valid id
|};
  [%expect
    {|
     2: 4:var, 8:\uD83B\uDE62, 21:=, 23:42, 25:;, 27:// invalid surrogate escape sequence,
     3: 4:var, 8:\u{1F42B}, 18:=, 20:2, 21:;, 23:// U+1F42B is not a valid id,
    Lexer error: fake:2:8: Illegal Unicode escape
    Lexer error: fake:3:8: Unexpected "\240\159\144\171" is not a valid identifier |}]

let%expect_test "string" =
  parse_print_token
    {|
    var a = "asf";
    var a = "munpiπππqtex";
    var a = "munpiπππqtex";
    var a = "munpiπππqtex";
|};
  [%expect
    {|
    2: 4:var, 8:a, 10:=, 12:"asf", 17:;,
    3: 4:var, 8:a, 10:=, 12:"munpi\207\128\207\128\207\128qtex", 26:;,
    4: 4:var, 8:a, 10:=, 12:"munpi\207\128\207\128\207\128qtex", 26:;,
    5: 4:var, 8:a, 10:=, 12:"munpi\207\128\207\128\207\128qtex", 26:;, |}]

let%expect_test "multiline string" =
  parse_print_token ~invalid:true {|
    42;
    "
    ";
    42
|};
  [%expect
    {|
     2: 4:42, 6:;,
     3: 4:"\n    ",
     4: 5:;,
     5: 4:42, 0:;,
    Lexer error: fake:3:5: Unexpected token ILLEGAL |}];
  parse_print_token {|
    42;
    "\
    ";
    42
|};
  [%expect {|
    2: 4:42, 6:;,
    3: 4:"    ",
    4: 5:;,
    5: 4:42, 0:;, |}];
  parse_print_token ~invalid:true {|
    42;
    "

    ";
    42
|};
  [%expect
    {|
     2: 4:42, 6:;,
     3: 4:"\n\n    ",
     5: 5:;,
     6: 4:42, 0:;,
    Lexer error: fake:3:5: Unexpected token ILLEGAL
    Lexer error: fake:4:0: Unexpected token ILLEGAL |}];
  [%expect {||}]

let%expect_test "multiline comments" =
  parse_print_token {|
//com1
//com2
//com3
|};
  [%expect {|
    2: 0://com1,
    3: 0://com2,
    4: 0://com3, |}];
  parse_print_token {|
/* test */ 42 /* test */
|};
  [%expect {|
    2: 0:/* test */, 11:42, 0:;, 14:/* test */, |}];
  parse_print_token {|
    42
    /*
    "

    */
    42
|};
  [%expect {|
    2: 4:42, 0:;,
    3: 4:/*
       "

       */,
    7: 4:42, 0:;, |}]

let%expect_test "++--" =
  parse_print_token ~extra:true {|
    ++a
    --a
    a++
    a++
|};
  [%expect
    {|
    2: 4:++ (INCR), 6:a (identifier), 0:; (virtual),
    3: 4:-- (DECR), 6:a (identifier), 0:; (virtual),
    4: 4:a (identifier), 5:++ (INCR_NB), 0:; (virtual),
    5: 4:a (identifier), 5:++ (INCR_NB), 0:; (virtual), |}]

let%expect_test "div_or_regexp" =
  parse_print_token
    {|
    1 / 2
    1 + /regexp/
    (b) / denominator
    if(a) { e } /regexp/
    if(b) /regexp/
    +{ } / denominator
    +{ } / denominator[a]
    |};
  [%expect
    {|
    2: 4:1, 6:/, 8:2, 0:;,
    3: 4:1, 6:+, 8:/regexp/,
    4: 4:(, 5:b, 6:), 8:/, 10:denominator, 0:;,
    5: 4:if, 6:(, 7:a, 8:), 10:{, 12:e, 0:;, 14:}, 16:/regexp/, 0:;,
    6: 4:if, 6:(, 7:b, 8:), 10:/regexp/,
    7: 4:+, 5:{, 7:}, 9:/, 11:denominator,
    8: 4:+, 5:{, 7:}, 9:/, 11:denominator, 22:[, 23:a, 24:], 0:;, |}]

let%expect_test "virtual semicolon" =
  parse_print_token
    ~extra:true
    {|
    return;
    return 2
    return
    2
a:while(true){
    continue;
    continue a
    continue
    a

    break;
    break a
    break
    a
}
    throw 2;
    throw 2

    { 1
    2 } 3

    a = b
    ++c

    a = b + c
    (d + e).print()

    do { x } while (true) y
    do ; while (true) y

    async
    function test() { }

    async function test() { }

    async
    function* test() { }

    async function * test() { }

    1 + async function * test() { }

|};
  [%expect
    {|
     2: 4:return, 10:;,
     3: 4:return, 11:2, 0:; (virtual),
     4: 4:return, 0:; (virtual),
     5: 4:2, 0:; (virtual),
     6: 0:a (identifier), 1::, 2:while, 7:(, 8:true, 12:), 13:{,
     7: 4:continue, 12:;,
     8: 4:continue, 13:a (identifier), 0:; (virtual),
     9: 4:continue, 0:; (virtual),
    10: 4:a (identifier), 0:; (virtual),
    12: 4:break, 9:;,
    13: 4:break, 10:a (identifier), 0:; (virtual),
    14: 4:break, 0:; (virtual),
    15: 4:a (identifier), 0:; (virtual),
    16: 0:},
    17: 4:throw, 10:2, 11:;,
    18: 4:throw, 10:2, 0:; (virtual),
    20: 4:{, 6:1, 0:; (virtual),
    21: 4:2, 0:; (virtual), 6:}, 8:3, 0:; (virtual),
    23: 4:a (identifier), 6:=, 8:b (identifier), 0:; (virtual),
    24: 4:++ (INCR), 6:c (identifier), 0:; (virtual),
    26: 4:a (identifier), 6:=, 8:b (identifier), 10:+, 12:c (identifier),
    27: 4:(, 5:d (identifier), 7:+, 9:e (identifier), 10:), 11:., 12:print (identifier), 17:(, 18:), 0:; (virtual),
    29: 4:do, 7:{, 9:x (identifier), 0:; (virtual), 11:}, 13:while, 19:(, 20:true, 24:), 0:; (virtual), 26:y (identifier), 0:; (virtual),
    30: 4:do, 7:;, 9:while, 15:(, 16:true, 20:), 0:; (virtual), 22:y (identifier), 0:; (virtual),
    32: 4:async, 0:; (virtual),
    33: 4:function, 13:test (identifier), 17:(, 18:), 20:{, 22:},
    35: 4:async, 10:function, 19:test (identifier), 23:(, 24:), 26:{, 28:},
    37: 4:async, 0:; (virtual),
    38: 4:function, 12:*, 14:test (identifier), 18:(, 19:), 21:{, 23:},
    40: 4:async, 10:function, 19:*, 21:test (identifier), 25:(, 26:), 28:{, 30:},
    42: 4:1, 6:+, 8:async, 14:function, 23:*, 25:test (identifier), 29:(, 30:), 32:{, 34:}, 0:; (virtual), |}]

let%expect_test _ =
  parse_print_token
    ~extra:true
    {|
function UnexpectedVirtualElement(data) {
    var err = new Error();

    err.type = 'virtual-hyperscript.unexpected.virtual-element';
    err.message =
        'The parent vnode is:\n' +
        errorString(data.parentVnode)
        '\n' +
        'Suggested fix: change your `h(..., [ ... ])` callsite.';
    err.foreignObject = data.foreignObject;
    err.parentVnode = data.parentVnode;

    return err;
}
|};
  [%expect
    {|
     2: 0:function, 9:UnexpectedVirtualElement (identifier), 33:(, 34:data (identifier), 38:), 40:{,
     3: 4:var, 8:err (identifier), 12:=, 14:new, 18:Error (identifier), 23:(, 24:), 25:;,
     5: 4:err (identifier), 7:., 8:type (identifier), 13:=, 15:"virtual-hyperscript.unexpected.virtual-element", 63:;,
     6: 4:err (identifier), 7:., 8:message (identifier), 16:=,
     7: 8:"The parent vnode is:\\n", 33:+,
     8: 8:errorString (identifier), 19:(, 20:data (identifier), 24:., 25:parentVnode (identifier), 36:), 0:; (virtual),
     9: 8:"\\n", 13:+,
    10: 8:"Suggested fix: change your `h(..., [ ... ])` callsite.", 64:;,
    11: 4:err (identifier), 7:., 8:foreignObject (identifier), 22:=, 24:data (identifier), 28:., 29:foreignObject (identifier), 42:;,
    12: 4:err (identifier), 7:., 8:parentVnode (identifier), 20:=, 22:data (identifier), 26:., 27:parentVnode (identifier), 38:;,
    14: 4:return, 11:err (identifier), 14:;,
    15: 0:}, |}]

let%expect_test "annot" =
  parse_print_token
    ~extra:true
    {|
a
//Provides: test
//Just a comment
//Requires: something
//Another comment
a
if (a) {
//Provides: test
b
}
//Provides: test
c
|};
  [%expect
    {|
     2: 0:a (identifier), 0:; (virtual),
     3: 0://Provides: test(annot),
     4: 0://Just a comment,
     5: 0://Requires: something(annot),
     6: 0://Another comment,
     7: 0:a (identifier), 0:; (virtual),
     8: 0:if, 3:(, 4:a (identifier), 5:), 7:{,
     9: 0://Provides: test(annot),
    10: 0:b (identifier), 0:; (virtual),
    11: 0:},
    12: 0://Provides: test(annot),
    13: 0:c (identifier), 0:; (virtual), |}]

let%expect_test _ =
  parse_print_token
    ~extra:true
    {|
Event.prototype.initEvent = function _Event_initEvent(type, bubbles, cancelable) {
    this.type = type
    this.bubbles = bubbles
    this.cancelable = cancelable
}
|};
  [%expect
    {|
    2: 0:Event (identifier), 5:., 6:prototype (identifier), 15:., 16:initEvent (identifier), 26:=, 28:function, 37:_Event_initEvent (identifier), 53:(, 54:type (identifier), 58:,, 60:bubbles (identifier), 67:,, 69:cancelable (identifier), 79:), 81:{,
    3: 4:this, 8:., 9:type (identifier), 14:=, 16:type (identifier), 0:; (virtual),
    4: 4:this, 8:., 9:bubbles (identifier), 17:=, 19:bubbles (identifier), 0:; (virtual),
    5: 4:this, 8:., 9:cancelable (identifier), 20:=, 22:cancelable (identifier), 0:; (virtual),
    6: 0:}, 0:; (virtual), |}]

let%expect_test _ =
  parse_print_token
    ~extra:true
    {|
var y = { async: 35}

var y = async x => x
var y = async => async
|};
  [%expect
    {|
    2: 0:var, 4:y (identifier), 6:=, 8:{, 10:async, 15::, 17:35, 19:}, 0:; (virtual),
    4: 0:var, 4:y (identifier), 6:=, 8:async, 14:x (identifier), 16:=>, 19:x (identifier), 0:; (virtual),
    5: 0:var, 4:y (identifier), 6:=, 8:async, 14:=>, 17:async, 0:; (virtual), |}]

let%expect_test "label parsing" =
  parse_print_token
    ~extra:true
    {|
get:{of:{from:{async:{break async}break from}break of}break get}
|};
  [%expect
    {| 2: 0:get, 3::, 4:{, 5:of, 7::, 8:{, 9:from, 13::, 14:{, 15:async, 20::, 21:{, 22:break, 28:async, 0:; (virtual), 33:}, 34:break, 40:from, 0:; (virtual), 44:}, 45:break, 51:of, 0:; (virtual), 53:}, 54:break, 60:get, 0:; (virtual), 63:}, |}]
