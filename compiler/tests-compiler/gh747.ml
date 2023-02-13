(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
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

open Js_of_ocaml_compiler
open! Stdlib
open Util

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let ocaml_prog =
        {|
let greeting = "hello world";;
print_endline greeting;;
let greeting = "hello world with unicode: Ɋ";;
print_endline greeting;;

let unicodeLength = String.length (String.make (Random.int 30) 'i');;
print_endline ("String.length(\"Ɋ\") should be two:" ^ string_of_int(unicodeLength));;
print_endline(String.make 1 "Ɋ".[0] ^ String.make 1 "Ɋ".[1]);;
|}
      in
      let ocaml_file =
        ocaml_prog
        |> Filetype.ocaml_text_of_string
        |> Filetype.write_ocaml ~name:"test.ml"
      in

      let js_file =
        ocaml_file
        |> compile_ocaml_to_cmo ~debug:true
        |> compile_cmo_to_javascript
             ~flags:[ "--debug-info" ]
             ~pretty:true
             ~sourcemap:true
      in
      let () = print_file (Filetype.path_of_js_file js_file) in
      ());
  [%expect
    {|
    $ cat "test.js"
      1:
      2: //# unitInfo: Provides: Test
      3: //# unitInfo: Requires: Stdlib, Stdlib__Random, Stdlib__String
      4: (function
      5:   (globalThis){
      6:    "use strict";
      7:    var
      8:     runtime = globalThis.jsoo_runtime,
      9:     caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
     10:     /*<<?>>*/ function caml_call1(f, a0){
     11:     return (f.l >= 0 ? f.l : f.l = f.length) == 1
     12:             ? f(a0)
     13:             : runtime.caml_call_gen(f, [a0]);
     14:    }
     15:     /*<<?>>*/ function caml_call2(f, a0, a1){
     16:     return (f.l >= 0 ? f.l : f.l = f.length) == 2
     17:             ? f(a0, a1)
     18:             : runtime.caml_call_gen(f, [a0, a1]);
     19:    }
     20:    var
     21:     global_data = runtime.caml_get_global_data(),
     22:     greeting = caml_string_of_jsbytes("hello world"),
     23:     greeting$0 = caml_string_of_jsbytes("hello world with unicode: \xc9\x8a"),
     24:     Stdlib = global_data.Stdlib,
     25:     Stdlib_Random = global_data.Stdlib__Random,
     26:     Stdlib_String = global_data.Stdlib__String;
     27:     /*<<test.ml:3:0>>*/  /*<<test.ml:3:0>>*/ caml_call1(Stdlib[46], greeting);
     28:     /*<<test.ml:5:0>>*/  /*<<test.ml:5:0>>*/ caml_call1
     29:     (Stdlib[46], greeting$0);
     30:     /*<<test.ml:7:47>>*/ var
     31:      /*<<test.ml:7:47>>*/ _a_ =
     32:        /*<<test.ml:7:47>>*/ caml_call1(Stdlib_Random[5], 30),
     33:      /*<<test.ml:7:34>>*/ unicodeLength =
     34:        /*<<test.ml:7:34>>*/ runtime.caml_ml_string_length
     35:        ( /*<<test.ml:7:34>>*/ caml_call2(Stdlib_String[1], _a_, 105)),
     36:      /*<<test.ml:8:56>>*/ _b_ =
     37:        /*<<test.ml:8:56>>*/ caml_call1(Stdlib[33], unicodeLength),
     38:      /*<<test.ml:8:14>>*/ _c_ =
     39:        /*<<test.ml:8:14>>*/ caml_call2
     40:        (Stdlib[28],
     41:         caml_string_of_jsbytes('String.length("\xc9\x8a") should be two:'),
     42:         _b_);
     43:     /*<<test.ml:8:0>>*/  /*<<test.ml:8:0>>*/ caml_call1(Stdlib[46], _c_);
     44:     /*<<test.ml:9:39>>*/ var
     45:      /*<<test.ml:9:39>>*/ _d_ =
     46:        /*<<test.ml:9:39>>*/ caml_call2(Stdlib_String[1], 1, 138),
     47:      /*<<test.ml:9:14>>*/ _e_ =
     48:        /*<<test.ml:9:14>>*/ caml_call2(Stdlib_String[1], 1, 201),
     49:      /*<<test.ml:9:13>>*/ _f_ =
     50:        /*<<test.ml:9:13>>*/ caml_call2(Stdlib[28], _e_, _d_);
     51:     /*<<test.ml:9:0>>*/  /*<<test.ml:9:0>>*/ caml_call1(Stdlib[46], _f_);
     52:     /*<<test.ml:9:0>>*/  /*<<test.ml:9:0>>*/ var
     53:     Test = [0, greeting$0, unicodeLength];
     54:     /*<<test.ml:9:0>>*/  /*<<test.ml:9:0>>*/ runtime.caml_register_global
     55:     (8, Test, "Test");
     56:     /*<<test.ml:9:0>>*/ return;
     57:    /*<<?>>*/ }
     58:   (globalThis));
     59:
     60: //# sourceMappingURL=test.map |}]
