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
     52:    var Test = [0, greeting$0, unicodeLength];
     53:    runtime.caml_register_global(8, Test, "Test");
     54:    return;
     55:    /*<<?>>*/ }
     56:   (globalThis));
     57:
     58: //# sourceMappingURL=test.map |}]

let%expect_test _ =
  with_temp_dir ~f:(fun () ->
      let ocaml_prog =
        {|
external get_config: unit -> string * int * bool = "caml_sys_get_config"
external get_executable_name : unit -> string = "caml_sys_executable_name"
external argv : string array = "%sys_argv"
external big_endian : unit -> bool = "%big_endian"
external word_size : unit -> int = "%word_size"
external int_size : unit -> int = "%int_size"
external max_wosize : unit -> int = "%max_wosize"
external unix : unit -> bool = "%ostype_unix"
external win32 : unit -> bool = "%ostype_win32"
external cygwin : unit -> bool = "%ostype_cygwin"
external get_backend_type : unit -> Sys.backend_type = "%backend_type"

let executable_name = get_executable_name()
let (os_type, _, _) = get_config()
let backend_type = get_backend_type ()
let big_endian = big_endian ()
let word_size = word_size ()
let int_size = int_size ()
let unix = unix ()
let win32 = win32 ()
let cygwin = cygwin ()
let max_array_length = max_wosize ()
let max_floatarray_length = max_array_length / (64 / word_size)
let max_string_length = word_size / 8 * max_array_length - 1

type 'a effect
type exn += Unhandled: 'a effect -> exn

type backtrace_slot =
  | Known_location of {
      is_raise    : bool;
      filename    : string;
      line_number : int;
      start_char  : int;
      end_char    : int;
      is_inline   : bool;
      defname     : string;
    }
  | Unknown_location of {
      is_raise : bool
    }

let format_backtrace_slot pos slot =
  let info is_raise =
    if is_raise then
      if pos = 0 then "Raised at" else "Re-raised at"
    else
      if pos = 0 then "Raised by primitive operation at" else "Called from"
  in
  match slot with
  | Unknown_location l ->
      if l.is_raise then
        (* compiler-inserted re-raise, skipped *) None
      else
        Some (Printf.sprintf "%s unknown location" (info false))
  | Known_location l ->
      Some (Printf.sprintf "%s %s in file \"%s\"%s, line %d, characters %d-%d"
              (info l.is_raise) l.defname l.filename
              (if l.is_inline then " (inlined)" else "")
              l.line_number l.start_char l.end_char)

let print_exception_backtrace outchan backtrace =
  match backtrace with
  | None ->
      Printf.fprintf outchan
        "(Program not linked with -g, cannot print stack backtrace)\n"
  | Some a ->
      for i = 0 to Array.length a - 1 do
        match format_backtrace_slot i a.(i) with
          | None -> ()
          | Some str -> Printf.fprintf outchan "%s\n" str
      done

module Either = struct
  open Either
  let compare ~left ~right e1 e2 = match e1, e2 with
  | Left v1, Left v2 -> left v1 v2
  | Right v1, Right v2 -> right v1 v2
  | Left _, Right _ -> (-1)
  | Right _, Left _ -> 1
end

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
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    $ cat "test.js"
      1:
      2: //# unitInfo: Provides: Test
      3: //# unitInfo: Requires: Stdlib__Printf
      4: (function
      5:   (globalThis){
      6:    "use strict";
      7:    var
      8:     runtime = globalThis.jsoo_runtime,
      9:     caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
     10:     /*<<?>>*/ function caml_call2(f, a0, a1){
     11:     return (f.l >= 0 ? f.l : f.l = f.length) == 2
     12:             ? f(a0, a1)
     13:             : runtime.caml_call_gen(f, [a0, a1]);
     14:    }
     15:     /*<<?>>*/ function caml_call3(f, a0, a1, a2){
     16:     return (f.l >= 0 ? f.l : f.l = f.length) == 3
     17:             ? f(a0, a1, a2)
     18:             : runtime.caml_call_gen(f, [a0, a1, a2]);
     19:    }
     20:     /*<<?>>*/ function caml_call8(f, a0, a1, a2, a3, a4, a5, a6, a7){
     21:     return (f.l >= 0 ? f.l : f.l = f.length) == 8
     22:             ? f(a0, a1, a2, a3, a4, a5, a6, a7)
     23:             : runtime.caml_call_gen(f, [a0, a1, a2, a3, a4, a5, a6, a7]);
     24:    }
     25:     /*<<test.ml:14:22>>*/ var
     26:     global_data = runtime.caml_get_global_data(),
     27:     cst = caml_string_of_jsbytes(""),
     28:     partial = [4, 0, 0, 0, [12, 45, [4, 0, 0, 0, 0]]],
     29:     Stdlib_Printf = global_data.Stdlib__Printf,
     30:      /*<<test.ml:14:22>>*/ executable_name =
     31:        /*<<test.ml:14:22>>*/ runtime.caml_sys_executable_name(0),
     32:     os_type =  /*<<test.ml:15:22>>*/ runtime.caml_sys_get_config(0)[1],
     33:     backend_type = [0, caml_string_of_jsbytes("js_of_ocaml")],
     34:     unix = runtime.caml_sys_const_ostype_unix(0),
     35:     win32 = runtime.caml_sys_const_ostype_win32(0),
     36:     cygwin = runtime.caml_sys_const_ostype_cygwin(0),
     37:     max_array_length = runtime.caml_sys_const_max_wosize(0),
     38:     max_floatarray_length = max_array_length / 2 | 0,
     39:     max_string_length = (4 * max_array_length | 0) - 1 | 0,
     40:      /*<<test.ml:28:12>>*/ Unhandled =
     41:       [248,
     42:        caml_string_of_jsbytes("Test.Unhandled"),
     43:        runtime.caml_fresh_oo_id(0)],
     44:     cst_Raised_at = caml_string_of_jsbytes("Raised at"),
     45:     cst_Re_raised_at = caml_string_of_jsbytes("Re-raised at"),
     46:     cst_Raised_by_primitive_operat =
     47:       caml_string_of_jsbytes("Raised by primitive operation at"),
     48:     cst_Called_from = caml_string_of_jsbytes("Called from"),
     49:     cst_inlined = caml_string_of_jsbytes(" (inlined)"),
     50:     _a_ =
     51:       [0,
     52:        [2,
     53:         0,
     54:         [12,
     55:          32,
     56:          [2,
     57:           0,
     58:           [11,
     59:            caml_string_of_jsbytes(' in file "'),
     60:            [2,
     61:             0,
     62:             [12,
     63:              34,
     64:              [2,
     65:               0,
     66:               [11,
     67:                caml_string_of_jsbytes(", line "),
     68:                [4,
     69:                 0,
     70:                 0,
     71:                 0,
     72:                 [11, caml_string_of_jsbytes(", characters "), partial]]]]]]]]]],
     73:        caml_string_of_jsbytes
     74:         ('%s %s in file "%s"%s, line %d, characters %d-%d')],
     75:     _b_ =
     76:       [0,
     77:        [2, 0, [11, caml_string_of_jsbytes(" unknown location"), 0]],
     78:        caml_string_of_jsbytes("%s unknown location")],
     79:     _c_ = [0, [2, 0, [12, 10, 0]], caml_string_of_jsbytes("%s\n")],
     80:     _d_ =
     81:       [0,
     82:        [11,
     83:         caml_string_of_jsbytes
     84:          ("(Program not linked with -g, cannot print stack backtrace)\n"),
     85:         0],
     86:        caml_string_of_jsbytes
     87:         ("(Program not linked with -g, cannot print stack backtrace)\n")];
     88:    function format_backtrace_slot(pos, slot){
     89:     function info(is_raise){
     90:       /*<<test.ml:46:4>>*/ return is_raise
     91:              ? 0 === pos ? cst_Raised_at : cst_Re_raised_at
     92:              : 0 === pos ? cst_Raised_by_primitive_operat : cst_Called_from;
     93:      /*<<test.ml:49:75>>*/ }
     94:      /*<<test.ml:51:2>>*/ if(0 === slot[0]){
     95:       /*<<test.ml:59:14>>*/ var
     96:       _h_ = slot[5],
     97:       _i_ = slot[4],
     98:       _j_ = slot[3],
     99:       _k_ = slot[6] ? cst_inlined : cst,
    100:       _l_ = slot[2],
    101:       _m_ = slot[7],
    102:        /*<<test.ml:59:14>>*/ _n_ = info(slot[1]);
    103:       /*<<test.ml:58:6>>*/ return [0,
    104:               /*<<test.ml:58:11>>*/ caml_call8
    105:               (Stdlib_Printf[4], _a_, _n_, _m_, _l_, _k_, _j_, _i_, _h_)];
    106:     }
    107:     if(slot[1])  /*<<test.ml:54:50>>*/ return 0;
    108:      /*<<test.ml:56:51>>*/  /*<<test.ml:56:51>>*/ var _o_ = info(0);
    109:      /*<<test.ml:56:8>>*/ return [0,
    110:              /*<<test.ml:56:13>>*/ caml_call2(Stdlib_Printf[4], _b_, _o_)];
    111:     /*<<test.ml:61:52>>*/ }
    112:    function print_exception_backtrace(outchan, backtrace){
    113:      /*<<test.ml:64:2>>*/ if(! backtrace)
    114:       /*<<test.ml:66:6>>*/ return  /*<<test.ml:66:6>>*/ caml_call2
    115:              (Stdlib_Printf[1], outchan, _d_);
    116:     var a = backtrace[1], _f_ = a.length - 1 - 1 | 0, _e_ = 0;
    117:     if(_f_ >= 0){
    118:      var i = _e_;
    119:      for(;;){
    120:        /*<<test.ml:70:14>>*/  /*<<test.ml:70:14>>*/ var
    121:        match = format_backtrace_slot(i, runtime.caml_check_bound(a, i)[1 + i]);
    122:       if(match){
    123:        var str = match[1];
    124:         /*<<test.ml:72:24>>*/  /*<<test.ml:72:24>>*/ caml_call3
    125:         (Stdlib_Printf[1], outchan, _c_, str);
    126:       }
    127:        /*<<test.ml:69:6>>*/  /*<<test.ml:69:6>>*/ var _g_ = i + 1 | 0;
    128:       if(_f_ === i) break;
    129:       var i = _g_;
    130:      }
    131:     }
    132:     return 0;
    133:     /*<<test.ml:73:10>>*/ }
    134:    function compare(left, right, e1, e2){
    135:      /*<<test.ml:77:35>>*/ if(0 === e1[0]){
    136:      var v1 = e1[1];
    137:      if(0 !== e2[0])  /*<<test.ml:80:23>>*/ return -1;
    138:      var v2 = e2[1];
    139:       /*<<test.ml:78:24>>*/ return  /*<<test.ml:78:24>>*/ caml_call2
    140:              (left, v1, v2);
    141:     }
    142:     var v1$0 = e1[1];
    143:     if(0 === e2[0])  /*<<test.ml:81:23>>*/ return 1;
    144:     var v2$0 = e2[1];
    145:      /*<<test.ml:79:26>>*/ return  /*<<test.ml:79:26>>*/ caml_call2
    146:             (right, v1$0, v2$0);
    147:     /*<<test.ml:81:24>>*/ }
    148:     /*<<test.ml:75:16>>*/ var
    149:      /*<<test.ml:75:16>>*/ Either = [0, compare],
    150:     Test =
    151:       [0,
    152:        executable_name,
    153:        os_type,
    154:        backend_type,
    155:        0,
    156:        32,
    157:        32,
    158:        unix,
    159:        win32,
    160:        cygwin,
    161:        max_array_length,
    162:        max_floatarray_length,
    163:        max_string_length,
    164:        Unhandled,
    165:        format_backtrace_slot,
    166:        print_exception_backtrace,
    167:        Either];
    168:    runtime.caml_register_global(12, Test, "Test");
    169:    return;
    170:    /*<<?>>*/ }
    171:   (globalThis));
    172:
    173: //# sourceMappingURL=test.map |}]
