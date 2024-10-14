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
     10:    function caml_call1(f, a0){
     11:     return (f.l >= 0 ? f.l : f.l = f.length) === 1
     12:             ? f(a0)
     13:             : runtime.caml_call_gen(f, [a0]);
     14:    }
     15:    function caml_call2(f, a0, a1){
     16:     return (f.l >= 0 ? f.l : f.l = f.length) === 2
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
     27:     /*<<test.ml:3:0>>*/ caml_call1(Stdlib[46], greeting);
     28:     /*<<test.ml:5:0>>*/ caml_call1(Stdlib[46], greeting$0);
     29:    var
     30:     _a_ = /*<<test.ml:7:47>>*/  caml_call1(Stdlib_Random[5], 30),
     31:     unicodeLength = /*<<test.ml:7:67>>*/
     32:       runtime.caml_ml_string_length
     33:        ( /*<<test.ml:7:34>>*/ caml_call2
     34:          ( /*<<test.ml:7:67>>*/ Stdlib_String[1], _a_, 105)),
     35:     _b_ = /*<<test.ml:8:56>>*/  caml_call1(Stdlib[33], unicodeLength),
     36:     _c_ = /*<<test.ml:8:14>>*/
     37:       caml_call2
     38:        (Stdlib[28],
     39:         caml_string_of_jsbytes('String.length("\xc9\x8a") should be two:'),
     40:         _b_);
     41:     /*<<test.ml:8:0>>*/ caml_call1(Stdlib[46], _c_);
     42:    var
     43:     _d_ = /*<<test.ml:9:39>>*/  caml_call2(Stdlib_String[1], 1, 138),
     44:     _e_ = /*<<test.ml:9:14>>*/  caml_call2(Stdlib_String[1], 1, 201),
     45:     _f_ = /*<<test.ml:9:13>>*/  caml_call2(Stdlib[28], _e_, _d_);
     46:     /*<<test.ml:9:0>>*/ caml_call1(Stdlib[46], _f_);
     47:    var Test = /*<<test.ml:9:62>>*/  [0, greeting$0, unicodeLength];
     48:    runtime.caml_register_global(8, Test, "Test");
     49:    return;
     50:    /*<<?>>*/ }
     51:   (globalThis));
     52:
     53: //# sourceMappingURL=test.map
    |}]

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

type 'a effect_
type exn += Unhandled: 'a effect_ -> exn

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
     10:    function caml_call2(f, a0, a1){
     11:     return (f.l >= 0 ? f.l : f.l = f.length) === 2
     12:             ? f(a0, a1)
     13:             : runtime.caml_call_gen(f, [a0, a1]);
     14:    }
     15:    function caml_call3(f, a0, a1, a2){
     16:     return (f.l >= 0 ? f.l : f.l = f.length) === 3
     17:             ? f(a0, a1, a2)
     18:             : runtime.caml_call_gen(f, [a0, a1, a2]);
     19:    }
     20:    function caml_call8(f, a0, a1, a2, a3, a4, a5, a6, a7){
     21:     return (f.l >= 0 ? f.l : f.l = f.length) === 8
     22:             ? f(a0, a1, a2, a3, a4, a5, a6, a7)
     23:             : runtime.caml_call_gen(f, [a0, a1, a2, a3, a4, a5, a6, a7]);
     24:    }
     25:    var
     26:     global_data = runtime.caml_get_global_data(),
     27:     cst = caml_string_of_jsbytes(""),
     28:     partial = [4, 0, 0, 0, [12, 45, [4, 0, 0, 0, 0]]],
     29:     Stdlib_Printf = global_data.Stdlib__Printf,
     30:     executable_name = /*<<test.ml:14:22>>*/
     31:       runtime.caml_sys_executable_name(0),
     32:     os_type = /*<<test.ml:15:34>>*/
     33:        /*<<test.ml:15:22>>*/ runtime.caml_sys_get_config
     34:         ( /*<<test.ml:15:34>>*/ 0)
     35:        [1],
     36:     backend_type = [0, caml_string_of_jsbytes("js_of_ocaml")],
     37:     unix = runtime.caml_sys_const_ostype_unix(0),
     38:     win32 = runtime.caml_sys_const_ostype_win32(0),
     39:     cygwin = runtime.caml_sys_const_ostype_cygwin(0),
     40:     max_array_length = runtime.caml_sys_const_max_wosize(0),
     41:     max_floatarray_length = max_array_length / 2 | 0,
     42:     max_string_length = (4 * max_array_length | 0) - 1 | 0,
     43:     Unhandled =
     44:       [248,
     45:        caml_string_of_jsbytes("Test.Unhandled"),
     46:        runtime.caml_fresh_oo_id(0)],
     47:     cst_Raised_at = caml_string_of_jsbytes("Raised at"),
     48:     cst_Re_raised_at = caml_string_of_jsbytes("Re-raised at"),
     49:     cst_Raised_by_primitive_operat =
     50:       caml_string_of_jsbytes("Raised by primitive operation at"),
     51:     cst_Called_from = caml_string_of_jsbytes("Called from"),
     52:     cst_inlined = caml_string_of_jsbytes(" (inlined)"),
     53:     _a_ =
     54:       [0,
     55:        [2,
     56:         0,
     57:         [12,
     58:          32,
     59:          [2,
     60:           0,
     61:           [11,
     62:            caml_string_of_jsbytes(' in file "'),
     63:            [2,
     64:             0,
     65:             [12,
     66:              34,
     67:              [2,
     68:               0,
     69:               [11,
     70:                caml_string_of_jsbytes(", line "),
     71:                [4,
     72:                 0,
     73:                 0,
     74:                 0,
     75:                 [11, caml_string_of_jsbytes(", characters "), partial]]]]]]]]]],
     76:        caml_string_of_jsbytes
     77:         ('%s %s in file "%s"%s, line %d, characters %d-%d')],
     78:     _b_ =
     79:       [0,
     80:        [2, 0, [11, caml_string_of_jsbytes(" unknown location"), 0]],
     81:        caml_string_of_jsbytes("%s unknown location")],
     82:     _c_ = [0, [2, 0, [12, 10, 0]], caml_string_of_jsbytes("%s\n")],
     83:     _d_ =
     84:       [0,
     85:        [11,
     86:         caml_string_of_jsbytes
     87:          ("(Program not linked with -g, cannot print stack backtrace)\n"),
     88:         0],
     89:        caml_string_of_jsbytes
     90:         ("(Program not linked with -g, cannot print stack backtrace)\n")];
     91:    function format_backtrace_slot(pos, slot){
     92:     function info(is_raise){
     93:       /*<<test.ml:46:4>>*/ return is_raise
     94:              ? 0 === pos ? cst_Raised_at : cst_Re_raised_at
     95:              : 0 === pos ? cst_Raised_by_primitive_operat : cst_Called_from /*<<test.ml:49:75>>*/ ;
     96:     }
     97:      /*<<test.ml:51:2>>*/ if(0 === slot[0]){
     98:      var
     99:       _h_ = /*<<test.ml:58:6>>*/  slot[5],
    100:       _i_ = slot[4],
    101:       _j_ = slot[3],
    102:       _k_ = slot[6] ? cst_inlined : cst,
    103:       _l_ = slot[2],
    104:       _m_ = slot[7],
    105:       _n_ = /*<<test.ml:59:14>>*/  info(slot[1]);
    106:       /*<<test.ml:61:52>>*/ return [0,
    107:               /*<<test.ml:58:11>>*/ caml_call8
    108:               ( /*<<test.ml:61:52>>*/ Stdlib_Printf[4],
    109:                _a_,
    110:                _n_,
    111:                _m_,
    112:                _l_,
    113:                _k_,
    114:                _j_,
    115:                _i_,
    116:                _h_)];
    117:     } /*<<test.ml:51:2>>*/
    118:      /*<<test.ml:53:6>>*/ if(slot[1])
    119:       /*<<test.ml:54:50>>*/ return 0;  /*<<test.ml:53:6>>*/
    120:     var _o_ = /*<<test.ml:56:51>>*/  info(0);
    121:      /*<<test.ml:56:64>>*/ return [0,
    122:              /*<<test.ml:56:13>>*/ caml_call2
    123:              ( /*<<test.ml:56:64>>*/ Stdlib_Printf[4], _b_, _o_)];
    124:     /*<<test.ml:61:52>>*/ }
    125:    function print_exception_backtrace(outchan, backtrace){
    126:      /*<<test.ml:64:2>>*/ if(! backtrace)
    127:       /*<<test.ml:66:6>>*/ return caml_call2(Stdlib_Printf[1], outchan, _d_) /*<<test.ml:73:10>>*/ ;  /*<<test.ml:64:2>>*/
    128:     var
    129:      a = backtrace[1],
    130:      _f_ = /*<<test.ml:69:6>>*/  a.length - 2 | 0,
    131:      _e_ = 0;
    132:     if(_f_ >= 0){
    133:      var i = _e_;
    134:      for(;;){
    135:       var
    136:        match = /*<<test.ml:70:14>>*/
    137:          format_backtrace_slot
    138:           (i,
    139:             /*<<test.ml:70:38>>*/ runtime.caml_check_bound
    140:              ( /*<<test.ml:70:14>>*/ a, i)
    141:             [1 + i]);
    142:        /*<<test.ml:70:43>>*/ if(match){
    143:        var str = match[1];
    144:         /*<<test.ml:72:24>>*/ caml_call3(Stdlib_Printf[1], outchan, _c_, str);
    145:       } /*<<test.ml:70:43>>*/
    146:       var _g_ = i + 1 | 0;
    147:       if(_f_ === i) break;
    148:       i = _g_;
    149:      }
    150:     } /*<<test.ml:69:6>>*/
    151:     return 0;
    152:     /*<<test.ml:73:10>>*/ }
    153:    function compare(left, right, e1, e2){
    154:      /*<<test.ml:77:35>>*/ if(0 === e1[0]){
    155:      var v1 = e1[1];
    156:      if(0 !== e2[0])  /*<<test.ml:80:23>>*/ return -1;  /*<<test.ml:77:35>>*/
    157:      var v2 = e2[1];
    158:       /*<<test.ml:78:24>>*/ return caml_call2(left, v1, v2) /*<<test.ml:81:24>>*/ ;
    159:     } /*<<test.ml:77:35>>*/
    160:     var v1$0 = e1[1];
    161:     if(0 === e2[0])  /*<<test.ml:81:23>>*/ return 1;  /*<<test.ml:77:35>>*/
    162:     var v2$0 = e2[1];
    163:      /*<<test.ml:79:26>>*/ return caml_call2(right, v1$0, v2$0) /*<<test.ml:81:24>>*/ ;
    164:    }
    165:    var
    166:     Either = /*<<test.ml:15:34>>*/  [0, compare],
    167:     Test =
    168:       [0,
    169:        executable_name,
    170:        os_type,
    171:        backend_type,
    172:        0,
    173:        32,
    174:        32,
    175:        unix,
    176:        win32,
    177:        cygwin,
    178:        max_array_length,
    179:        max_floatarray_length,
    180:        max_string_length,
    181:        Unhandled,
    182:        format_backtrace_slot,
    183:        print_exception_backtrace,
    184:        Either];
    185:    runtime.caml_register_global(12, Test, "Test");
    186:    return;
    187:    /*<<?>>*/ }
    188:   (globalThis));
    189:
    190: //# sourceMappingURL=test.map
    |}]
