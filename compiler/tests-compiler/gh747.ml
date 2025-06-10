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
      4: //# shape: Test:[N,N]
      5: (function
      6:   (globalThis){
      7:    "use strict";
      8:    var
      9:     runtime = globalThis.jsoo_runtime,
     10:     caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
     11:    function caml_call1(f, a0){
     12:     return (f.l >= 0 ? f.l : f.l = f.length) === 1
     13:             ? f(a0)
     14:             : runtime.caml_call_gen(f, [a0]);
     15:    }
     16:    function caml_call2(f, a0, a1){
     17:     return (f.l >= 0 ? f.l : f.l = f.length) === 2
     18:             ? f(a0, a1)
     19:             : runtime.caml_call_gen(f, [a0, a1]);
     20:    }
     21:    var
     22:     global_data = runtime.caml_get_global_data(),
     23:     greeting = caml_string_of_jsbytes("hello world"),
     24:     greeting$0 = caml_string_of_jsbytes("hello world with unicode: \xc9\x8a"),
     25:     Stdlib = global_data.Stdlib,
     26:     Stdlib_Random = global_data.Stdlib__Random,
     27:     Stdlib_String = global_data.Stdlib__String;
     28:     /*<<test.ml:3:0>>*/ caml_call1(Stdlib[46], greeting);
     29:     /*<<test.ml:5:0>>*/ caml_call1(Stdlib[46], greeting$0);
     30:    var
     31:     _a_ =  /*<<test.ml:7:47>>*/ caml_call1(Stdlib_Random[5], 30),
     32:     unicodeLength =
     33:        /*<<test.ml:7:34>>*/  /*<<test.ml:7:67>>*/ runtime.caml_ml_string_length
     34:        ( /*<<test.ml:7:34>>*/ caml_call2(Stdlib_String[1], _a_, 105)),
     35:     _b_ =  /*<<test.ml:8:56>>*/ caml_call1(Stdlib[33], unicodeLength),
     36:     _c_ =
     37:        /*<<test.ml:8:14>>*/ caml_call2
     38:        (Stdlib[28],
     39:         caml_string_of_jsbytes('String.length("\xc9\x8a") should be two:'),
     40:         _b_);
     41:     /*<<test.ml:8:0>>*/ caml_call1(Stdlib[46], _c_);
     42:    var
     43:     _d_ =  /*<<test.ml:9:39>>*/ caml_call2(Stdlib_String[1], 1, 138),
     44:     _e_ =  /*<<test.ml:9:14>>*/ caml_call2(Stdlib_String[1], 1, 201),
     45:     _f_ =  /*<<test.ml:9:13>>*/ caml_call2(Stdlib[28], _e_, _d_);
     46:     /*<<test.ml:9:0>>*/ caml_call1(Stdlib[46], _f_);
     47:    var Test =  /*<<test.ml:9:62>>*/ [0, greeting$0, unicodeLength];
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
      4: //# shape: Test:[N,N,[N],N,N,N,N,N,N,N,N,N,N,F(2),F(2),[F(4)]]
      5: (function
      6:   (globalThis){
      7:    "use strict";
      8:    var
      9:     runtime = globalThis.jsoo_runtime,
     10:     caml_string_of_jsbytes = runtime.caml_string_of_jsbytes;
     11:    function caml_call2(f, a0, a1){
     12:     return (f.l >= 0 ? f.l : f.l = f.length) === 2
     13:             ? f(a0, a1)
     14:             : runtime.caml_call_gen(f, [a0, a1]);
     15:    }
     16:    function caml_call3(f, a0, a1, a2){
     17:     return (f.l >= 0 ? f.l : f.l = f.length) === 3
     18:             ? f(a0, a1, a2)
     19:             : runtime.caml_call_gen(f, [a0, a1, a2]);
     20:    }
     21:    function caml_call8(f, a0, a1, a2, a3, a4, a5, a6, a7){
     22:     return (f.l >= 0 ? f.l : f.l = f.length) === 8
     23:             ? f(a0, a1, a2, a3, a4, a5, a6, a7)
     24:             : runtime.caml_call_gen(f, [a0, a1, a2, a3, a4, a5, a6, a7]);
     25:    }
     26:    var
     27:     global_data = runtime.caml_get_global_data(),
     28:     cst = caml_string_of_jsbytes(""),
     29:     partial = [4, 0, 0, 0, [12, 45, [4, 0, 0, 0, 0]]],
     30:     Stdlib_Printf = global_data.Stdlib__Printf,
     31:     executable_name =
     32:        /*<<test.ml:14:22>>*/ runtime.caml_sys_executable_name(0),
     33:     os_type =  /*<<test.ml:15:22>>*/ runtime.caml_sys_get_config(0)[1],
     34:     backend_type =
     35:        /*<<test.ml:15:34>>*/ [0, caml_string_of_jsbytes("js_of_ocaml")],
     36:     unix = runtime.caml_sys_const_ostype_unix(0),
     37:     win32 = runtime.caml_sys_const_ostype_win32(0),
     38:     cygwin = runtime.caml_sys_const_ostype_cygwin(0),
     39:     max_array_length = runtime.caml_sys_const_max_wosize(0),
     40:     max_floatarray_length = max_array_length / 2 | 0,
     41:     max_string_length = (4 * max_array_length | 0) - 1 | 0,
     42:     Unhandled =
     43:       [248,
     44:        caml_string_of_jsbytes("Test.Unhandled"),
     45:        runtime.caml_fresh_oo_id(0)],
     46:     cst_Raised_at = caml_string_of_jsbytes("Raised at"),
     47:     cst_Re_raised_at = caml_string_of_jsbytes("Re-raised at"),
     48:     cst_Raised_by_primitive_operat =
     49:       caml_string_of_jsbytes("Raised by primitive operation at"),
     50:     cst_Called_from = caml_string_of_jsbytes("Called from"),
     51:     cst_inlined = caml_string_of_jsbytes(" (inlined)"),
     52:     _a_ =
     53:       [0,
     54:        [2,
     55:         0,
     56:         [12,
     57:          32,
     58:          [2,
     59:           0,
     60:           [11,
     61:            caml_string_of_jsbytes(' in file "'),
     62:            [2,
     63:             0,
     64:             [12,
     65:              34,
     66:              [2,
     67:               0,
     68:               [11,
     69:                caml_string_of_jsbytes(", line "),
     70:                [4,
     71:                 0,
     72:                 0,
     73:                 0,
     74:                 [11, caml_string_of_jsbytes(", characters "), partial]]]]]]]]]],
     75:        caml_string_of_jsbytes
     76:         ('%s %s in file "%s"%s, line %d, characters %d-%d')],
     77:     _b_ =
     78:       [0,
     79:        [2, 0, [11, caml_string_of_jsbytes(" unknown location"), 0]],
     80:        caml_string_of_jsbytes("%s unknown location")],
     81:     _c_ = [0, [2, 0, [12, 10, 0]], caml_string_of_jsbytes("%s\n")],
     82:     _d_ =
     83:       [0,
     84:        [11,
     85:         caml_string_of_jsbytes
     86:          ("(Program not linked with -g, cannot print stack backtrace)\n"),
     87:         0],
     88:        caml_string_of_jsbytes
     89:         ("(Program not linked with -g, cannot print stack backtrace)\n")];
     90:    function format_backtrace_slot(pos, slot){
     91:     function info(is_raise){
     92:       /*<<test.ml:46:4>>*/ return is_raise
     93:              ? 0 === pos ? cst_Raised_at : cst_Re_raised_at
     94:              : 0 === pos ? cst_Raised_by_primitive_operat : cst_Called_from /*<<test.ml:49:75>>*/ ;
     95:     }
     96:      /*<<test.ml:51:2>>*/ if(0 === slot[0]){
     97:      var
     98:       _g_ =  /*<<test.ml:58:6>>*/ slot[5],
     99:       _h_ = slot[4],
    100:       _i_ = slot[3],
    101:       _j_ = slot[6] ? cst_inlined : cst,
    102:       _k_ =  /*<<test.ml:59:14>>*/ slot[2],
    103:       _l_ = slot[7],
    104:       _m_ = info(slot[1]);
    105:       /*<<test.ml:58:11>>*/ return [0,
    106:              caml_call8
    107:               (Stdlib_Printf[4], _a_, _m_, _l_, _k_, _j_, _i_, _h_, _g_)] /*<<test.ml:61:52>>*/ ;
    108:     }
    109:      /*<<test.ml:53:6>>*/ if(slot[1])  /*<<test.ml:54:50>>*/ return 0;
    110:     var _n_ =  /*<<test.ml:56:51>>*/ info(0);
    111:      /*<<test.ml:56:13>>*/ return [0, caml_call2(Stdlib_Printf[4], _b_, _n_)] /*<<test.ml:56:64>>*/ ;
    112:     /*<<test.ml:61:52>>*/ }
    113:    function print_exception_backtrace(outchan, backtrace){
    114:      /*<<test.ml:64:2>>*/ if(! backtrace)
    115:       /*<<test.ml:66:6>>*/ return caml_call2(Stdlib_Printf[1], outchan, _d_) /*<<test.ml:73:10>>*/ ;
    116:     var
    117:      a =  /*<<test.ml:64:2>>*/ backtrace[1],
    118:      _e_ =  /*<<test.ml:69:6>>*/ a.length - 2 | 0,
    119:      _f_ = 0;
    120:     if(_e_ >= 0){
    121:      var i = _f_;
    122:      for(;;){
    123:       var
    124:        match =
    125:           /*<<test.ml:70:38>>*/  /*<<test.ml:70:14>>*/ format_backtrace_slot
    126:           (i,  /*<<test.ml:70:38>>*/ runtime.caml_check_bound(a, i)[i + 1]);
    127:        /*<<test.ml:70:43>>*/ if(match){
    128:        var str = match[1];
    129:         /*<<test.ml:72:24>>*/ caml_call3(Stdlib_Printf[1], outchan, _c_, str);
    130:       }
    131:       var _g_ =  /*<<test.ml:70:43>>*/ i + 1 | 0;
    132:       if(_e_ === i) break;
    133:       i = _g_;
    134:      }
    135:     }
    136:      /*<<test.ml:69:6>>*/ return 0;
    137:     /*<<test.ml:73:10>>*/ }
    138:    function compare(left, right, e1, e2){
    139:      /*<<test.ml:77:35>>*/ if(0 === e1[0]){
    140:      var v1 = e1[1];
    141:      if(0 !== e2[0])  /*<<test.ml:80:23>>*/ return -1;
    142:      var v2 =  /*<<test.ml:77:35>>*/ e2[1];
    143:       /*<<test.ml:78:24>>*/ return caml_call2(left, v1, v2) /*<<test.ml:81:24>>*/ ;
    144:     }
    145:     var v1$0 =  /*<<test.ml:77:35>>*/ e1[1];
    146:     if(0 === e2[0])  /*<<test.ml:81:23>>*/ return 1;
    147:     var v2$0 =  /*<<test.ml:77:35>>*/ e2[1];
    148:      /*<<test.ml:79:26>>*/ return caml_call2(right, v1$0, v2$0) /*<<test.ml:81:24>>*/ ;
    149:    }
    150:    var
    151:     Either =  /*<<test.ml:15:34>>*/ [0, compare],
    152:     Test =
    153:       [0,
    154:        executable_name,
    155:        os_type,
    156:        backend_type,
    157:        0,
    158:        32,
    159:        32,
    160:        unix,
    161:        win32,
    162:        cygwin,
    163:        max_array_length,
    164:        max_floatarray_length,
    165:        max_string_length,
    166:        Unhandled,
    167:        format_backtrace_slot,
    168:        print_exception_backtrace,
    169:        Either];
    170:    runtime.caml_register_global(12, Test, "Test");
    171:    return;
    172:    /*<<?>>*/ }
    173:   (globalThis));
    174:
    175: //# sourceMappingURL=test.map
    |}]
