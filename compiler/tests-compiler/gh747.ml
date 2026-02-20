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
     10:     _b_ = runtime.caml_string_of_jsbytes;
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
     22:     _a_ = runtime.caml_get_global_data(),
     23:     greeting = _b_("hello world"),
     24:     greeting$0 = _b_("hello world with unicode: \xc9\x8a"),
     25:     Stdlib = _a_.Stdlib,
     26:     Stdlib_Random = _a_.Stdlib__Random,
     27:     Stdlib_String = _a_.Stdlib__String;
     28:     /*<<test.ml:3:0>>*/ caml_call1(Stdlib[46], greeting);
     29:     /*<<test.ml:5:0>>*/ caml_call1(Stdlib[46], greeting$0);
     30:     /*<<test.ml:7:47>>*/ _a_ = caml_call1(Stdlib_Random[5], 30);
     31:    var
     32:     unicodeLength =
     33:        /*<<test.ml:7:34>>*/  /*<<test.ml:7:67>>*/ runtime.caml_ml_string_length
     34:        ( /*<<test.ml:7:34>>*/ caml_call2(Stdlib_String[1], _a_, 105)),
     35:     _a_ =  /*<<test.ml:8:56>>*/ caml_call1(Stdlib[33], unicodeLength),
     36:     _b_ =
     37:        /*<<test.ml:8:14>>*/ caml_call2
     38:        (Stdlib[28], _b_('String.length("\xc9\x8a") should be two:'), _a_);
     39:     /*<<test.ml:8:0>>*/ caml_call1(Stdlib[46], _b_);
     40:     /*<<test.ml:9:39>>*/ _b_ = caml_call2(Stdlib_String[1], 1, 138);
     41:     /*<<test.ml:9:14>>*/ _a_ = caml_call2(Stdlib_String[1], 1, 201);
     42:     /*<<test.ml:9:13>>*/ _b_ = caml_call2(Stdlib[28], _a_, _b_);
     43:     /*<<test.ml:9:0>>*/ caml_call1(Stdlib[46], _b_);
     44:     /*<<test.ml:9:62>>*/ runtime.caml_register_global
     45:     (8, [0, greeting$0, unicodeLength], "Test");
     46:    return;
     47:    /*<<?>>*/ }
     48:   (globalThis));
     49:
     50: //# sourceMappingURL=test.map
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
     27:     Stdlib_Printf = runtime.caml_get_global_data().Stdlib__Printf,
     28:     executable_name =
     29:        /*<<test.ml:14:22>>*/ runtime.caml_sys_executable_name(0),
     30:     os_type =  /*<<test.ml:15:22>>*/ runtime.caml_sys_get_config(0)[1],
     31:     backend_type =
     32:        /*<<test.ml:15:34>>*/ [0, caml_string_of_jsbytes("js_of_ocaml")],
     33:     unix = runtime.caml_sys_const_ostype_unix(0),
     34:     win32 = runtime.caml_sys_const_ostype_win32(0),
     35:     cygwin = runtime.caml_sys_const_ostype_cygwin(0),
     36:     max_array_length = runtime.caml_sys_const_max_wosize(0),
     37:     max_floatarray_length = max_array_length / 2 | 0,
     38:     max_string_length = (4 * max_array_length | 0) - 1 | 0,
     39:     Unhandled =
     40:       [248,
     41:        caml_string_of_jsbytes("Test.Unhandled"),
     42:        runtime.caml_fresh_oo_id(0)],
     43:     cst_Raised_at = caml_string_of_jsbytes("Raised at"),
     44:     cst_Re_raised_at = caml_string_of_jsbytes("Re-raised at"),
     45:     cst_Raised_by_primitive_operat =
     46:       caml_string_of_jsbytes("Raised by primitive operation at"),
     47:     cst_Called_from = caml_string_of_jsbytes("Called from"),
     48:     cst_inlined = caml_string_of_jsbytes(" (inlined)"),
     49:     _a_ =
     50:       [0,
     51:        [2,
     52:         0,
     53:         [12,
     54:          32,
     55:          [2,
     56:           0,
     57:           [11,
     58:            caml_string_of_jsbytes(' in file "'),
     59:            [2,
     60:             0,
     61:             [12,
     62:              34,
     63:              [2,
     64:               0,
     65:               [11,
     66:                caml_string_of_jsbytes(", line "),
     67:                [4,
     68:                 0,
     69:                 0,
     70:                 0,
     71:                 [11,
     72:                  caml_string_of_jsbytes(", characters "),
     73:                  [4, 0, 0, 0, [12, 45, [4, 0, 0, 0, 0]]]]]]]]]]]]],
     74:        caml_string_of_jsbytes
     75:         ('%s %s in file "%s"%s, line %d, characters %d-%d')],
     76:     cst = caml_string_of_jsbytes(""),
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
     98:       _f_ =  /*<<test.ml:58:6>>*/ slot[5],
     99:       _g_ = slot[4],
    100:       _h_ = slot[3],
    101:       _i_ = slot[6] ? cst_inlined : cst,
    102:       _j_ =  /*<<test.ml:59:14>>*/ slot[2],
    103:       _k_ = slot[7],
    104:       _l_ = info(slot[1]);
    105:       /*<<test.ml:58:11>>*/ return [0,
    106:              caml_call8
    107:               (Stdlib_Printf[4], _a_, _l_, _k_, _j_, _i_, _h_, _g_, _f_)] /*<<test.ml:61:52>>*/ ;
    108:     }
    109:      /*<<test.ml:53:6>>*/ if(slot[1])  /*<<test.ml:54:50>>*/ return 0;
    110:      /*<<test.ml:56:51>>*/ _f_ = info(0);
    111:      /*<<test.ml:56:13>>*/ return [0, caml_call2(Stdlib_Printf[4], _b_, _f_)] /*<<test.ml:56:64>>*/ ;
    112:     /*<<test.ml:61:52>>*/ }
    113:    function print_exception_backtrace(outchan, backtrace){
    114:      /*<<test.ml:64:2>>*/ if(! backtrace)
    115:       /*<<test.ml:66:6>>*/ return caml_call2(Stdlib_Printf[1], outchan, _d_) /*<<test.ml:73:10>>*/ ;
    116:     var
    117:      a =  /*<<test.ml:64:2>>*/ backtrace[1],
    118:      _f_ =  /*<<test.ml:69:6>>*/ a.length - 2 | 0;
    119:     if(_f_ >= 0){
    120:      var i = 0;
    121:      for(;;){
    122:       var
    123:        _e_ =
    124:           /*<<test.ml:70:38>>*/  /*<<test.ml:70:14>>*/ format_backtrace_slot
    125:           (i,  /*<<test.ml:70:38>>*/ runtime.caml_check_bound(a, i)[i + 1]);
    126:        /*<<test.ml:70:43>>*/ if(_e_){
    127:        var str = _e_[1];
    128:         /*<<test.ml:72:24>>*/ caml_call3(Stdlib_Printf[1], outchan, _c_, str);
    129:       }
    130:        /*<<test.ml:70:43>>*/ _e_ = i + 1 | 0;
    131:       if(_f_ === i) break;
    132:       i = _e_;
    133:      }
    134:     }
    135:      /*<<test.ml:69:6>>*/ return 0;
    136:     /*<<test.ml:73:10>>*/ }
    137:    function compare(left, right, e1, e2){
    138:      /*<<test.ml:77:35>>*/ if(0 === e1[0]){
    139:      var v1 = e1[1];
    140:      if(0 !== e2[0])  /*<<test.ml:80:23>>*/ return -1;
    141:      var v2 =  /*<<test.ml:77:35>>*/ e2[1];
    142:       /*<<test.ml:78:24>>*/ return caml_call2(left, v1, v2) /*<<test.ml:81:24>>*/ ;
    143:     }
    144:     var v1$0 =  /*<<test.ml:77:35>>*/ e1[1];
    145:     if(0 === e2[0])  /*<<test.ml:81:23>>*/ return 1;
    146:     var v2$0 =  /*<<test.ml:77:35>>*/ e2[1];
    147:      /*<<test.ml:79:26>>*/ return caml_call2(right, v1$0, v2$0) /*<<test.ml:81:24>>*/ ;
    148:    }
    149:     /*<<test.ml:15:34>>*/ runtime.caml_register_global
    150:     (12,
    151:      [0,
    152:       executable_name,
    153:       os_type,
    154:       backend_type,
    155:       0,
    156:       32,
    157:       32,
    158:       unix,
    159:       win32,
    160:       cygwin,
    161:       max_array_length,
    162:       max_floatarray_length,
    163:       max_string_length,
    164:       Unhandled,
    165:       format_backtrace_slot,
    166:       print_exception_backtrace,
    167:       [0, compare]],
    168:      "Test");
    169:    return;
    170:    /*<<?>>*/ }
    171:   (globalThis));
    172:
    173: //# sourceMappingURL=test.map
    |}]
