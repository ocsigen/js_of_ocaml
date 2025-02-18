(* Js_of_ocaml benchmarks
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open StdLabels
open Common

module Param = struct
  type t =
    { warm_up_time : float
    ; min_measures : int
    ; max_confidence : float
    ; max_duration : float
    ; verbose : bool
    }

  let default =
    { warm_up_time = 1.0
    ; min_measures = 10
    ; max_confidence = 0.03
    ; max_duration = 20.
    ; verbose = false
    }

  let fast x = { x with min_measures = 5; max_confidence = 0.15 }

  let ffast x = { x with min_measures = 2; max_confidence = 42. }

  let verbose x = { x with verbose = true }
end

let run_command ~verbose cmd =
  if verbose then Format.printf "+ %s@." cmd;
  match Unix.system cmd with
  | Unix.WEXITED res when res <> 0 ->
      failwith (Printf.sprintf "Command '%s' failed with exit code %d." cmd res)
  | Unix.WSIGNALED s ->
      failwith (Format.sprintf "Command '%s' killed with signal %d." cmd s)
  | _ -> ()

let time ~verbose cmd =
  let t1 = (Unix.times ()).Unix.tms_cutime in
  run_command ~verbose cmd;
  let t2 = (Unix.times ()).Unix.tms_cutime in
  t2 -. t1

let compile_gen
    (param : Param.t)
    ~comptime
    prog
    src_dir
    (src_spec : Spec.t)
    dst_dir
    dst_spec =
  mkdir (Spec.dir ~root:dst_dir dst_spec);
  List.iter (Spec.find_names ~root:src_dir src_spec) ~f:(fun nm ->
      let src = Spec.file ~root:src_dir src_spec nm in
      let dst = Spec.file ~root:dst_dir dst_spec nm in
      if need_update src dst
      then
        let cmd = prog ~src ~dst in
        try
          if comptime
          then write_measures compiletimes dst_spec nm [ time ~verbose:param.verbose cmd ]
          else run_command ~verbose:param.verbose cmd
        with Failure s -> Format.eprintf "Failure: %s@." s)

let compile param ~comptime prog =
  compile_gen param ~comptime (fun ~src ~dst -> Printf.sprintf "%s %s -o %s" prog src dst)

(****)

let need_more ~print (param : Param.t) l =
  let a = Array.of_list l in
  let n = Array.length a in
  if n = 0
  then true
  else
    let m, i = mean_with_confidence a in
    if print then Format.eprintf "==> %f +/- %f / %f %d\r%!" m i (i /. m) n;
    n < param.min_measures || i /. m > param.max_confidence /. 2.

let warm_up (param : Param.t) cmd =
  let t = ref 0. in
  while !t < param.warm_up_time do
    let t' =
      time ~verbose:param.verbose (Printf.sprintf "timeout %f %s" param.max_duration cmd)
    in
    if t' > param.max_duration
    then failwith (Printf.sprintf "Warmup took too long %.1fs" t');
    t := !t +. t'
  done

let rec measure_rec ~print (param : Param.t) cmd l =
  let t = time ~verbose:param.verbose cmd in
  let l = t :: l in
  if need_more ~print param l then measure_rec ~print param cmd l else l

let measure_one param code (meas : Measure.t) spec nm cmd =
  let l =
    if measures_need_update code meas spec nm
    then []
    else read_measures meas.Measure.path spec nm
  in
  if need_more ~print:false param l
  then (
    Format.eprintf "warming up ...\r%!";
    warm_up param cmd;
    let l = measure_rec ~print:true param cmd l in
    write_measures meas spec nm l;
    Format.eprintf "\n%!";
    l)
  else l

let measure ~param ~code ~(meas : Measure.t) ~spec cmd =
  List.iter (Spec.find_names ~root:code spec) ~f:(fun nm ->
      let cmd = if cmd = "" then cmd else cmd ^ " " in
      let cmd = Format.sprintf "%s%s" cmd (Spec.file ~root:code spec nm) in
      Format.eprintf "Measure %s@." cmd;
      try ignore (measure_one param code meas spec nm cmd)
      with Failure s -> Format.eprintf "Failure: %s@." s)

(****)

let compile_no_ext param ~comptime prog src_dir src_spec dst_dir dst_spec =
  compile_gen param ~comptime prog src_dir src_spec dst_dir (Spec.no_ext dst_spec)

let ml_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf
        "perl ./utils/remove_comments.pl %s | sed 's/^ *//g' | wc -c > %s"
        src
        dst)

let file_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf "wc -c < %s > %s" src dst)

let compr_file_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf "sed 's/^ *//g' %s | gzip -c | wc -c > %s" src dst)

let bzip2_file_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf "sed 's/^ *//g' %s | bzip2 -c | wc -c > %s" src dst)

let runtime_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf
        {|awk -vRS="--MARK--start-of-jsoo-gen--MARK--" '{print length($0)}' %s | head -n1  > %s|}
        src
        dst)

let gen_size param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      Format.sprintf
        {|awk -vRS="--MARK--start-of-jsoo-gen--MARK--" '{print length($0)}' %s | tail -n1 > %s|}
        src
        dst)

(****)

let read_config file =
  if not (Sys.file_exists file)
  then (
    Format.eprintf "Configuration file '%s' not found!@." file;
    exit 1);
  let i = ref [] in
  let ch = open_in file in
  (try
     while true do
       let line = String.trim (input_line ch) in
       if line.[0] <> '#'
       then
         match
           List.filter
             ~f:(function
               | "" -> false
               | _ -> true)
             (split_on_char line ~sep:' ')
         with
         | "interpreter" :: nm :: rem -> i := (String.concat ~sep:" " rem, nm) :: !i
         | [ "interpreter" ] ->
             Format.eprintf "Malformed config option '%s'@." line;
             exit 1
         | kind :: _ ->
             Format.eprintf "Unknown config option '%s'@." kind;
             exit 1
         | [] ->
             Format.eprintf "Bad config line '%s'@." line;
             exit 1
     done
   with End_of_file -> ());
  close_in ch;
  List.rev !i

let _ =
  let compile_only = ref false in
  let full = ref false in
  let effects = ref `None in
  let conf_file = ref "run.config" in
  let nobyteopt = ref false in
  let wasm = ref false in
  let param = ref Param.default in
  let fast_run () = param := Param.fast !param in
  let ffast_run () = param := Param.ffast !param in
  let verbose () = param := Param.verbose !param in
  let set_effects (backend : [ `None | `Cps | `Double_translation ]) =
    effects := backend
  in
  let nojs = ref false in
  let options =
    [ "-compile", Arg.Set compile_only, " only compiles"
    ; "-all", Arg.Set full, " run all benchmarks"
    ; ( "-effects"
      , Arg.Symbol
          ( [ "none"; "cps"; "double-translation" ]
          , function
            | "none" -> set_effects `None
            | "cps" -> set_effects `Cps
            | "double-translation" -> set_effects `Double_translation
            | _ -> assert false )
      , " only run with and without effect handler support" )
    ; "-config", Arg.Set_string conf_file, "<file> use <file> as a config file"
    ; "-fast", Arg.Unit fast_run, " perform less iterations"
    ; "-ffast", Arg.Unit ffast_run, " perform very few iterations"
    ; "-verbose", Arg.Unit verbose, " verbose"
    ; ( "-nobyteopt"
      , Arg.Set nobyteopt
      , " do not run benchs on bytecode and native programs" )
    ; ( "-nojs"
      , Arg.Set nojs
      , " do not compile to Javascript with js_of_ocaml (default: compiles)" )
    ; "-wasm", Arg.Set wasm, " compile to Wasm with wasm_of_ocaml (default false)"
    ]
  in
  Arg.parse
    (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));
  let conf_file = !conf_file in
  let compile_only = !compile_only in
  let nobyteopt = !nobyteopt in
  let full = !full in
  let param = !param in
  let interpreters = read_config conf_file in
  let compile = compile param ~comptime:true in
  let compile_jsoo ~wasm ?(effects = `None) opts =
    compile
      (Format.sprintf
         "%s -q %s%s %s"
         (if wasm then "wasm_of_ocaml" else "js_of_ocaml")
         (if wasm then "" else "--target-env browser --debug mark-runtime-gen ")
         opts
         (match effects with
         | `None -> "--disable=effects"
         | `Cps -> "--effects=cps"
         | `Double_translation -> "--effects=double-translation"))
  in
  let js = not !nojs in
  let wasm = !wasm in
  let effects =
    match !effects, wasm with
    | ((`None | `Cps) as e), _ -> e
    | (`Double_translation as e), false -> e
    | `Double_translation, true ->
        raise
          (Arg.Bad "option `-effects double-translation` incompatible with option `-wasm`")
  in
  Format.eprintf "Compile@.";
  compile "ocamlc" src Spec.ml code Spec.byte;
  compile "ocamlopt" src Spec.ml code Spec.opt;
  compile "ocamlc -unsafe" src Spec.ml code Spec.byte_unsafe;
  compile "ocamlopt" src Spec.ml code Spec.opt_unsafe;
  if wasm then compile_jsoo ~wasm "" code Spec.byte code Spec.wasm_of_ocaml;
  if js
  then (
    compile_jsoo ~wasm "" code Spec.byte code Spec.js_of_ocaml;
    compile_jsoo ~wasm "--opt=3" code Spec.byte code Spec.js_of_ocaml_o3;
    compile_jsoo
      ~wasm
      "--enable=use-js-string"
      code
      Spec.byte
      code
      Spec.js_of_ocaml_js_string;
    compile_jsoo ~wasm "--disable inline" code Spec.byte code Spec.js_of_ocaml_inline;
    compile_jsoo ~wasm "--disable deadcode" code Spec.byte code Spec.js_of_ocaml_deadcode;
    compile_jsoo ~wasm "--disable compact" code Spec.byte code Spec.js_of_ocaml_compact;
    compile_jsoo ~wasm "--disable optcall" code Spec.byte code Spec.js_of_ocaml_call;
    (match effects with
    | `None -> ()
    | _ -> compile_jsoo ~wasm ~effects "" code Spec.byte code Spec.js_of_ocaml_effects);
    compile_jsoo ~wasm "" code Spec.byte_unsafe code Spec.js_of_ocaml_unsafe);
  Format.eprintf "Sizes@.";
  ml_size param src Spec.ml sizes.Measure.path Spec.ml;
  file_size param code Spec.byte sizes.Measure.path Spec.byte;
  if js
  then
    file_size
      param
      code
      Spec.js_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.js_of_ocaml "full");
  if wasm
  then
    file_size
      param
      code
      Spec.wasm_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.wasm_of_ocaml "full");
  if js
  then (
    compr_file_size
      param
      code
      Spec.js_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.js_of_ocaml "gzipped");
    (match effects with
    | `None -> ()
    | _ ->
        compr_file_size
          param
          code
          Spec.js_of_ocaml_effects
          sizes.Measure.path
          (Spec.sub_spec Spec.js_of_ocaml_effects "gzipped");
        bzip2_file_size
          param
          code
          Spec.js_of_ocaml_effects
          sizes.Measure.path
          (Spec.sub_spec Spec.js_of_ocaml_effects "bzip2"));
    bzip2_file_size
      param
      code
      Spec.js_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.js_of_ocaml "bzip2");
    runtime_size
      param
      code
      Spec.js_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.js_of_ocaml "runtime");
    gen_size
      param
      code
      Spec.js_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.js_of_ocaml "generated");
    gen_size param code Spec.js_of_ocaml_o3 sizes.Measure.path Spec.js_of_ocaml_o3;
    gen_size
      param
      code
      Spec.js_of_ocaml_js_string
      sizes.Measure.path
      Spec.js_of_ocaml_js_string;
    gen_size param code Spec.js_of_ocaml_inline sizes.Measure.path Spec.js_of_ocaml_inline;
    gen_size
      param
      code
      Spec.js_of_ocaml_deadcode
      sizes.Measure.path
      Spec.js_of_ocaml_deadcode;
    gen_size
      param
      code
      Spec.js_of_ocaml_compact
      sizes.Measure.path
      Spec.js_of_ocaml_compact;
    gen_size param code Spec.js_of_ocaml_call sizes.Measure.path Spec.js_of_ocaml_call;
    match effects with
    | `None -> ()
    | _ ->
        gen_size
          param
          code
          Spec.js_of_ocaml_effects
          sizes.Measure.path
          Spec.js_of_ocaml_effects);
  if wasm
  then (
    compr_file_size
      param
      code
      Spec.wasm_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.wasm_of_ocaml "gzipped");
    bzip2_file_size
      param
      code
      Spec.wasm_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.wasm_of_ocaml "bzip2");
    gen_size
      param
      code
      Spec.wasm_of_ocaml
      sizes.Measure.path
      (Spec.sub_spec Spec.wasm_of_ocaml "generated"));
  if compile_only then exit 0;
  Format.eprintf "Measure@.";
  if not nobyteopt
  then (
    measure ~param ~code ~meas:times ~spec:Spec.opt "";
    measure ~param ~code ~meas:times ~spec:Spec.byte "");
  let interpreters, suites =
    if full
    then
      ( interpreters
      , (if js
         then
           [ Some Spec.js_of_ocaml
           ; Some Spec.js_of_ocaml_o3
           ; Some Spec.js_of_ocaml_js_string
           ; Some Spec.js_of_ocaml_unsafe
           ; Some Spec.js_of_ocaml_inline
           ; Some Spec.js_of_ocaml_deadcode
           ; Some Spec.js_of_ocaml_compact
           ; Some Spec.js_of_ocaml_call
           ]
           @
           match effects with
           | `None -> []
           | _ -> [ Some Spec.js_of_ocaml_effects ]
         else [])
        @ if wasm then [ Some Spec.wasm_of_ocaml ] else [] )
    else
      ( (match interpreters with
        | i :: _ -> [ i ]
        | [] -> [])
      , (match effects, js with
        | `Cps, true -> [ Some Spec.js_of_ocaml; Some Spec.js_of_ocaml_effects ]
        | `Double_translation, _ ->
            failwith "double translation benchmarks not implemented"
        | `None, true -> [ Some Spec.js_of_ocaml ]
        | _, false -> [])
        @ if wasm then [ Some Spec.wasm_of_ocaml ] else [] )
  in
  List.iter interpreters ~f:(fun (comp, dir) ->
      if js
      then
        (* Measure the time taken by an implementation in Javascript for comparison *)
        measure
          ~param
          ~code:src
          ~meas:Measure.{ times with path = Filename.concat times.path dir }
          ~spec:Spec.js
          comp;
      List.iter suites ~f:(function
        | None -> ()
        | Some suite ->
            measure
              ~param
              ~code
              ~meas:Measure.{ times with path = Filename.concat times.path dir }
              ~spec:suite
              comp))
