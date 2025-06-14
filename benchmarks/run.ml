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
    ; max_duration = 120.
    ; verbose = false
    }

  let fast x = { x with min_measures = 5; max_confidence = 0.15; max_duration = 20. }

  let ffast x = { x with min_measures = 2; max_confidence = 42.; max_duration = 20. }

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

let file_size ?(wasm = false) param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      let files =
        if wasm then src ^ " " ^ Filename.remove_extension src ^ ".assets/*" else src
      in
      Format.sprintf "cat %s | wc -c > %s" files dst)

let compr_file_size ?(wasm = false) param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      let input =
        if wasm
        then
          Printf.sprintf
            "echo %s %s | xargs gzip -c"
            src
            (Filename.remove_extension src ^ ".assets/*")
        else Printf.sprintf "sed 's/^ *//g' %s | gzip -c" src
      in
      Format.sprintf "%s | wc -c > %s" input dst)

let bzip2_file_size ?(wasm = false) param =
  compile_no_ext param ~comptime:false (fun ~src ~dst ->
      let input =
        if wasm
        then
          Printf.sprintf
            "echo %s %s | xargs bzip2 -c"
            src
            (Filename.remove_extension src ^ ".assets/*")
        else Printf.sprintf "sed 's/^ *//g' %s | bzip2 -c" src
      in
      Format.sprintf "%s | wc -c > %s" input dst)

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

let _ =
  let compile_only = ref false in
  let full = ref false in
  let interpreter_config = ref "run.config" in
  let param = ref Param.default in
  let reports = ref [] in
  let fast_run () = param := Param.fast !param in
  let ffast_run () = param := Param.ffast !param in
  let verbose () = param := Param.verbose !param in
  let options =
    [ "-compile", Arg.Set compile_only, " only compiles"
    ; "-all", Arg.Set full, " run all benchmarks"
    ; "-config", Arg.Set_string interpreter_config, "<file> use <file> as a config file"
    ; "-fast", Arg.Unit fast_run, " perform less iterations"
    ; "-ffast", Arg.Unit ffast_run, " perform very few iterations"
    ; "-verbose", Arg.Unit verbose, " verbose"
    ]
  in
  Arg.parse
    (Arg.align options)
    (fun report -> reports := report :: !reports)
    (Format.sprintf "Usage: %s [options] [REPORTS.config]*" Sys.argv.(0));
  let compile_only = !compile_only in
  let full = !full in
  let param = !param in
  let interpreters = read_interpreter_config !interpreter_config in
  let filtered, filter_bench =
    match !reports with
    | [] -> false, fun (_, _) -> true
    | reports ->
        List.map reports ~f:read_report_config
        |> List.concat_map ~f:(fun l ->
               List.concat_map l ~f:(fun l ->
                   List.concat_map l ~f:(function
                     | None -> []
                     | Some (p1, p2, _measure, _) -> (
                         match String.split_on_char ~sep:'/' p1 with
                         | [ "results"; "times"; _host; interpreter ] ->
                             [ interpreter, p2 ]
                         | _ -> []))))
        |> List.sort_uniq ~cmp:compare
        |> fun required ->
        true, fun (interp, suite) -> List.mem (interp, Spec.name suite) ~set:required
  in
  let compile = compile param ~comptime:true in
  let compile_jsoo ?(effects = `None) opts =
    compile
      (Format.sprintf
         "js_of_ocaml -q --target-env browser --debug mark-runtime-gen %s %s"
         opts
         (match effects with
         | `None -> "--effects=disabled"
         | `Cps -> "--effects=cps"
         | `Double_translation -> "--effects=double-translation"))
  in
  let compile_wasmoo ?(effects = `None) opts =
    compile
      (Format.sprintf
         "wasm_of_ocaml -q %s %s"
         opts
         (match effects with
         | `None -> ""
         | `Cps -> "--effects=cps"))
  in
  Format.eprintf "Compile@.";
  compile "ocamlc" src Spec.ml code Spec.byte;
  compile "ocamlopt" src Spec.ml code Spec.opt;
  compile_wasmoo "--opt=2" code Spec.byte code Spec.wasm_of_ocaml;
  compile_jsoo "--opt=2" code Spec.byte code Spec.js_of_ocaml;
  compile_jsoo "--opt=3" code Spec.byte code Spec.js_of_ocaml_o3;
  compile_jsoo "--enable=use-js-string" code Spec.byte code Spec.js_of_ocaml_js_string;
  compile_jsoo "--disable inline" code Spec.byte code Spec.js_of_ocaml_inline;
  compile_jsoo "--disable deadcode" code Spec.byte code Spec.js_of_ocaml_deadcode;
  compile_jsoo "--disable compact" code Spec.byte code Spec.js_of_ocaml_compact;
  compile_jsoo "--disable optcall" code Spec.byte code Spec.js_of_ocaml_call;
  compile_jsoo ~effects:`Cps "" code Spec.byte code Spec.js_of_ocaml_effects_cps;
  compile_jsoo
    ~effects:`Double_translation
    ""
    code
    Spec.byte
    code
    Spec.js_of_ocaml_effects_double_translation;
  compile "ocamlc -unsafe" src Spec.ml code Spec.byte_unsafe;
  compile "ocamlopt" src Spec.ml code Spec.opt_unsafe;
  compile_jsoo "" code Spec.byte_unsafe code Spec.js_of_ocaml_unsafe;
  Format.eprintf "Sizes@.";
  let sizes = sizes.Measure.path in
  ml_size param src Spec.ml sizes Spec.ml;
  file_size param code Spec.byte sizes Spec.byte;
  file_size param code Spec.js_of_ocaml sizes (Spec.sub_spec Spec.js_of_ocaml "full");
  file_size
    ~wasm:true
    param
    code
    Spec.wasm_of_ocaml
    sizes
    (Spec.sub_spec Spec.wasm_of_ocaml "full");
  compr_file_size
    param
    code
    Spec.js_of_ocaml
    sizes
    (Spec.sub_spec Spec.js_of_ocaml "gzipped");
  compr_file_size
    param
    code
    Spec.js_of_ocaml_effects_cps
    sizes
    (Spec.sub_spec Spec.js_of_ocaml_effects_cps "gzipped");
  compr_file_size
    param
    code
    Spec.js_of_ocaml_effects_double_translation
    sizes
    (Spec.sub_spec Spec.js_of_ocaml_effects_double_translation "gzipped");
  compr_file_size
    ~wasm:true
    param
    code
    Spec.wasm_of_ocaml
    sizes
    (Spec.sub_spec Spec.wasm_of_ocaml "gzipped");
  bzip2_file_size
    param
    code
    Spec.js_of_ocaml_effects_cps
    sizes
    (Spec.sub_spec Spec.js_of_ocaml_effects_cps "bzip2");
  bzip2_file_size
    param
    code
    Spec.js_of_ocaml_effects_double_translation
    sizes
    (Spec.sub_spec Spec.js_of_ocaml_effects_double_translation "bzip2");
  bzip2_file_size
    param
    code
    Spec.js_of_ocaml
    sizes
    (Spec.sub_spec Spec.js_of_ocaml "bzip2");
  bzip2_file_size
    ~wasm:true
    param
    code
    Spec.wasm_of_ocaml
    sizes
    (Spec.sub_spec Spec.wasm_of_ocaml "bzip2");
  runtime_size
    param
    code
    Spec.js_of_ocaml
    sizes
    (Spec.sub_spec Spec.js_of_ocaml "runtime");
  gen_size param code Spec.js_of_ocaml sizes (Spec.sub_spec Spec.js_of_ocaml "generated");
  gen_size param code Spec.js_of_ocaml_o3 sizes Spec.js_of_ocaml_o3;
  gen_size param code Spec.js_of_ocaml_js_string sizes Spec.js_of_ocaml_js_string;
  gen_size param code Spec.js_of_ocaml_inline sizes Spec.js_of_ocaml_inline;
  gen_size param code Spec.js_of_ocaml_deadcode sizes Spec.js_of_ocaml_deadcode;
  gen_size param code Spec.js_of_ocaml_compact sizes Spec.js_of_ocaml_compact;
  gen_size param code Spec.js_of_ocaml_call sizes Spec.js_of_ocaml_call;
  gen_size param code Spec.js_of_ocaml_effects_cps sizes Spec.js_of_ocaml_effects_cps;
  gen_size
    param
    code
    Spec.js_of_ocaml_effects_double_translation
    sizes
    Spec.js_of_ocaml_effects_double_translation;
  if compile_only then exit 0;
  Format.eprintf "Measure@.";
  if filter_bench ("", Spec.opt) then measure ~param ~code ~meas:times ~spec:Spec.opt "";
  if filter_bench ("", Spec.byte) then measure ~param ~code ~meas:times ~spec:Spec.byte "";
  let all_spec =
    [ Some Spec.js_of_ocaml
    ; Some Spec.js_of_ocaml_o3
    ; Some Spec.js_of_ocaml_js_string
    ; Some Spec.js_of_ocaml_unsafe
    ; Some Spec.js_of_ocaml_inline
    ; Some Spec.js_of_ocaml_deadcode
    ; Some Spec.js_of_ocaml_compact
    ; Some Spec.js_of_ocaml_call
    ; Some Spec.js_of_ocaml_effects_cps
    ; Some Spec.js_of_ocaml_effects_double_translation
    ; Some Spec.wasm_of_ocaml
    ]
  in
  let interpreters, suites =
    if full || filtered
    then interpreters, all_spec
    else
      ( (match interpreters with
        | i :: _ -> [ i ]
        | [] -> [])
      , [ Some Spec.js_of_ocaml; Some Spec.wasm_of_ocaml ] )
  in
  List.iter interpreters ~f:(fun (comp, dir) ->
      (* Measure the time taken by an implementation in Javascript for comparison *)
      if filter_bench (dir, Spec.js)
      then
        measure
          ~param
          ~code:src
          ~meas:Measure.{ times with path = Filename.concat times.path dir }
          ~spec:Spec.js
          comp;
      List.iter suites ~f:(function
        | None -> ()
        | Some suite ->
            if filter_bench (dir, suite)
            then
              measure
                ~param
                ~code
                ~meas:Measure.{ times with path = Filename.concat times.path dir }
                ~spec:suite
                comp))
