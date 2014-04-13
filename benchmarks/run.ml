#! /usr/bin/ocaml unix.cma
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

let verbose = ref true;;

#use "lib/common.ml"

(****)

let run_command cmd =
  if !verbose then Format.printf "+ %s@." cmd;
  match Unix.system cmd with
    Unix.WEXITED res when res <> 0 ->
      Format.eprintf "Command '%s' failed with exit code %d.@." cmd res;
      raise Exit
  | Unix.WSIGNALED s ->
      Format.eprintf "Command '%s' killed with signal %d.@." cmd s;
      raise Exit
  | _ ->
    ()

let time cmd =
  let t1 = (Unix.times ()).Unix.tms_cutime in
  run_command cmd;
  let t2 = (Unix.times ()).Unix.tms_cutime in
  t2 -. t1

(****)

let compile_gen ~comptime prog src_dir src_spec dst_dir dst_spec =
  mkdir (dir dst_dir dst_spec);
  List.iter
    (fun nm ->
       let src = file src_dir src_spec nm in
       let dst = file dst_dir dst_spec nm in
       if need_update src dst then begin
         let cmd = prog src dst in
         try
           if comptime
           then write_measures compiletimes dst_spec nm [time cmd]
           else run_command cmd
         with Exit -> ()

       end)
    (benchs src_dir src_spec)

let compile ~comptime prog =
  compile_gen ~comptime (Format.sprintf "%s %s -o %s" prog)

let warm_up_time = 1.0
let min_measures = ref 10
let max_confidence = ref 0.03
let max_duration = ref 1200.

let fast_run () =
  min_measures := 5; max_confidence := 0.15; max_duration := 30.

let ffast_run () =
  min_measures := 2; max_confidence := 42.; max_duration := 30.

(****)

let need_more l =
  let a = Array.of_list l in
  let (m, i) = mean_with_confidence a in
  let n = Array.length a in
  Format.eprintf "==> %f +/- %f / %f %d@." m i (i /. m) n;
  n < !min_measures || (i /. m > !max_confidence /. 2.)

let warm_up cmd =
  let t = ref 0. in
  while !t < warm_up_time do
    let t' = time cmd in
    if t' > !max_duration then raise Exit;
    t := !t +. t'
  done

let rec measure_rec cmd l =
  let t = time cmd in
  let l = t :: l in
  if need_more l then measure_rec cmd l else l

let measure_one code meas spec nm cmd =
  let l =
    if measures_need_update code meas spec nm then [] else
    read_measures meas spec nm
  in
  if need_more l then begin
    warm_up cmd;
    let l = measure_rec cmd l in
    write_measures meas spec nm l;
    l
  end else
    l

let measure code meas spec cmd =
  List.iter
    (fun nm ->
       let cmd = cmd ^ file code spec nm in
       try
         ignore (measure_one code meas spec nm cmd)
       with Exit -> ())
    (benchs code spec)

(****)

let compile_no_ext ~comptime prog src_dir src_spec dst_dir dst_spec =
  compile_gen ~comptime prog src_dir src_spec dst_dir (no_ext dst_spec)

let ml_size =
  compile_no_ext
    ~comptime:false
    (Format.sprintf "perl ./lib/remove_comments.pl %s | sed 's/^ *//g' | wc -c > %s")

let file_size =
  compile_no_ext ~comptime:false (Format.sprintf "wc -c < %s > %s")

let compr_file_size =
  compile_no_ext
    ~comptime:false
    (Format.sprintf "sed 's/^ *//g' %s | gzip -c | wc -c > %s")

(* let runtime_size = *)
(*   compile_no_ext ~comptime:false (Format.sprintf "head -n -1 %s | wc -c > %s") *)

let gen_size =
  compile_no_ext ~comptime:false (Format.sprintf "tail -1 %s | wc -c > %s")

(****)

let compile_only = ref false
let full = ref false
let conf = ref "run.config"
let do_ocamljs = ref true
let nobyteopt = ref false

let has_ocamljs = Sys.command "ocamljs 2> /dev/null" = 0
let run_ocamljs () = !do_ocamljs && has_ocamljs


let interpreters = ref []

let read_config () =
  let f = !conf in
  if not (Sys.file_exists f) then begin
    Format.eprintf "Configuration file '%s' not found!@." f;
    exit 1
  end;
  let i = ref [] in
  let ch = open_in f in
  let split_at_space l =
    let i = String.index l ' ' in
    (String.sub l 0 i, String.sub l (i + 1) (String.length l - i - 1))
  in
  begin try
    while true do
      let l = input_line ch in
      if l.[0] <> '#'
      then begin
        try
          let (kind, rem) = split_at_space l in
          match kind with
              "interpreter" ->
                let (nm, cmd) = split_at_space rem in
                i := (cmd ^ " ", nm) :: !i
            | _ ->
              Format.eprintf "Unknown config option '%s'@." kind;
              exit 1
        with Not_found ->
          Format.eprintf "Bad config line '%s'@." l;
          exit 1
      end
    done
  with End_of_file -> () end;
  close_in ch;
  interpreters := List.rev !i

let _ =
  let options =
    [("-compile", Arg.Set compile_only, " only compiles");
     ("-all", Arg.Set full, " run all benchmarks");
     ("-config", Arg.Set_string conf, "<file> use <file> as a config file");
     ("-fast", Arg.Unit fast_run, " perform less iterations");
     ("-ffast", Arg.Unit ffast_run, " perform very few iterations");
     ("-noocamljs", Arg.Clear do_ocamljs, " do not run ocamljs");
     ("-nobyteopt", Arg.Set nobyteopt, " do not run benchs on bytecode and native programs")]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));

  read_config ();

  compile ~comptime:true "ocamlc" src ml code byte;
  compile ~comptime:true "ocamlopt" src ml code opt;
  compile ~comptime:true "js_of_ocaml" code byte code js_of_ocaml;
  compile ~comptime:true "js_of_ocaml -disable inline" code byte code js_of_ocaml_inline;
  compile ~comptime:true "js_of_ocaml -disable deadcode" code byte code js_of_ocaml_deadcode;
  compile ~comptime:true "js_of_ocaml -disable compact" code byte code js_of_ocaml_compact;
  compile ~comptime:true "js_of_ocaml -disable optcall" code byte code js_of_ocaml_call;
  if run_ocamljs () then compile ~comptime:true "ocamljs" src ml code ocamljs;
  compile ~comptime:true "ocamlc -unsafe" src ml code byte_unsafe;
  compile ~comptime:true "ocamlopt" src ml code opt_unsafe;
  compile ~comptime:true "js_of_ocaml" code byte_unsafe code js_of_ocaml_unsafe;
  if run_ocamljs () then compile ~comptime:true "ocamljs -unsafe" src ml code ocamljs_unsafe;

  ml_size src ml sizes ml;
  file_size code byte sizes byte;
  file_size code js_of_ocaml sizes (sub_spec js_of_ocaml "full");
  compr_file_size code js_of_ocaml sizes (sub_spec js_of_ocaml "gzipped");
  (* runtime_size code js_of_ocaml sizes (sub_spec js_of_ocaml "runtime"); *)
  gen_size code js_of_ocaml sizes (sub_spec js_of_ocaml "generated");
  gen_size code js_of_ocaml_inline sizes js_of_ocaml_inline;
  gen_size code js_of_ocaml_deadcode sizes js_of_ocaml_deadcode;
  gen_size code js_of_ocaml_compact sizes js_of_ocaml_compact;
  gen_size code js_of_ocaml_call sizes js_of_ocaml_call;
  if run_ocamljs () then compr_file_size code ocamljs sizes ocamljs;

  if !compile_only then exit 0;

  if not !nobyteopt then begin
    measure code times opt "";
    measure code times byte "";
  end;

  let (compilers, suites) =
    if !full then
      (!interpreters,
       [js_of_ocaml; 
        js_of_ocaml_unsafe; 
        js_of_ocaml_inline; 
        js_of_ocaml_deadcode; 
        js_of_ocaml_compact; 
        js_of_ocaml_call; 
        ocamljs;
        ocamljs_unsafe; ])
    else
      (begin match !interpreters with i :: r -> [i] | [] -> [] end,
       [js_of_ocaml])
  in
  List.iter
    (fun (comp, dir) ->
      measure src (Filename.concat times dir) js comp;
      List.iter
        (fun suite -> measure code (Filename.concat times dir) suite comp)
        suites)
    compilers
