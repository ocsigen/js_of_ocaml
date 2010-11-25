#! /usr/bin/ocaml unix.cma

let verbose = ref true;;

#use "lib/common.ml"

(****)

(*
let run_command cmd =
  if !verbose then Format.printf "+ %s@." cmd;
  let res = Sys.command cmd in
  if res <> 0 then begin
    Format.eprintf "Command '%s' failed with exit code %d.@." cmd res;
    raise Exit
  end
*)

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

let compile prog src_dir src_spec dst_dir dst_spec =
  mkdir (dir dst_dir dst_spec);
  List.iter
    (fun nm ->
       let src = file src_dir src_spec nm in
       let dst = file dst_dir dst_spec nm in
       if need_update src dst then begin
         let cmd = Format.sprintf "%s %s -o %s" prog src dst in
         run_command cmd
       end)
    (benchs src_dir src_spec)

let warm_up_time = 1.0
let min_measures = 10
let max_confidence = 0.03
let max_duration = 1200.
(*
let warm_up_time = 0.0
let min_measures = 1
let max_confidence = 1000.
*)

(****)

let need_more l =
  let a = Array.of_list l in
  let (m, i) = mean_with_confidence a in
  let n = Array.length a in
  Format.eprintf "==> %f +/- %f / %f %d@." m i (i /. m) n;
  n < min_measures || (i /. m > max_confidence /. 2.)

let warm_up cmd =
  let t = ref 0. in
  while !t < warm_up_time do
    let t' = time cmd in
    if t' > max_duration then raise Exit;
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

let has_ocamljs = Sys.command "ocamljs 2> /dev/null" = 0

let compile_only = ref false
let full = ref false

let _ =
  let options =
    [("-compile", Arg.Set compile_only, "only compiles");
     ("-full", Arg.Set full, "run all benchmarks")]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));

  compile "ocamlc" src ml code byte;
  compile "ocamlopt" src ml code opt;
  compile "js_of_ocaml" code byte code js_of_ocaml;
  if has_ocamljs then compile "ocamljs" src ml code ocamljs;
  compile "ocamlc -unsafe" src ml code byte_unsafe;
  compile "ocamlopt" src ml code opt_unsafe;
  compile "js_of_ocaml" code byte_unsafe code js_of_ocaml_unsafe;
  if has_ocamljs then compile "ocamljs -unsafe" src ml code ocamljs_unsafe;

  if !compile_only then exit 0;

  measure code meas opt "";
  measure code meas byte "";
  let (compilers, suites) =
    if !full then
      ([(v8, "v8"); (tm, "tm"); (nitro, "nitro")],
       [js_of_ocaml; (*js_of_ocaml_unsafe; ocamljs; ocamljs_unsafe*)])
    else
      ([(v8, "v8")], [js_of_ocaml])
  in
  List.iter
    (fun (comp, dir) ->
      measure src (Filename.concat meas dir) js comp;
      List.iter
        (fun suite ->
          measure code (Filename.concat meas dir) suite comp)
        suites)
    compilers
