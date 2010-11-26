#! /usr/bin/ocaml unix.cma

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

let compile_gen prog src_dir src_spec dst_dir dst_spec =
  mkdir (dir dst_dir dst_spec);
  List.iter
    (fun nm ->
       let src = file src_dir src_spec nm in
       let dst = file dst_dir dst_spec nm in
       if need_update src dst then begin
         let cmd = prog src dst in
         try
           run_command cmd
         with Exit -> ()
       end)
    (benchs src_dir src_spec)

let compile prog = compile_gen (Format.sprintf "%s %s -o %s" prog)

let warm_up_time = 1.0
let min_measures = ref 10
let max_confidence = ref 0.03
let max_duration = ref 1200.

let fast_run () =
  min_measures := 5; max_confidence := 0.15; max_duration := 30.

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

let compile_no_ext prog src_dir src_spec dst_dir dst_spec =
  compile_gen prog src_dir src_spec dst_dir (no_ext dst_spec)
let ml_size =
  compile_no_ext
    (Format.sprintf "perl ./lib/remove_comments.pl %s | wc -c > %s")
let file_size = compile_no_ext (Format.sprintf "wc -c < %s > %s")
let runtime_size = compile_no_ext (Format.sprintf "head -n -1 %s | wc -c > %s")
let gen_size = compile_no_ext (Format.sprintf "tail -1 %s | wc -c > %s")

(****)

let has_ocamljs = Sys.command "ocamljs 2> /dev/null" = 0

let compile_only = ref false
let full = ref false
let conf = ref "config"

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
    done
  with End_of_file -> () end;
  close_in ch;
  interpreters := List.rev !i

let _ =
  let options =
    [("-compile", Arg.Set compile_only, " only compiles");
     ("-full", Arg.Set full, " run all benchmarks");
     ("-config", Arg.Set_string conf, "<file> use <file> as a config file");
     ("-fast", Arg.Unit fast_run, " perform less iterations")]
  in
  Arg.parse (Arg.align options)
    (fun s -> raise (Arg.Bad (Format.sprintf "unknown option `%s'" s)))
    (Format.sprintf "Usage: %s [options]" Sys.argv.(0));

  read_config ();

  compile "ocamlc" src ml code byte;
  compile "ocamlopt" src ml code opt;
  compile "js_of_ocaml" code byte code js_of_ocaml;
  compile "js_of_ocaml -noinline" code byte code js_of_ocaml_inline;
  compile "js_of_ocaml -disable deadcode" code byte code js_of_ocaml_deadcode;
  compile "js_of_ocaml -disable compactexpr" code byte code js_of_ocaml_compact;
  compile "js_of_ocaml -disable optcall" code byte code js_of_ocaml_call;
  if has_ocamljs then compile "ocamljs" src ml code ocamljs;
  compile "ocamlc -unsafe" src ml code byte_unsafe;
  compile "ocamlopt" src ml code opt_unsafe;
  compile "js_of_ocaml" code byte_unsafe code js_of_ocaml_unsafe;
  if has_ocamljs then compile "ocamljs -unsafe" src ml code ocamljs_unsafe;

  ml_size src ml sizes ml;
  file_size code byte sizes byte;
  file_size code js_of_ocaml sizes (sub_spec js_of_ocaml "full");
  runtime_size code js_of_ocaml sizes (sub_spec js_of_ocaml "runtime");
  gen_size code js_of_ocaml sizes (sub_spec js_of_ocaml "generated");
  gen_size code js_of_ocaml_inline sizes js_of_ocaml_inline;
  gen_size code js_of_ocaml_deadcode sizes js_of_ocaml_deadcode;
  gen_size code js_of_ocaml_compact sizes js_of_ocaml_compact;
  gen_size code js_of_ocaml_call sizes js_of_ocaml_call;
  if has_ocamljs then file_size code ocamljs sizes ocamljs;

  if !compile_only then exit 0;

  measure code times opt "";
  measure code times byte "";
  let (compilers, suites) =
    if !full then
      (!interpreters,
       [js_of_ocaml; (*js_of_ocaml_unsafe; ocamljs; ocamljs_unsafe*)])
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
