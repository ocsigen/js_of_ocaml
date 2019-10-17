open! Js_of_ocaml_compiler.Stdlib
open Js_of_ocaml_compiler
open Cmdliner

type options =
  { files : string list
  ; output_file : string
  ; include_dirs : string list }

let options =
  let files =
    let doc = "files [$(docv)]." in
    Arg.(value & pos_all string [] & info [] ~docv:"FILES" ~doc)
  in
  let output_file =
    let doc = "Set output file name to [$(docv)]." in
    Arg.(required & opt (some string) None & info ["o"] ~docv:"FILE" ~doc)
  in
  let include_dirs =
    let doc = "Add [$(docv)] to the list of include directories." in
    Arg.(value & opt_all string [] & info ["I"] ~docv:"DIR" ~doc)
  in
  Term.(
    pure (fun files output_file include_dirs -> {files; output_file; include_dirs}) files
    $ output_file
    $ include_dirs)

let info =
  let doc = "" in
  Term.info "jsoo_fs" ~doc

let temp_file_name =
  (* Inlined unavailable Filename.temp_file_name. Filename.temp_file gives
     us incorrect permissions. https://github.com/ocsigen/js_of_ocaml/issues/182 *)
  let prng = lazy (Random.State.make_self_init ()) in
  fun ~temp_dir prefix suffix ->
    let rnd = Random.State.bits (Lazy.force prng) land 0xFFFFFF in
    Filename.concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let gen_file file f =
  let f_tmp =
    temp_file_name ~temp_dir:(Filename.dirname file) (Filename.basename file) ".tmp"
  in
  try
    let ch = open_out_bin f_tmp in
    (try f ch
     with e ->
       close_out ch;
       raise e);
    close_out ch;
    (try Sys.remove file with Sys_error _ -> ());
    Sys.rename f_tmp file
  with exc ->
    Sys.remove f_tmp;
    raise exc

let f {files; output_file; include_dirs} =
  let code =
    {|
//Provides: caml_create_file_extern
function caml_create_file_extern(name,content){
  if(joo_global_object.caml_create_file)
    joo_global_object.caml_create_file(name,content);
  else {
    if(!joo_global_object.caml_fs_tmp) joo_global_object.caml_fs_tmp = [];
    joo_global_object.caml_fs_tmp.push({name:name,content:content});
  }
  return 0;
}
|}
  in
  let fragments = Linker.parse_string code in
  let () =
    List.iter fragments ~f:(fun fr -> Linker.load_fragment ~filename:"<dummy>" fr)
  in
  let instr =
    PseudoFs.f
      ~prim:`caml_create_file_extern
      ~cmis:StringSet.empty
      ~files
      ~paths:include_dirs
  in
  let code = Code.prepend Code.empty instr in
  gen_file output_file (fun chan ->
      let pfs_fmt = Pretty_print.to_out_channel chan in
      Driver.f
        ~standalone:true
        ~global:`Auto
        pfs_fmt
        (Parse_bytecode.Debug.create ())
        code)

let main = Cmdliner.Term.(pure f $ options), info

let _ =
  Timer.init Sys.time;
  try
    Cmdliner.Term.eval ~catch:false ~argv:(Util.normalize_argv ~warn_:true Sys.argv) main
  with
  | (Match_failure _ | Assert_failure _ | Not_found) as exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf
        "%s: You found a bug. Please report it at \
         https://github.com/ocsigen/js_of_ocaml/issues :@."
        Sys.argv.(0);
      Format.eprintf "Error: %s@." (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
  | Failure s ->
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) s;
      exit 1
  | exc ->
      let backtrace = Printexc.get_backtrace () in
      Format.eprintf "%s: Error: %s@." Sys.argv.(0) (Printexc.to_string exc);
      prerr_string backtrace;
      exit 1
