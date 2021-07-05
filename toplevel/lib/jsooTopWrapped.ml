open! Js_of_ocaml
open! Js_of_ocaml_compiler
open! Js_of_ocaml_compiler.Stdlib

type loc =
  { loc_start : int * int
  ; loc_end : int * int }

type error =
  { msg : string
  ; locs : loc list }

type warning = error

type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

let warnings = ref []

let convert_loc loc =
  let _file1, line1, col1 = Location.get_pos_info loc.Location.loc_start in
  let _file2, line2, col2 = Location.get_pos_info loc.Location.loc_end in
  {loc_start = line1, col1; loc_end = line2, col2}

let () =
  let warning_reporter = !Location.warning_reporter in
  Location.warning_reporter :=
    fun loc w ->
      match warning_reporter loc w with
      | Some report ->
          let buf = Buffer.create 503 in
          let ppf = Format.formatter_of_buffer buf in
          let printer = !Location.report_printer () in
          printer.pp printer ppf report;
          let msg = Buffer.contents buf in
          let loc = convert_loc loc in
          warnings := {msg; locs = [loc]} :: !warnings;
          Some report
      | None -> None

(* Workaround Marshal bug triggered by includemod.ml:607 *)
let () = Clflags.error_size := 0

(* Disable inlining of JSOO which may blow the JS stack *)
let () = Js_of_ocaml_compiler.Config.Flag.disable "inline"

let return_success e = Success (e, !warnings)

let return_error e = Error (e, !warnings)

(* let return_unit_success = return_success () *)

(** Error handling *)

let report_error_rec ppf (report : Location.report) =
  let locs = ref [] in
  let printer = !Location.report_printer () in
  let printer =
    { printer with
      pp_main_loc =
        (fun pr report fmt loc ->
          locs := loc :: !locs;
          printer.pp_main_loc pr report fmt loc)
    ; pp_submsg_loc =
        (fun pr report fmt loc ->
          locs := loc :: !locs;
          printer.pp_submsg_loc pr report fmt loc) }
  in
  printer.pp printer ppf report;
  List.map ~f:convert_loc !locs

let report_error err =
  let buf = Buffer.create 503 in
  let ppf = Format.formatter_of_buffer buf in
  let locs = report_error_rec ppf err in
  Format.pp_print_flush ppf ();
  let msg = Buffer.contents buf in
  {msg; locs}

let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok error) -> report_error error
  | Some `Already_displayed | None ->
      let msg = Printexc.to_string exn in
      {msg; locs = []}

let return_exn exn = return_error (error_of_exn exn)

(** Execution helpers *)

let trim_end s =
  let ws = function
    | ' ' | '\t' | '\n' -> true
    | _ -> false
  in
  let len = String.length s in
  let stop = ref (len - 1) in
  while !stop > 0 && ws s.[!stop] do
    decr stop
  done;
  String.sub s ~pos:0 ~len:(!stop + 1)

let normalize code =
  let content = trim_end code in
  let len = String.length content in
  if String.is_empty content
  then content
  else if len > 2 && Char.equal content.[len - 2] ';' && Char.equal content.[len - 1] ';'
  then content ^ "\n"
  else content ^ " ;;\n"

let init_loc lb filename =
  Location.input_name := filename;
  Location.input_lexbuf := Some lb;
  Location.init lb filename

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len', nl =
      try String.index_from s !p '\n' - !p + 1, false
      with _ -> String.length s - !p, true
    in
    let len'' = min len len' in
    String.blit ~src:s ~src_pos:!p ~dst:buffer ~dst_pos:0 ~len:len'';
    (match ppf with
    | Some ppf ->
        Format.fprintf ppf "%s" (Bytes.sub_string buffer ~pos:0 ~len:len'');
        if nl then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

let execute () ?ppf_code ?(print_outcome = true) ~ppf_answer code =
  let code = normalize code in
  let lb =
    match ppf_code with
    | Some ppf_code -> Lexing.from_function (refill_lexbuf code (ref 0) (Some ppf_code))
    | None -> Lexing.from_string code
  in
  init_loc lb "//toplevel//";
  warnings := [];
  let rec loop () =
    let phr = !Toploop.parse_toplevel_phrase lb in
    let phr = JsooTopPpx.preprocess_phrase phr in
    let success = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    if success then loop () else return_success false
  in
  try
    let res = loop () in
    flush_all ();
    res
  with
  | End_of_file ->
      flush_all ();
      return_success true
  | exn ->
      flush_all ();
      return_error (error_of_exn exn)

let use_string () ?(filename = "//toplevel//") ?(print_outcome = true) ~ppf_answer code =
  let lb = Lexing.from_string code in
  init_loc lb filename;
  warnings := [];
  try
    List.iter
      ~f:(fun phr ->
        if not (Toploop.execute_phrase print_outcome ppf_answer phr)
        then raise Exit
        else Format.pp_print_flush ppf_answer ())
      (List.map ~f:JsooTopPpx.preprocess_phrase (!Toploop.parse_use_file lb));
    flush_all ();
    return_success true
  with
  | Exit ->
      flush_all ();
      Format.pp_print_flush ppf_answer ();
      return_success false
  | exn ->
      flush_all ();
      return_error (error_of_exn exn)

let parse_mod_string modname sig_code impl_code =
  let open Parsetree in
  let open Ast_helper in
  let str =
    let impl_lb = Lexing.from_string impl_code in
    init_loc impl_lb (String.uncapitalize_ascii modname ^ ".ml");
    Parse.implementation impl_lb
  in
  let m =
    match sig_code with
    | None -> Mod.structure str
    | Some sig_code ->
        let sig_lb = Lexing.from_string sig_code in
        init_loc sig_lb (String.uncapitalize_ascii modname ^ ".mli");
        let s = Parse.interface sig_lb in
        Mod.constraint_ (Mod.structure str) (Mty.signature s)
  in
  Ptop_def [Str.module_ (Mb.mk (Location.mknoloc modname) m)]

let use_mod_string () ?(print_outcome = true) ~ppf_answer ~modname ?sig_code impl_code =
  if not (String.equal (String.capitalize_ascii modname) modname)
  then
    invalid_arg
      "Tryocaml_toploop.use_mod_string: the module name must start with a capital letter.";
  warnings := [];
  try
    let phr =
      JsooTopPpx.preprocess_phrase @@ parse_mod_string modname sig_code impl_code
    in
    let res = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    flush_all ();
    return_success res
  with exn ->
    flush_all ();
    return_error (error_of_exn exn)

(* Extracted from the "execute" function in "ocaml/toplevel/toploop.ml" *)
let check_phrase env = function
  | Parsetree.Ptop_def sstr ->
      Typecore.reset_delayed_checks ();
      let str, sg, sg_names, newenv = Typemod.type_toplevel_phrase env sstr in
      let sg' = Typemod.Signature_names.simplify newenv sg_names sg in
      ignore (Includemod.signatures env sg sg');
      Typecore.force_delayed_checks ();
      let _lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      newenv
  | Parsetree.Ptop_dir _ -> env

let check () ?(setenv = false) code =
  let lb = Lexing.from_string code in
  init_loc lb "//toplevel//";
  warnings := [];
  try
    let env =
      List.fold_left
        ~f:check_phrase
        ~init:!Toploop.toplevel_env
        (List.map ~f:JsooTopPpx.preprocess_phrase (!Toploop.parse_use_file lb))
    in
    if setenv then Toploop.toplevel_env := env;
    return_success ()
  with
  | End_of_file -> return_success ()
  | exn -> return_exn exn
