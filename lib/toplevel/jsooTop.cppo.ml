(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2014 Hugo Heuzard
 * Copyright (C) 2016 OCamlPro
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

open Compiler
let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

let setup = lazy (
  Hashtbl.add Toploop.directive_table "enable" (Toploop.Directive_string Option.Optim.enable);
  Hashtbl.add Toploop.directive_table "disable" (Toploop.Directive_string Option.Optim.disable);
  Hashtbl.add Toploop.directive_table "debug_on" (Toploop.Directive_string Option.Debug.enable);
  Hashtbl.add Toploop.directive_table "debug_off" (Toploop.Directive_string Option.Debug.disable);
  Hashtbl.add Toploop.directive_table "tailcall" (Toploop.Directive_string (Option.Param.set "tc"));
  Topdirs.dir_directory "/cmis";
  let initial_primitive_count =
    Array.length (split_primitives (Symtable.data_primitive_names ())) in

  let compile s =
    let prims =
      split_primitives (Symtable.data_primitive_names ()) in
    let unbound_primitive p =
      try ignore (Js.Unsafe.eval_string p); false with _ -> true in
    let stubs = ref [] in
    Array.iteri
      (fun i p ->
         if i >= initial_primitive_count && unbound_primitive p then
           stubs :=
             Format.sprintf
               "function %s(){caml_failwith(\"%s not implemented\")}" p p
             :: !stubs)
      prims;
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Format.(pp_print_flush std_formatter ());
    Format.(pp_print_flush err_formatter ());
    flush stdout; flush stderr;
    let res = Buffer.contents b in
    let res = String.concat "" !stubs ^ res in
    Js.Unsafe.global##toplevelEval(res)
  in
  Js.Unsafe.global##toplevelCompile <- compile (*XXX HACK!*);
  Js.Unsafe.global##toplevelEval <- (fun x -> Js.Unsafe.eval_string x);
  ())

let refill_lexbuf s p ppf buffer len =
  if !p = String.length s
  then 0
  else
    let len',nl =
      try String.index_from s !p '\n' - !p + 1,false
      with _ -> String.length s - !p,true in
    let len'' = min len len' in
    String.blit s !p buffer 0 len'';
    (match ppf with
     | Some ppf ->
       Format.fprintf ppf "%s" (Bytes.sub_string buffer 0 len'');
       if nl then Format.pp_print_newline ppf ();
       Format.pp_print_flush ppf ()
     | None -> ());
    p:=!p + len'';
    len''

let use ffp content =
  let name = "/dev/fake_stdin" in
  if Sys.file_exists name then Sys.remove name;
  Sys_js.register_file ~name ~content;
  Toploop.use_silently ffp name

let execute printval ?pp_code ?highlight_location  pp_answer s =
  let lb = Lexing.from_function (refill_lexbuf s (ref 0) pp_code) in
  try
    while true do
      try
        let phr = !Toploop.parse_toplevel_phrase lb in
        ignore(Toploop.execute_phrase printval pp_answer phr)
      with
      | End_of_file ->
        raise End_of_file
      | JsooTopError.Camlp4 (loc,_exn) -> 
	 begin match highlight_location with
	       | None -> () 
	       | Some f -> f loc
	 end;
      | x ->
	 begin match highlight_location with
	       | None -> () 
	       | Some f ->   
		  match JsooTopError.loc x with
		  | None -> ()
		  | Some loc -> f loc
	 end;
         Errors.report_error Format.err_formatter x;
    done
  with End_of_file ->
    flush_all ()

let initialize () =
  Sys.interactive := false;
  Lazy.force setup;
  Toploop.initialize_toplevel_env ();
  Toploop.input_name := "//toplevel//";
  Sys.interactive := true

module Wrapped = struct

  type loc = {
    loc_start: int * int;
    loc_end: int * int;
  }

  type error = {
    msg: string;
    locs: loc list;
    if_highlight: string;
  }

  type warning = error

  type 'a result =
    | Success of 'a * warning list
    | Error of error * warning list

  let warnings = ref []

  let convert_loc loc =
    let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
    let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
    { loc_start = (line1, col1) ; loc_end = (line2, col2) }

#ifdef WITH_WARNINGS
  let () =
    Location.warning_printer :=
      (fun loc _fmt w ->
         if Warnings.is_active w then begin
           let buf = Buffer.create 503 in
           let ppf = Format.formatter_of_buffer buf in
           Location.print ppf loc;
           Format.fprintf ppf "Warning %a@." Warnings.print w;
           let msg = Buffer.contents buf in
           Buffer.reset buf;
           Format.fprintf ppf "Warning %a@." Warnings.print w;
           let if_highlight = Buffer.contents buf in
           let loc = convert_loc loc in
           warnings := { msg; locs = [loc]; if_highlight } :: !warnings
         end)
#endif

  (* Workaround Marshal bug triggered by includemod.ml:607 *)
  let () = Clflags.error_size := 0

  (* Disable inlining of JSOO which may blow the JS stack *)
  let () = Compiler.Option.Optim.disable "inline"

  let return_success e = Success (e, !warnings)
  let return_error e = Error (e, !warnings)
  (* let return_unit_success = return_success () *)

  (** Error handling *)
  let dummy_ppf = Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let rec report_error_rec hg_ppf ppf {Location.loc; msg; sub; if_highlight} =
    Location.print ppf loc;
    Format.pp_print_string ppf msg;
    let hg_ppf =
      if if_highlight <> "" then
        (Format.pp_print_string hg_ppf if_highlight; dummy_ppf)
      else
        (Format.pp_print_string hg_ppf msg; hg_ppf) in
    let locs =
      List.concat @@
      List.map
        (fun err ->
           Format.pp_force_newline ppf ();
           Format.pp_open_box ppf 2;
           let locs = report_error_rec hg_ppf ppf err in
           Format.pp_close_box ppf ();
           locs)
        sub in
    convert_loc loc :: locs

  let report_error err =
    let buf = Buffer.create 503 in
    let ppf = Format.formatter_of_buffer buf in
    let hg_buf = Buffer.create 503 in
    let hg_ppf = Format.formatter_of_buffer hg_buf in
    let locs = report_error_rec hg_ppf ppf err in
    Format.pp_print_flush ppf ();
    Format.pp_print_flush hg_ppf ();
    let msg = Buffer.contents buf in
    let if_highlight = Buffer.contents hg_buf in
    { msg; locs; if_highlight; }

  let error_of_exn exn =
    match Location.error_of_exn exn with
    | None ->
      let msg = Printexc.to_string exn in
      { msg; locs = []; if_highlight = msg }
    | Some error -> report_error error

  let return_exn exn = return_error (error_of_exn exn)

  (** Execution helpers *)

  let trim_end s =
    let ws c = c = ' ' || c = '\t' || c = '\n' in
    let len = String.length s in
    let stop = ref (len - 1) in
    while !stop > 0 && (ws s.[!stop])
    do decr stop done;
    String.sub s 0 (!stop + 1)

  let normalize code =
    let content = trim_end code in
    let len = String.length content in
    if content = "" then
      content
    else if (len > 2
             && content.[len - 2] = ';'
             && content.[len - 1] = ';') then
      content ^ "\n"
    else
      content ^ " ;;\n"

  let init_loc lb filename =
    Location.input_name := filename;
    Location.input_lexbuf := Some lb;
    Location.init lb filename

  let execute () ?ppf_code ?(print_outcome  = true) ~ppf_answer code =
    let code = normalize code in
    let lb =
      match ppf_code with
      | Some ppf_code ->
        Lexing.from_function (refill_lexbuf code (ref 0) (Some ppf_code))
      | None -> Lexing.from_string code in
    init_loc lb "//toplevel//";
    warnings := [];
    let rec loop () =
      let phr = !Toploop.parse_toplevel_phrase lb in
      let phr = JsooTopPpx.preprocess_phrase phr in
      let success = Toploop.execute_phrase print_outcome ppf_answer phr in
      Format.pp_print_flush ppf_answer ();
      if success then loop () else return_success false in
    try let res = loop () in flush_all () ; res
    with
    | End_of_file ->
        flush_all ();
        return_success true
    | exn ->
        flush_all ();
        return_error (error_of_exn exn)

  let use_string
      () ?(filename = "//toplevel//") ?(print_outcome  = true) ~ppf_answer code =
    let lb = Lexing.from_string code in
    init_loc lb filename;
    warnings := [];
    try
      List.iter
        (fun phr ->
           if not (Toploop.execute_phrase print_outcome ppf_answer phr) then
             raise Exit
           else
             Format.pp_print_flush ppf_answer ())
        (List.map JsooTopPpx.preprocess_phrase (!Toploop.parse_use_file lb)) ;
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
      init_loc impl_lb (String.uncapitalize modname ^ ".ml");
      Parse.implementation impl_lb in
    let m =
      match sig_code with
      | None -> (Mod.structure str)
      | Some sig_code ->
        let sig_lb = Lexing.from_string sig_code in
        init_loc sig_lb (String.uncapitalize modname ^ ".mli");
        let s = Parse.interface sig_lb in
        Mod.constraint_ (Mod.structure str) (Mty.signature s) in
    Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc modname) m) ]

  let use_mod_string
      () ?(print_outcome  = true) ~ppf_answer ~modname ?sig_code impl_code =
    if String.capitalize modname <> modname then
      invalid_arg
        "Tryocaml_toploop.use_mod_string: \
         the module name must start with a capital letter.";
    warnings := [];
    try
      let phr =
        JsooTopPpx.preprocess_phrase @@
        parse_mod_string modname sig_code impl_code in
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
      let (str, sg, newenv) = Typemod.type_toplevel_phrase env sstr in
      let sg' = Typemod.simplify_signature sg in
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
          check_phrase
          !Toploop.toplevel_env
          (List.map
             JsooTopPpx.preprocess_phrase
             (!Toploop.parse_use_file lb)) in
      if setenv then Toploop.toplevel_env := env;
      return_success ()
    with
    | End_of_file -> return_success ()
    | exn -> return_exn exn

end

let syntaxes = ref []
let register_camlp4_syntax name f =
  syntaxes := name :: !syntaxes;
  f (fun (_name,cb) ->
      (* Format.eprintf "execute callback for %s@." name; *)
      cb ())
let get_camlp4_syntaxes () = List.rev !syntaxes
