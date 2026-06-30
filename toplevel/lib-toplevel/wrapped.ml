(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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

open! Js_of_ocaml_compiler
open! Js_of_ocaml_compiler.Stdlib

type loc =
  { loc_start : int * int
  ; loc_end : int * int
  }

type error =
  { msg : string
  ; locs : loc list
  }

type warning = error

type 'a result =
  | Success of 'a * warning list
  | Error of error * warning list

let convert_loc loc =
  let _file1, line1, col1 = Location.get_pos_info loc.Location.loc_start in
  let _file2, line2, col2 = Location.get_pos_info loc.Location.loc_end in
  { loc_start = line1, col1; loc_end = line2, col2 }

(* Run [f] with a capturing [Location.warning_reporter] and Marshal-safe
   [Clflags.error_size] installed; restore both on exit. [f] returns
   [`Ok v] or [`Err e]; [with_capture] pairs that with the captured
   warning list to produce a [result]. All mutable state is local to
   this call — the module exposes no global warning ref. *)
let with_capture (f : unit -> [ `Ok of 'a | `Err of error ]) : 'a result =
  let warnings = ref [] in
  let prev_reporter = !Location.warning_reporter in
  let prev_error_size = !Clflags.error_size in
  (* Capture warnings as first-class data and return [None] so the
     default Warnings machinery does not *also* render them through
     [report_printer]. Consumers of [Wrapped] get warnings on the
     result; if we returned [Some], they would additionally be printed
     to the toplevel's error stream (stderr in the worker, [pp_stderr]
     on the host), producing duplicated output. *)
  (Location.warning_reporter :=
     fun loc w ->
       match prev_reporter loc w with
       | Some report ->
           let buf = Buffer.create 503 in
           let ppf = Format.formatter_of_buffer buf in
           let printer = !Location.report_printer () in
           printer.pp printer ppf report;
           Format.pp_print_flush ppf ();
           let msg = Buffer.contents buf in
           let loc = convert_loc loc in
           warnings := { msg; locs = [ loc ] } :: !warnings;
           None
       | None -> None);
  (* [Includemod] attaches expanded signature context to module-type
     mismatch error records. That context can hold closures (e.g. from
     [Location.report_printer]), which are not marshallable — so when
     [Async] ships an error across a Worker boundary via [Json.output]
     (built on [Marshal]) it raises "function value". Setting
     [error_size := 0] disables the enrichment; errors still report
     the mismatch, just without the fancy expansion. *)
  Clflags.error_size := 0;
  let raw =
    Fun.protect
      ~finally:(fun () ->
        Location.warning_reporter := prev_reporter;
        Clflags.error_size := prev_error_size)
      f
  in
  (* Warnings are prepended to [warnings] as the typechecker walks
     the source, so it's in reverse source order. Reverse here so
     consumers iterate top-to-bottom like every other OCaml tool. *)
  let ws = List.rev !warnings in
  match raw with
  | `Ok v -> Success (v, ws)
  | `Err e -> Error (e, ws)

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
          printer.pp_submsg_loc pr report fmt loc)
    }
  in
  printer.pp printer ppf report;
  (* [locs] was built by prepending in callback order (main, then each
     submsg), so reverse to hand back [main; sub1; sub2; ...]. *)
  List.rev_map ~f:convert_loc !locs

let report_error err =
  let buf = Buffer.create 503 in
  let ppf = Format.formatter_of_buffer buf in
  let locs = report_error_rec ppf err in
  Format.pp_print_flush ppf ();
  let msg = Buffer.contents buf in
  { msg; locs }

let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok error) -> report_error error
  | Some `Already_displayed | None ->
      let msg = Printexc.to_string exn in
      { msg; locs = [] }

(** Execution helpers *)

let trim_end s =
  let ws = function
    | ' ' | '\t' | '\n' -> true
    | _ -> false
  in
  let len = String.length s in
  let stop = ref (len - 1) in
  while !stop >= 0 && ws s.[!stop] do
    decr stop
  done;
  String.sub s ~pos:0 ~len:(!stop + 1)

let normalize code =
  let content = trim_end code in
  if String.is_empty content
  then content
  else if String.ends_with ~suffix:";;" content
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
        if nl && len'' = len' then Format.pp_print_newline ppf ();
        Format.pp_print_flush ppf ()
    | None -> ());
    p := !p + len'';
    len''

let execute () ?ppf_code ?(print_outcome = true) ~ppf_answer code =
  with_capture @@ fun () ->
  let code = normalize code in
  let lb =
    match ppf_code with
    | Some ppf_code -> Lexing.from_function (refill_lexbuf code (ref 0) (Some ppf_code))
    | None -> Lexing.from_string code
  in
  init_loc lb "//toplevel//";
  let rec loop () =
    let phr = !Toploop.parse_toplevel_phrase lb in
    let phr = Ppx.preprocess_phrase phr in
    let success = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    if success then loop () else `Ok false
  in
  try
    let res = loop () in
    flush_all ();
    res
  with
  | End_of_file ->
      flush_all ();
      `Ok true
  | exn ->
      flush_all ();
      `Err (error_of_exn exn)

let use_string () ?(filename = "//toplevel//") ?(print_outcome = true) ~ppf_answer code =
  with_capture @@ fun () ->
  let lb = Lexing.from_string code in
  init_loc lb filename;
  try
    List.iter
      ~f:(fun phr ->
        if not (Toploop.execute_phrase print_outcome ppf_answer phr)
        then raise Exit
        else Format.pp_print_flush ppf_answer ())
      (List.map ~f:Ppx.preprocess_phrase (!Toploop.parse_use_file lb));
    flush_all ();
    `Ok true
  with
  | Exit ->
      flush_all ();
      Format.pp_print_flush ppf_answer ();
      `Ok false
  | exn ->
      flush_all ();
      `Err (error_of_exn exn)

let parse_mod_string modname sig_code impl_code =
  let open Parsetree in
  let open Ast_helper in
  (* [init_loc] mutates [Location.input_name]/[input_lexbuf]; we reset it
     per sub-parse so positions in the Parsetree are tagged with the
     right filename. Positions are captured at parse time, so the
     leftover .mli state after this function returns is benign. *)
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
  Ptop_def [ Str.module_ (Mb.mk (Location.mknoloc (Some modname)) m) ]

let use_mod_string () ?(print_outcome = true) ~ppf_answer ~modname ?sig_code impl_code =
  with_capture @@ fun () ->
  try
    if not (String.equal (String.capitalize_ascii modname) modname)
    then
      invalid_arg
        "Wrapped.use_mod_string: the module name must start with a capital letter.";
    let phr =
      Ppx.preprocess_phrase @@ parse_mod_string modname sig_code impl_code
    in
    let res = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    flush_all ();
    `Ok res
  with exn ->
    flush_all ();
    `Err (error_of_exn exn)

(* Extracted from the "execute" function in "ocaml/toplevel/toploop.ml" *)
let check_phrase env = function
  | Parsetree.Ptop_def sstr ->
      Typecore.reset_delayed_checks ();
      let str, sg, sg_names, _, newenv = Typemod.type_toplevel_phrase env sstr in
      let sg' = Typemod.Signature_names.simplify newenv sg_names sg in
      ignore (Includemod.signatures ~mark:true env sg sg');
      Typecore.force_delayed_checks ();
      let _lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      newenv
  | Parsetree.Ptop_dir _ -> env

let check () ?(setenv = false) code =
  with_capture @@ fun () ->
  let lb = Lexing.from_string code in
  init_loc lb "//toplevel//";
  try
    let env =
      List.fold_left
        ~f:check_phrase
        ~init:!Toploop.toplevel_env
        (List.map ~f:Ppx.preprocess_phrase (!Toploop.parse_use_file lb))
    in
    if setenv then Toploop.toplevel_env := env;
    `Ok ()
  with
  | End_of_file -> `Ok ()
  | exn -> `Err (error_of_exn exn)
