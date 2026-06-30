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
module Wrapped_intf = Js_of_ocaml_toplevel_protocol.Wrapped_intf

(* The result types are defined in the dependency-free [msg] library (so the
   host driver can use them without the toplevel runtime); re-export them here. *)
type loc = Wrapped_intf.loc =
  { loc_start : int * int
  ; loc_end : int * int
  }

type error = Wrapped_intf.error =
  { msg : string
  ; locs : loc list
  }

type warning = error

type 'a result = 'a Wrapped_intf.result =
  | Success of 'a * warning list
  | Error of error * warning list

let convert_loc loc =
  let _file1, line1, col1 = Location.get_pos_info loc.Location.loc_start in
  let _file2, line2, col2 = Location.get_pos_info loc.Location.loc_end in
  { loc_start = line1, col1; loc_end = line2, col2 }

(* The [Location] report printer ends its output with a newline; drop it so a
   rendered [msg] is a bare message and consumers control the final formatting
   (otherwise a consumer adding its own newline produces a blank line). *)
let drop_trailing_newline s =
  let n = ref (String.length s) in
  while !n > 0 && Char.equal s.[!n - 1] '\n' do
    decr n
  done;
  String.sub s ~pos:0 ~len:!n

(* Run [f] with a capturing [Location.warning_reporter] and Marshal-safe
   [Clflags.error_size] installed; restore both on exit. [f] returns
   [`Ok v] or [`Err e]; [with_capture] pairs that with the captured
   warning list to produce a [result]. All mutable state is local to
   this call — the module exposes no global warning ref. *)
let with_capture (f : unit -> [ `Ok of 'a | `Err of error ]) : 'a result =
  (* Reset [Location]'s per-phrase line counter, as the stock toplevel loop
     does. Otherwise it accumulates across evaluations and the report printer
     prepends a blank-line separator before diagnostics on the second run. *)
  Location.reset ();
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
           let msg = drop_trailing_newline (Buffer.contents buf) in
           let loc = convert_loc loc in
           warnings := { msg; locs = [ loc ] } :: !warnings;
           (* Rendering the warning into our private buffer bumped [Location]'s
              line counter; undo that so the outcome printed afterwards (in the
              same phrase) is not prefixed with a separating blank line. *)
           Location.reset ();
           None
       | None -> None);
  (* [Includemod] attaches expanded signature context to module-type
     mismatch error records. That context can hold closures (e.g. from
     [Location.report_printer]), which are not marshallable — so when
     [Js_of_ocaml_toplevel_worker_lwt_client] ships an error across a Worker
     boundary via [Json.output]
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
  let msg = drop_trailing_newline (Buffer.contents buf) in
  { msg; locs }

let error_of_exn exn =
  match Location.error_of_exn exn with
  | Some (`Ok error) -> report_error error
  | Some `Already_displayed | None ->
      let msg = Printexc.to_string exn in
      { msg; locs = [] }

(** Enforcing the [check ~setenv:true] / code-execution separation *)

(* [check ~setenv:true] installs definitions into the type environment but
   discards the translated lambda, so those bindings have no runtime
   counterpart: the type environment is left out of sync with the runtime,
   and running code afterwards could read uninitialized globals. While such
   a "scratch" environment is active we therefore refuse to run code (see
   [execute], [use], [use_mod_string], which return [Error]);
   [clear_check] restores the environment snapshot taken just before the
   first [check ~setenv:true], dropping the type-only definitions while
   keeping everything that was actually executed. [check ~setenv:true]
   itself ran no code, so there is nothing on the runtime side to undo.

   [None] means no scratch env is active; [Some env] both marks it active
   and holds the in-sync environment to restore on [clear_check]. *)
let synced_env = ref None

let scratch_env_error =
  { msg =
      "cannot run code while a scratch typing environment from [check ~setenv:true] is \
       active; clear it with [clear_check] first."
  ; locs = []
  }

let clear_check () =
  (match !synced_env with
  | Some env -> Toploop.toplevel_env := env
  | None -> ());
  synced_env := None

(* Run [f] unless a scratch typing environment is active, in which case
   reject with [scratch_env_error] instead of executing code. *)
let guard_run f =
  match !synced_env with
  | Some _ -> Error (scratch_env_error, [])
  | None -> f ()

(** Execution helpers *)

let init_loc lb filename =
  Location.init lb filename;
  (* [Location.init] points [input_lexbuf] at [lb], which makes the error
     printer inline a source excerpt. [Wrapped] reports locations via [locs]
     instead, so clear it to keep rendered messages excerpt-free (consumers
     show the source themselves) and avoid pinning the lexbuf. *)
  Location.input_lexbuf := None

let execute () ?ppf_code ?(print_outcome = true) ~ppf_answer code =
  guard_run
  @@ fun () ->
  with_capture
  @@ fun () ->
  let lb = Lexing.from_function (Refill.lexbuf code (ref 0) ppf_code) in
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

let make_lexbuf ?ppf_code code =
  let lb = Lexing.from_function (Refill.lexbuf code (ref 0) ppf_code) in
  init_loc lb "//toplevel//";
  lb

let step () ?(print_outcome = false) ~ppf_answer lb =
  guard_run
  @@ fun () ->
  with_capture
  @@ fun () ->
  try
    let phr = !Toploop.parse_toplevel_phrase lb in
    let phr = Ppx.preprocess_phrase phr in
    let success = Toploop.execute_phrase print_outcome ppf_answer phr in
    Format.pp_print_flush ppf_answer ();
    flush_all ();
    `Ok (`Phrase success)
  with
  | End_of_file ->
      flush_all ();
      `Ok `Eof
  | (Syntaxerr.Error _ | Lexer.Error _) as exn ->
      (* A syntax/lex error leaves the lexbuf positioned mid-phrase. Flush the
         buffered input so that a caller which keeps stepping makes progress (the
         refill feeds one line at a time) instead of re-parsing the same broken
         phrase forever. Type/eval errors leave the lexbuf past the [;;], so no
         flush is needed there. *)
      Lexing.flush_input lb;
      flush_all ();
      `Err (error_of_exn exn)
  | exn ->
      flush_all ();
      `Err (error_of_exn exn)

let use () ?(filename = "//toplevel//") ?(print_outcome = false) ~ppf_answer code =
  guard_run
  @@ fun () ->
  with_capture
  @@ fun () ->
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
  guard_run
  @@ fun () ->
  with_capture
  @@ fun () ->
  try
    if not (String.equal (String.capitalize_ascii modname) modname)
    then
      invalid_arg
        "Wrapped.use_mod_string: the module name must start with a capital letter.";
    let phr = Ppx.preprocess_phrase @@ parse_mod_string modname sig_code impl_code in
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
  with_capture
  @@ fun () ->
  let lb = Lexing.from_string code in
  init_loc lb "//toplevel//";
  (* Type inference mutates the global type graph in place: unification links
     type variables (including the weak variables that live in the toplevel
     environment, e.g. the ['_weak] in [let r = ref []]). Snapshot it so a
     speculative or failing [check] leaves no residue; [Toploop.execute_phrase]
     does the same for [execute]. *)
  let snap = Btype.snapshot () in
  try
    let env =
      List.fold_left
        ~f:check_phrase
        ~init:!Toploop.toplevel_env
        (List.map ~f:Ppx.preprocess_phrase (!Toploop.parse_use_file lb))
    in
    if setenv
    then (
      (* Committing the scratch env: keep the inferred mutations. Snapshot the
         in-sync environment on the first [setenv] of a run, so [clear_check] can
         roll back to it later; consecutive [check ~setenv] keep building on the
         scratch env and must not overwrite it. *)
      (match !synced_env with
      | Some _ -> ()
      | None -> synced_env := Some !Toploop.toplevel_env);
      Toploop.toplevel_env := env)
    else
      (* A non-committing check: undo the mutations so the real environment's
         (weak) type variables are not silently constrained. *)
      Btype.backtrack snap;
    `Ok ()
  with
  | End_of_file ->
      Btype.backtrack snap;
      `Ok ()
  | exn ->
      (* Drop the partial mutations of the failed check. *)
      Btype.backtrack snap;
      `Err (error_of_exn exn)
