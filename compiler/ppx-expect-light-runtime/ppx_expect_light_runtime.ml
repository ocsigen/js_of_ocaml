(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

module Axes = Axes

type variant =
  { condition : unit -> bool
  ; is_default : bool
  ; expected : string
  ; payload_start : int
  ; payload_end : int
  }

type test =
  { filename : string
  ; description : string
  ; line_number : int
  ; tags : string list
  ; expectations : variant array array
  ; body : unit -> unit
  }

let tests : test list ref = ref []

let register_test ~filename ~description ~line_number ~tags ~expectations body =
  tests := { filename; description; line_number; tags; expectations; body } :: !tests

(* -- Per-test run state --------------------------------------------------- *)

type run =
  { cap : Capture.t
  ; expectations : variant array array
  ; filename : string
  ; description : string
  ; mutable corrections : Corrected.correction list
  ; mutable reports : string list
  }

let current : run option ref = ref None

(* Pick the active variant of a group: the first conditional variant whose
   predicate holds, otherwise the default one (a plain [%expect]). *)
let select_variant group =
  let n = Array.length group in
  let rec find i =
    if i >= n
    then None
    else
      let v = group.(i) in
      if (not v.is_default) && v.condition () then Some v else find (i + 1)
  in
  match find 0 with
  | Some v -> v
  | None ->
      let rec find_default i =
        if i >= n
        then group.(0)
        else if group.(i).is_default
        then group.(i)
        else find_default (i + 1)
      in
      find_default 0

let check ~index =
  match !current with
  | None -> ()
  | Some r ->
      let actual = Capture.read r.cap in
      if index >= 0 && index < Array.length r.expectations
      then begin
        let group = r.expectations.(index) in
        if Array.length group > 0
        then begin
          let v = select_variant group in
          if not (String.equal (Text.normalize actual) (Text.normalize v.expected))
          then begin
            r.corrections <-
              { Corrected.filename = r.filename
              ; payload_start = v.payload_start
              ; payload_end = v.payload_end
              ; actual
              }
              :: r.corrections;
            r.reports <-
              Diff.make ~description:r.description ~expected:v.expected ~actual
              :: r.reports
          end
        end
      end

let read_output () =
  match !current with
  | None -> ""
  | Some r -> Capture.read r.cap

(* -- Runner CLI ----------------------------------------------------------- *)

type config =
  { drop : string list
  ; require : string list
  ; list_partitions : bool
  ; libname : string
  ; source_tree_root : string
  ; diff_cmd : string option
  }

let parse_argv () =
  let argv = Sys.argv in
  let n = Array.length argv in
  let libname = ref "" in
  let drop = ref [] in
  let require = ref [] in
  let list_partitions = ref false in
  let source_tree_root = ref "." in
  let diff_cmd = ref None in
  let i = ref 1 in
  while !i < n && not (String.equal argv.(!i) "inline-test-runner") do
    incr i
  done;
  if !i < n then incr i;
  (if !i < n
   then
     let a = argv.(!i) in
     if String.length a = 0 || not (Char.equal a.[0] '-')
     then (
       libname := a;
       incr i));
  while !i < n do
    (match argv.(!i) with
    | "-drop-tag" ->
        incr i;
        if !i < n then drop := argv.(!i) :: !drop
    | "-require-tag" ->
        incr i;
        if !i < n then require := argv.(!i) :: !require
    | "-list-partitions" -> list_partitions := true
    | "-source-tree-root" ->
        incr i;
        if !i < n then source_tree_root := argv.(!i)
    | "-diff-cmd" ->
        incr i;
        if !i < n then diff_cmd := Some argv.(!i)
    | "-partition" | "-list-partitions-into-file" | "-matching" | "-only-test" ->
        incr i (* skip the flag's value *)
    | _ -> () (* ignore unknown flags *));
    incr i
  done;
  { drop = !drop
  ; require = !require
  ; list_partitions = !list_partitions
  ; libname = !libname
  ; source_tree_root = !source_tree_root
  ; diff_cmd = !diff_cmd
  }

let test_skipped cfg tags =
  List.exists (fun t -> Axes.tag_dropped t || List.mem t cfg.drop) tags
  || List.exists (fun req -> not (List.mem req tags)) cfg.require

let exit () =
  Printexc.record_backtrace true;
  let cfg = parse_argv () in
  if cfg.list_partitions
  then begin
    print_string (if String.equal cfg.libname "" then "all" else cfg.libname);
    print_newline ();
    Stdlib.exit 0
  end;
  (* In [-diff-cmd -] mode (always passed by dune) the runner just writes the
     [.corrected] files and lets dune diff them against the sources and drive
     promotion: an expect mismatch is therefore NOT a runner failure. Run
     standalone (no [-diff-cmd -]), the runner prints the diffs itself and fails
     on any mismatch. Runtime exceptions always fail the runner. *)
  let dune_mode =
    match cfg.diff_cmd with
    | Some "-" -> true
    | _ -> false
  in
  let all_corrections = ref [] in
  let mismatches = ref 0 in
  let errors = ref 0 in
  let ran = ref 0 in
  List.iter
    (fun test ->
      if test_skipped cfg test.tags
      then ()
      else begin
        incr ran;
        let cap = Capture.start () in
        let descr =
          Printf.sprintf "%s:%d %s" test.filename test.line_number test.description
        in
        let r =
          { cap
          ; expectations = test.expectations
          ; filename = test.filename
          ; description = descr
          ; corrections = []
          ; reports = []
          }
        in
        current := Some r;
        let exn =
          match test.body () with
          | () -> None
          | exception e -> Some (e, Printexc.get_backtrace ())
        in
        current := None;
        Capture.stop cap;
        (match r.corrections with
        | [] -> ()
        | corrections ->
            incr mismatches;
            all_corrections := List.rev_append corrections !all_corrections;
            if not dune_mode
            then List.iter (fun s -> output_string stderr s) (List.rev r.reports));
        match exn with
        | None -> ()
        | Some (e, bt) ->
            incr errors;
            Printf.eprintf
              "FAILED: %s\n  exception: %s\n%s\n"
              descr
              (Printexc.to_string e)
              bt
      end)
    (List.rev !tests);
  Corrected.write ~root:cfg.source_tree_root !all_corrections;
  flush stderr;
  let failed = if dune_mode then !errors else !mismatches + !errors in
  if failed > 0
  then begin
    Printf.eprintf "ppx_expect_light: FAILED %d / %d tests\n%!" failed !ran;
    Stdlib.exit 2
  end;
  (* Force process termination instead of returning, like ppx_inline_test's
     runner. Under js_of_ocaml/wasm_of_ocaml this stops a dangling async
     operation left by a test (e.g. an un-awaited [fetch]) from keeping node
     alive and crashing on an unhandled rejection after the tests pass. *)
  Stdlib.exit 0
