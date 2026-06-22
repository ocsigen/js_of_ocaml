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
  { lib : string
  ; filename : string
  ; description : string
  ; line_number : int
  ; start_pos : int
  ; end_pos : int
  ; tags : string list
  ; condition : unit -> bool (* reified [@when] on the test; gates running *)
  ; expectations : variant array array
  ; body : unit -> unit
  }

let tests : test list ref = ref []

let register_test
    ~lib
    ~filename
    ~description
    ~line_number
    ~start_pos
    ~end_pos
    ~tags
    ~condition
    ~expectations
    body =
  tests :=
    { lib
    ; filename
    ; description
    ; line_number
    ; start_pos
    ; end_pos
    ; tags
    ; condition
    ; expectations
    ; body
    }
    :: !tests

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
  ; list_partitions_into_file : string option
  ; partition : string option
  ; only_test : (string * int option) list (* filename suffix, optional line *)
  ; matching : string list (* name substrings *)
  ; libname : string (* run only tests belonging to this library *)
  ; source_tree_root : string
  ; diff_cmd : string option
  ; verbose : bool
  }

(* [s] contains [substring] (used by [-matching]). *)
let is_substring ~substring s =
  let m = String.length substring and n = String.length s in
  if m = 0
  then true
  else
    let rec loop i =
      if i + m > n
      then false
      else if String.equal (String.sub s i m) substring
      then true
      else loop (i + 1)
    in
    loop 0

(* Parse a [-only-test] location: [file.ml], [file.ml:line], or the
   [File "file.ml", line N, ...] form printed by the compiler. *)
let parse_only_spec (s : string) : string * int option =
  let digits_at str start =
    let n = String.length str in
    let j = ref start in
    while !j < n && str.[!j] >= '0' && str.[!j] <= '9' do
      incr j
    done;
    if !j > start then int_of_string_opt (String.sub str start (!j - start)) else None
  in
  let prefix = "File \"" in
  let plen = String.length prefix in
  if String.length s >= plen && String.equal (String.sub s 0 plen) prefix
  then
    let rest = String.sub s plen (String.length s - plen) in
    match String.index_opt rest '"' with
    | None -> s, None
    | Some q ->
        let file = String.sub rest 0 q in
        let after = String.sub rest q (String.length rest - q) in
        let line =
          let m = "line " in
          let n = String.length after and ml = String.length m in
          let rec loop i =
            if i + ml > n
            then None
            else if String.equal (String.sub after i ml) m
            then digits_at after (i + ml)
            else loop (i + 1)
          in
          loop 0
        in
        file, line
  else
    match String.index_opt s ':' with
    | None -> s, None
    | Some k ->
        let file = String.sub s 0 k in
        let after = String.sub s (k + 1) (String.length s - k - 1) in
        file, digits_at after 0

let parse_argv () =
  let argv = Sys.argv in
  let n = Array.length argv in
  let libname = ref "" in
  let drop = ref [] in
  let require = ref [] in
  let list_partitions = ref false in
  let list_partitions_into_file = ref None in
  let partition = ref None in
  let only_test = ref [] in
  let matching = ref [] in
  let source_tree_root = ref "." in
  let diff_cmd = ref None in
  let verbose = ref false in
  let i = ref 1 in
  while !i < n && not (String.equal argv.(!i) "inline-test-runner") do
    incr i
  done;
  if !i < n then incr i;
  (* The positional library-name argument: only tests belonging to it run. *)
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
    | "-verbose" -> verbose := true
    | "-source-tree-root" ->
        incr i;
        if !i < n then source_tree_root := argv.(!i)
    | "-diff-cmd" ->
        incr i;
        if !i < n then diff_cmd := Some argv.(!i)
    | "-list-partitions-into-file" ->
        incr i;
        if !i < n then list_partitions_into_file := Some argv.(!i)
    | "-partition" ->
        incr i;
        if !i < n then partition := Some argv.(!i)
    | "-matching" ->
        incr i;
        if !i < n then matching := argv.(!i) :: !matching
    | "-only-test" ->
        incr i;
        if !i < n then only_test := parse_only_spec argv.(!i) :: !only_test
    | arg ->
        (* Fail on an unrecognised argument rather than skipping it: an unknown
           flag may carry behaviour we are silently dropping. *)
        Printf.eprintf "ppx_expect_light: unknown argument %S\n%!" arg;
        Stdlib.exit 1);
    incr i
  done;
  { drop = !drop
  ; require = !require
  ; list_partitions = !list_partitions
  ; list_partitions_into_file = !list_partitions_into_file
  ; partition = !partition
  ; only_test = !only_test
  ; matching = !matching
  ; libname = !libname
  ; source_tree_root = !source_tree_root
  ; diff_cmd = !diff_cmd
  ; verbose = !verbose
  }

let tags_skipped cfg tags =
  List.exists (fun t -> Axes.tag_dropped t || List.mem t cfg.drop) tags
  || List.exists (fun req -> not (List.mem req tags)) cfg.require

(* [-only-test] location match, mirroring ppx_inline_test: the test's filename
   must end with the requested suffix on a path boundary, and the line (if
   given) must match. *)
let only_test_match cfg (test : test) =
  match cfg.only_test with
  | [] -> true
  | specs ->
      List.exists
        (fun (file, line_opt) ->
          let dn = test.filename in
          let ps = String.length dn - String.length file in
          ps >= 0
          && String.equal (String.sub dn ps (String.length file)) file
          && (ps = 0 || Char.equal dn.[ps - 1] '/')
          &&
          match line_opt with
          | None -> true
          | Some line -> line = test.line_number)
        specs

let matching_ok cfg descr =
  match cfg.matching with
  | [] -> true
  | subs -> List.exists (fun substring -> is_substring ~substring descr) subs

(* A test's filename ([%expect]'s [pos_fname]) is its partition, so each source
   file forms one partition that dune can run in its own process. *)
let partition_selected cfg (test : test) =
  match cfg.partition with
  | None -> true
  | Some p -> String.equal p test.filename

(* A runner links its library's dependencies, so it sees tests from several
   libraries; only run the ones belonging to the requested library (the
   positional argument). An empty request runs every library. *)
let lib_selected cfg (test : test) =
  String.equal cfg.libname "" || String.equal cfg.libname test.lib

let exit () =
  Printexc.record_backtrace true;
  let cfg = parse_argv () in
  let list_partitions oc =
    (* Distinct filenames of this library's tests, in source order. *)
    let seen = Hashtbl.create 16 in
    List.iter
      (fun (test : test) ->
        if lib_selected cfg test && not (Hashtbl.mem seen test.filename)
        then begin
          Hashtbl.add seen test.filename ();
          output_string oc test.filename;
          output_char oc '\n'
        end)
      (List.rev !tests)
  in
  if cfg.list_partitions
  then begin
    list_partitions stdout;
    flush stdout;
    Stdlib.exit 0
  end;
  (match cfg.list_partitions_into_file with
  | None -> ()
  | Some file ->
      let oc = open_out file in
      list_partitions oc;
      close_out oc;
      Stdlib.exit 0);
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
  (* Like ppx_inline_test, hold failure reports until the end in verbose mode so
     they don't interleave with the per-test progress lines on stdout. *)
  let delayed = ref [] in
  let report s =
    if cfg.verbose then delayed := s :: !delayed else output_string stderr s
  in
  (* [-verbose] line, kept close to ppx_inline_test's:
     [File "f.ml", line N, characters s-e: name (0.000 sec)]. *)
  let verbose_descr (test : test) =
    Printf.sprintf
      "File %S, line %d, characters %d-%d%s"
      test.filename
      test.line_number
      test.start_pos
      test.end_pos
      (if String.equal test.description "" then "" else ": " ^ test.description)
  in
  List.iter
    (fun (test : test) ->
      let descr =
        Printf.sprintf "%s:%d %s" test.filename test.line_number test.description
      in
      if
        (not (lib_selected cfg test))
        || (not (partition_selected cfg test))
        || (not (only_test_match cfg test))
        || not (matching_ok cfg descr)
      then () (* not part of this run (other library/partition, or not requested) *)
      else if tags_skipped cfg test.tags || not (test.condition ())
      then (
        if
          (* Skipped by [@tags] or a false [@when]. Unlike ppx_inline_test,
             surface these in verbose mode. *)
          cfg.verbose
        then Printf.printf "%s (skipped)\n%!" (verbose_descr test))
      else begin
        incr ran;
        if cfg.verbose then print_string (verbose_descr test);
        let cap = Capture.start () in
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
        let t0 = Sys.time () in
        let exn =
          match test.body () with
          | () -> None
          | exception e -> Some (e, Printexc.get_backtrace ())
        in
        let elapsed = Sys.time () -. t0 in
        current := None;
        Capture.stop cap;
        if cfg.verbose then Printf.printf " (%.3f sec)\n%!" elapsed;
        (match r.corrections with
        | [] -> ()
        | corrections ->
            incr mismatches;
            all_corrections := List.rev_append corrections !all_corrections;
            if not dune_mode then List.iter report (List.rev r.reports));
        match exn with
        | None -> ()
        | Some (e, bt) ->
            incr errors;
            report
              (Printf.sprintf
                 "FAILED: %s\n  exception: %s\n%s\n"
                 descr
                 (Printexc.to_string e)
                 bt)
      end)
    (List.rev !tests);
  (match List.rev !delayed with
  | [] -> ()
  | reports ->
      Printf.eprintf "\n%s\n" (String.make 70 '=');
      List.iter (fun s -> output_string stderr s) reports);
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
