(* Exercise the [Wrapped] toplevel API, which returns errors and warnings
   as first-class values instead of printing them to stderr. The output is
   deliberately structural (result tag, warning/error counts, source
   positions) so the snapshot stays stable across OCaml versions; only
   message substrings that are stable across 4.13-5.5 are checked. *)

open Js_of_ocaml_toplevel

let () = Direct.initialize ()

let () =
  (* Rely only on the embedded cmis, like the other toplevel tests. *)
  Load_path.reset ();
  Topdirs.dir_directory "/static/cmis"

let pp_loc { Wrapped.loc_start = l1, c1; loc_end = l2, c2 } =
  Printf.printf "  loc: (%d,%d)-(%d,%d)\n" l1 c1 l2 c2

(* A small dependency-free substring test, so the compiled test only needs
   the stdlib cmis. *)
let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec at i =
    if i + nl > hl then false
    else if String.sub haystack i nl = needle then true
    else at (i + 1)
  in
  nl = 0 || at 0

(* Print a stable view of an error/warning: its source locations and
   whether the human-readable message contains a given substring. The
   raw [msg] wording shifts between compiler versions, so we never print
   it verbatim. *)
let pp_error label expect ({ Wrapped.msg; locs } : Wrapped.error) =
  Printf.printf "  %s contains %S: %b\n" label expect (contains ~needle:expect msg);
  List.iter pp_loc locs

let report name (r : _ Wrapped.result) ~expect =
  Printf.printf "=== %s ===\n" name;
  match r with
  | Wrapped.Success (_, warnings) ->
      Printf.printf "Success, %d warning(s)\n" (List.length warnings);
      List.iter (pp_error "warning" expect) warnings
  | Wrapped.Error (err, warnings) ->
      Printf.printf "Error, %d warning(s)\n" (List.length warnings);
      pp_error "error" expect err

let buf = Buffer.create 64
let ppf_answer = Format.formatter_of_buffer buf

(* A successful phrase produces no warnings. *)
let () =
  report "ok" ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let x = 1 + 2;;")

(* An unused-variable warning is captured on the [Success] result rather
   than printed; we check the count and location, not the wording. *)
let () =
  report "warning" ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer
       "let f () = let y = 1 in ();;")

(* A type error is returned as [Error] with its source location. *)
let () =
  report "type-error" ~expect:"Unbound value"
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let z = nope + 1;;")

(* A syntax error is returned as [Error]; "Syntax error" is stable. *)
let () =
  report "syntax-error" ~expect:"Syntax error"
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let = ;;")

(* [check] type-checks without evaluating and returns unit on success. *)
let () =
  report "check-ok" ~expect:"" (Wrapped.check () "let g x = x + 1;;")

(* [use_mod_string] wraps the code in a named module. *)
let () =
  report "use-mod" ~expect:""
    (Wrapped.use_mod_string () ~print_outcome:false ~ppf_answer ~modname:"M"
       "let v = 42")
