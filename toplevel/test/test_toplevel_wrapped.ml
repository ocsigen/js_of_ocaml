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
    if i + nl > hl
    then false
    else if String.sub haystack i nl = needle
    then true
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
  report
    "ok"
    ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let x = 1 + 2;;")

(* An unused-variable warning is captured on the [Success] result rather
   than printed; we check the count and location, not the wording. *)
let () =
  report
    "warning"
    ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let f () = let y = 1 in ();;")

(* A type error is returned as [Error] with its source location. *)
let () =
  report
    "type-error"
    ~expect:"Unbound value"
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let z = nope + 1;;")

(* A syntax error is returned as [Error]; "Syntax error" is stable. *)
let () =
  report
    "syntax-error"
    ~expect:"Syntax error"
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let = ;;")

(* [check] type-checks without evaluating and returns unit on success. *)
let () = report "check-ok" ~expect:"" (Wrapped.check () "let g x = x + 1;;")

(* [use_mod_string] wraps the code in a named module. *)
let () =
  report
    "use-mod"
    ~expect:""
    (Wrapped.use_mod_string () ~print_outcome:false ~ppf_answer ~modname:"M" "let v = 42")

(* [check ~setenv:true] keeps the resulting type environment as a scratch env
   whose definitions have no runtime backing. *)
let () =
  report "check-setenv" ~expect:"" (Wrapped.check () ~setenv:true "let scratch_only = 1;;")

(* While that scratch env is active, running code is rejected. *)
let () =
  report
    "execute-blocked"
    ~expect:"scratch typing environment"
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let w = 1;;")

(* [clear_check] drops the scratch definitions and restores the previous,
   in-sync environment, re-enabling execution. *)
let () = Wrapped.clear_check ()

let () =
  report
    "execute-after-clear"
    ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let w = 1;;")

(* The scratch-only definition is gone after [clear_check]... *)
let () =
  report
    "scratch-dropped"
    ~expect:"Unbound value"
    (Wrapped.check () "let _ = scratch_only;;")

(* ...while a value executed before the scratch check is still in scope. *)
let () =
  report
    "executed-kept"
    ~expect:""
    (Wrapped.execute () ~print_outcome:false ~ppf_answer "let _ = x + 1;;")

(* [step] drives a multi-phrase buffer one phrase at a time and, unlike
   [execute], lets the caller continue past a failing phrase. *)
let () =
  Printf.printf "=== step ===\n";
  let lb = Wrapped.make_lexbuf "let s1 = 1;; let s2 = nope;; let s3 = s1 + 3;;" in
  let rec loop () =
    match Wrapped.step () ~ppf_answer lb with
    | Wrapped.Success (`Phrase ok, _) ->
        Printf.printf "phrase ok=%b\n" ok;
        loop ()
    | Wrapped.Error (err, _) ->
        Printf.printf
          "error contains %S: %b\n"
          "Unbound value"
          (contains ~needle:"Unbound value" err.Wrapped.msg);
        loop ()
    | Wrapped.Success (`Eof, _) -> Printf.printf "eof\n"
  in
  loop ()

(* Diagnostics must not gain a leading blank line across evaluations: the
   toplevel resets [Location]'s per-phrase line counter and so must Wrapped,
   otherwise re-evaluating prepends a separator before the message. *)
let () =
  let warn_msg () =
    match Wrapped.execute () ~print_outcome:false ~ppf_answer "let _ = function Some _ -> ()" with
    | Wrapped.Success (_, w :: _) -> w.Wrapped.msg
    | _ -> ""
  in
  ignore (warn_msg ());
  let msg = warn_msg () in
  Printf.printf "=== re-eval warning ===\n";
  Printf.printf "leading blank: %b\n" (String.length msg > 0 && msg.[0] = '\n');
  (* The outcome printer reads the same counter: a phrase that *warns* must not
     get its answer prefixed with a blank line (rendering the warning bumps the
     counter before the outcome is printed). *)
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  let outcome () =
    Buffer.clear buf;
    ignore
      (Wrapped.execute () ~print_outcome:true ~ppf_answer:ppf "let head = function x :: _ -> x"
        : bool Wrapped.result);
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  ignore (outcome ());
  let out = outcome () in
  Printf.printf "outcome leading blank: %b\n" (String.length out > 0 && out.[0] = '\n')
