(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Ocsigen team
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

(* Extract documentation examples so the build can check them.

   In the [.mli] doc-comments and the [.mld] manual pages, an [{@ocaml[ ... ]}]
   code block is *type-checked by default*. Two metadata markers opt out:

   - [{@ocaml skip[ ... ]}]    : ignored entirely (not even parsed) — for
     pseudo-code, elisions ([... ]) and toplevel-style snippets.
   - [{@ocaml parse[ ... ]}]   : only parsed (syntax-checked), not type-checked —
     for examples that reference hypothetical/undefined identifiers or placeholder
     types but should still be valid OCaml.
   - [{@ocaml prelude[ ... ]}] : shared setup (opens, helper bindings) prepended
     at top level to the type unit so the other examples can refer to it. Put it
     in a plain [(* ... *)] comment in an [.mli] to keep it out of the rendered
     documentation (odoc renders only [(** ... *)] doc-comments).

   A plain [{[ ... ]}] block carries no metadata and is always highlight-only
   (never checked). Use it for fragments you do not want checked at all.

   Each checked block must be a sequence of OCaml structure items (what you would
   paste into a [.ml] file), so it can be wrapped verbatim in its own module. A
   bare expression should be written as [let () = ignore (...)].

   Usage:
     extract_examples [--open MODULE]... [--select type|parse] FILE...

   [--select type] (the default) emits the type-checked blocks; [--select parse]
   emits the parse-only blocks. Files may be [.mli] or [.mld]. The generated
   modules carry a [# line] directive pointing back at the original file, so an
   error is reported at the real source line. *)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s)

(* Module-name-safe identifier derived from a file's basename. *)
let ident_of_basename base =
  let b = Bytes.of_string base in
  for i = 0 to Bytes.length b - 1 do
    match Bytes.get b i with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> ()
    | _ -> Bytes.set b i '_'
  done;
  String.capitalize_ascii (Bytes.to_string b)

let opening = "{@ocaml"

let closing = "]}"

type tier =
  | Type (* type-checked (the default) *)
  | Parse (* parse-only: [parse] metadata *)
  | Skip (* ignored: [skip] metadata *)
  | Prelude (* setup prepended to the type unit: [prelude] metadata *)

let contains ~sub s =
  let ls = String.length s and lsub = String.length sub in
  let rec loop i =
    if i + lsub > ls then false else String.sub s i lsub = sub || loop (i + 1)
  in
  loop 0

(* The tier is read from the [{@ocaml <meta>[] metadata: [skip]/[parse]/[prelude]
   opt out of (respectively) all checking, typing, and being a standalone module;
   anything else (including no metadata) is type-checked. A [prelude] block holds
   shared setup (opens, helper bindings) that is prepended to the type unit so the
   other examples can refer to it; place it in a plain [(* ... *)] comment in an
   [.mli] to keep it out of the rendered documentation. *)
let tier_of_meta meta =
  if contains ~sub:"skip" meta
  then Skip
  else if contains ~sub:"prelude" meta
  then Prelude
  else if contains ~sub:"parse" meta
  then Parse
  else Type

(* Find the [{@ocaml ...[ ... ]}] code blocks in [s]. Returns (tier, line, code)
   triples where [line] is the 1-based line of the first character of [code].
   Plain [{[ ... ]}] blocks carry no [{@ocaml] marker and are never returned. *)
let blocks s =
  let len = String.length s in
  let found = ref [] in
  let line_of pos =
    let n = ref 1 in
    for i = 0 to pos - 1 do
      if s.[i] = '\n' then incr n
    done;
    !n
  in
  (* naive substring search for [opening] starting at [i] *)
  let rec at i =
    if i + String.length opening > len
    then None
    else if String.sub s i (String.length opening) = opening
    then Some i
    else at (i + 1)
  in
  let rec find_close j =
    if j + String.length closing > len
    then None
    else if String.sub s j (String.length closing) = closing
    then Some j
    else find_close (j + 1)
  in
  let rec search from =
    match at from with
    | None -> ()
    | Some i -> (
        (* metadata sits between [{@ocaml] and the [[] that opens the code *)
        match String.index_from_opt s (i + String.length opening) '[' with
        | None -> ()
        | Some lb -> (
            let meta =
              String.sub s (i + String.length opening) (lb - i - String.length opening)
            in
            let code_start = lb + 1 in
            match find_close code_start with
            | None -> ()
            | Some close ->
                let code = String.sub s code_start (close - code_start) in
                found := (tier_of_meta meta, line_of code_start, code) :: !found;
                search (close + String.length closing)))
  in
  search 0;
  List.rev !found

let () =
  let opens = ref [] in
  let files = ref [] in
  let want = ref Type in
  let rec parse = function
    | "--open" :: m :: rest ->
        opens := m :: !opens;
        parse rest
    | "--select" :: "type" :: rest ->
        want := Type;
        parse rest
    | "--select" :: "parse" :: rest ->
        want := Parse;
        parse rest
    | "--select" :: s :: _ -> failwith ("unknown --select " ^ s)
    | f :: rest ->
        files := f :: !files;
        parse rest
    | [] -> ()
  in
  parse (List.tl (Array.to_list Sys.argv));
  let opens = List.rev !opens in
  let files = List.rev !files in
  let want = !want in
  set_binary_mode_out stdout true;
  let emit_code line file code =
    Printf.printf "# %d %S\n" line file;
    print_string code;
    (* ensure the block ends on its own line *)
    if String.length code > 0 && code.[String.length code - 1] <> '\n'
    then print_char '\n'
  in
  let by_file = List.map (fun file -> file, blocks (read_file file)) files in
  print_string
    "(* GENERATED by manual/examples-check/extract_examples.ml -- do not edit. *)\n\
     (* Documentation examples ({@ocaml[ ]} blocks) from the .mli doc-comments and\n\
    \   .mld manual pages. *)\n\
     [@@@warning \"-a\"]\n";
  (* Top-level opens, once: they cover the prelude and every example module
     (nested modules see the enclosing opens), and putting them here rather than
     inside each module avoids [open] preceding a bare-expression block. *)
  List.iter (fun m -> Printf.printf "open %s\n" m) opens;
  (* Prelude blocks come first, at top level, so the examples can refer to them.
     Only meaningful for the type unit. *)
  if want = Type
  then
    List.iter
      (fun (file, bs) ->
        List.iter (fun (t, line, code) -> if t = Prelude then emit_code line file code) bs)
      by_file;
  (* Then the blocks of the wanted tier, each isolated in its own module. *)
  List.iter
    (fun (file, bs) ->
      let id = ident_of_basename (Filename.remove_extension (Filename.basename file)) in
      bs
      |> List.filter_map (fun (t, line, code) ->
          if t = want then Some (line, code) else None)
      |> List.iteri (fun n (line, code) ->
          Printf.printf "\nmodule Example_%s_%d = struct\n" id (n + 1);
          emit_code line file code;
          print_string "end\n"))
    by_file;
  (* Keep the file a non-empty, valid module even when it has no blocks. *)
  print_string "\nlet () = ()\n"
