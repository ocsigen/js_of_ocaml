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

(* Completeness guard for [manual/api.mld]: every public module of every
   documented library must be referenced from the API reference page.

   Public modules are read from the source of truth rather than re-listed here,
   so the check fails (printing the offending names) whenever someone adds a
   public module without documenting which package provides it:

   - [--wrapper FILE]: a library's main module file ([js_of_ocaml.ml],
     [js_of_ocaml_lwt.ml], ...); its top-level [module X = ...] aliases are the
     public modules.
   - [--module NAME]: a public module of an unwrapped library (e.g. the
     [js_of_ocaml.deriving] library, whose modules are top-level).
   - [--skip NAME]: a public-but-undocumented module (e.g. a deprecated alias).

   The program prints, one per line, every public module name that does not
   appear in [api.mld]. A passing run prints nothing. *)

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

let is_ident_char c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c = '_'

(* Top-level [module Name ...] declarations of a wrapper file. *)
let wrapper_modules path =
  read_file path
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.index_opt line ' ' with
      | Some i when String.sub line 0 i = "module" ->
          let rest = String.sub line (i + 1) (String.length line - i - 1) in
          let j = ref 0 in
          while !j < String.length rest && is_ident_char rest.[!j] do
            incr j
          done;
          if !j > 0 then Some (String.sub rest 0 !j) else None
      | _ -> None)

(* Whole-word occurrence of [name] in [haystack]. *)
let mentions haystack name =
  let nl = String.length name and hl = String.length haystack in
  let rec loop i =
    if i + nl > hl
    then false
    else if
      String.sub haystack i nl = name
      && (i = 0 || not (is_ident_char haystack.[i - 1]))
      && (i + nl = hl || not (is_ident_char haystack.[i + nl]))
    then true
    else loop (i + 1)
  in
  loop 0

let () =
  let api = ref "" in
  let wrappers = ref [] in
  let modules = ref [] in
  let skip = ref [] in
  let rec parse = function
    | "--api" :: f :: tl ->
        api := f;
        parse tl
    | "--wrapper" :: f :: tl ->
        wrappers := f :: !wrappers;
        parse tl
    | "--module" :: m :: tl ->
        modules := m :: !modules;
        parse tl
    | "--skip" :: m :: tl ->
        skip := m :: !skip;
        parse tl
    | [] -> ()
    | x :: _ -> failwith ("check_api: unexpected argument " ^ x)
  in
  parse (List.tl (Array.to_list Sys.argv));
  let api_text = read_file !api in
  let all =
    List.concat_map wrapper_modules (List.rev !wrappers) @ List.rev !modules
    |> List.sort_uniq compare
  in
  List.iter
    (fun m -> if not (List.mem m !skip || mentions api_text m) then print_endline m)
    all
