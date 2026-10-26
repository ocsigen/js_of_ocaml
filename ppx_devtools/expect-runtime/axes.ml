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

let backend =
  match Sys.backend_type with
  | Native | Bytecode -> "native"
  | Other "js_of_ocaml" -> "js"
  | Other "wasm_of_ocaml" -> "wasm"
  | Other s -> s

let js = String.equal backend "js"

let wasm = String.equal backend "wasm"

let native = String.equal backend "native"

let os_type = Sys.os_type

let win32 = String.equal os_type "Win32"

let unix = String.equal os_type "Unix"

let cygwin = String.equal os_type "Cygwin"

let ocaml_version =
  (* Parse the leading [maj.min.patch] of [Sys.ocaml_version], stopping at the
     first [+], [-] or [~] suffix. *)
  let v = Sys.ocaml_version in
  let stop =
    let n = String.length v in
    let rec loop i =
      if i >= n
      then n
      else
        match v.[i] with
        | '+' | '-' | '~' -> i
        | _ -> loop (i + 1)
    in
    loop 0
  in
  String.sub v 0 stop |> String.split_on_char '.' |> List.map int_of_string

(* On OCaml >= 4.14 [open! Sys] shadows the local [ocaml_release] with the real
   one, whose [extra] uses [Sys.extra_prefix]; the local constructors are then
   unused (warning 37). They are still needed as a fallback on 4.13. *)
type extra_prefix =
  | Plus
  | Tilde
[@@warning "-37"]

type release_info = { extra : (extra_prefix * string) option }

let oxcaml =
  (* [Sys.ocaml_release] is only available since OCaml 4.14. The local
     [ocaml_release] record provides the [extra] field label and a [None]
     fallback; on 4.14+ [open! Sys] shadows it with the real value. *)
  let ocaml_release = { extra = None } in
  ignore ocaml_release.extra;
  match
    let open! Sys in
    ocaml_release.extra
  with
  | Some (Plus, "ox") -> true
  | _ -> false
