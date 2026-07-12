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

(* The engine a generated program is run under, from the dune profile. This is
   the right axis for a native test that compiles and runs a program under
   another engine (e.g. tests-compiler invoking quickjs). *)
let target_engine =
  match Sys.getenv_opt "JSOO_TEST_ENGINE" with
  | Some e when not (String.equal e "") -> e
  | _ -> (
      (* [JSOO_ENGINE] selects the engine that runs the generated code (node,
         bun, ...); reflect it here so engine-specific test guards apply. *)
      match Sys.getenv_opt "JSOO_ENGINE" with
      | Some e when not (String.equal e "") -> e
      | _ -> "node")

(* The engine the current test process itself runs on. A native test process
   does not run under a JS/wasm engine, so it ignores [JSOO_TEST_ENGINE]; only a
   js/wasm process is actually hosted by [target_engine]. The [node]/[quickjs]/
   [wasi] shorthands resolve to this axis. *)
let host_engine =
  match Sys.backend_type with
  | Native | Bytecode -> "native"
  | Other _ -> target_engine

let os_type = Sys.os_type

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

let rec version_compare v v' =
  match v, v' with
  | [ x ], [ y ] -> compare (x : int) y
  | [], [] -> 0
  | [], y :: _ -> compare 0 y
  | x :: _, [] -> compare x 0
  | x :: xs, y :: ys -> (
      match compare (x : int) y with
      | 0 -> version_compare xs ys
      | n -> n)

let int_size_64 = Sys.int_size >= 63

let tag_dropped = function
  | "disabled" -> true
  | "no-js" -> String.equal backend "js"
  | "js-only" -> not (String.equal backend "js")
  | "no-wasm" -> String.equal backend "wasm"
  | "wasm-only" -> not (String.equal backend "wasm")
  | "native-only" -> not (String.equal backend "native")
  | "no-quickjs" -> String.equal host_engine "quickjs"
  | "quickjs-only" -> not (String.equal host_engine "quickjs")
  | "no-wasi" -> String.equal host_engine "wasi"
  | "wasi-only" -> not (String.equal host_engine "wasi")
  | "64-bits-only" -> not int_size_64
  | "32-bits-only" -> int_size_64
  | _ -> false
